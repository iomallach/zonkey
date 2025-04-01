const std = @import("std");
const ast = @import("ast.zig");

const TypeEnv = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(std.StringHashMap(ast.Type)),

    pub fn init(allocator: std.mem.Allocator) !TypeEnv {
        const scopes = std.ArrayList(std.StringHashMap(ast.Type)).init(allocator);
        var type_env = TypeEnv{
            .allocator = allocator,
            .scopes = scopes,
        };
        try type_env.enterScope();
        return type_env;
    }

    pub fn enterScope(self: *TypeEnv) !void {
        const new_scope = std.StringHashMap(ast.Type).init(self.allocator);
        try self.scopes.append(new_scope);
    }

    pub fn exitScope(self: *TypeEnv) void {
        var cur_scope = self.scopes.pop().?;
        cur_scope.deinit();
    }

    pub fn define(self: *TypeEnv, name: []const u8, typ: ast.Type) !void {
        var cur_scope = &self.scopes.items[self.scopes.items.len - 1];
        try cur_scope.putNoClobber(name, typ);
    }

    pub fn lookup(self: *TypeEnv, key: []const u8) ?ast.Type {
        var nth_scope = self.scopes.items.len;

        while (nth_scope >= 0) {
            nth_scope -= 1;
            if (self.scopes.items[nth_scope].get(key)) |typ| {
                return typ;
            }
        }

        return null;
    }
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    type_env: TypeEnv,
    errors_list: std.ArrayList([]const u8),
    current_function: ?*ast.FunctionLiteral,

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        return TypeChecker{
            .allocator = allocator,
            .type_env = try TypeEnv.init(allocator),
            .errors_list = std.ArrayList([]const u8).init(allocator),
            .current_function = null,
        };
    }

    pub fn inferAndCheck(self: *TypeChecker, program: *ast.AstNode) !?ast.Type {
        switch (program.*) {
            .Program => |*prog| {
                for (prog.program.items) |*p| {
                    _ = try self.inferAndCheck(p);
                }
            },
            .LetStatement => |*ls| {
                const exp_type = (try self.inferAndCheck(ls.value)).?;
                if (ls.name.Identifier.inferred_type) |it| {
                    if (it != exp_type) {
                        try self.errors_list.append("BAD ERROR");
                        return null;
                    }
                } else {
                    ls.inferred_type = exp_type;
                }

                try self.type_env.define(ls.name.Identifier.value, ls.inferred_type.?);
            },
            .ReturnStatement => |*r| {
                const ret_type = (try self.inferAndCheck(r.return_value)).?;
                if (self.current_function.?.return_type != ret_type) {
                    try self.errors_list.append("BAD ERROR");
                }
            },
            .ExpressionStatement => |*es| {
                _ = try self.inferAndCheck(es.expression);
            },
            .BlockStatement => |*bs| {
                try self.type_env.enterScope();
                for (bs.statements.items) |*stmt| {
                    _ = try self.inferAndCheck(stmt);
                }
                self.type_env.exitScope();
            },
            .WhileLoopStatement => |wls| {
                // TODO: implement later
                _ = wls;
                unreachable;
            },
            .FunctionLiteral => |*fl| {
                try self.type_env.enterScope();
                self.current_function = fl;
                // define parameters
                for (fl.parameters.items) |*p| {
                    try self.type_env.define(p.FunctionParameter.ident.Identifier.value, p.FunctionParameter.inferred_type);
                }
                // check statements
                for (fl.body.BlockStatement.statements.items) |*stmt| {
                    _ = try self.inferAndCheck(stmt);
                }
                self.current_function = null;
                self.type_env.exitScope();
            },
            .ArrayLiteral => unreachable, //TODO: implement later
            .FunctionCall => |*fc| {
                if (fc.function.FunctionLiteral.parameters.items.len != fc.arguments.items.len) {
                    try self.errors_list.append("Argument number mismatch");
                }
                for (fc.function.FunctionLiteral.parameters.items, 0..) |*p, i| {
                    const param_type = p.FunctionParameter.inferred_type;
                    const passed_type = (try self.inferAndCheck(&fc.arguments.items[i])).?;
                    if (param_type != passed_type) {
                        try self.errors_list.append("Function parameter type mismatch");
                    }
                }
                return fc.function.FunctionLiteral.return_type;
            },
            .If => |iff| {
                const cond_type = (try self.inferAndCheck(iff.condition)).?;
                if (cond_type != .Bool) {
                    try self.errors_list.append("Expected boolean expression");
                }
                //TODO: we allow if expressions, but for now this doesn't infer cons/alt types
                _ = try self.inferAndCheck(iff.consequence);
                if (iff.alternative) |alt| {
                    _ = try self.inferAndCheck(alt);
                }
            },
            .Prefix => |pref| {
                const exp_type = (try self.inferAndCheck(pref.right)).?;
                if (pref.operator == .Minus and (exp_type == .Float or exp_type == .Integer)) {
                    return exp_type;
                } else if (pref.operator == .Negation and exp_type == .Bool) {
                    return exp_type;
                }
                try self.errors_list.append("Invalid prefix expression type");
                return null;
            },
            .Infix => |infx| {
                const left_type = (try self.inferAndCheck(infx.left)).?;
                const right_type = (try self.inferAndCheck(infx.right)).?;
                // string and void are immediately unsupported in infix at the moment
                if (left_type == .String or left_type == .Void or right_type == .String or right_type == .Void) {
                    try self.errors_list.append("Invalid infix expression");
                    return null;
                }
                switch (infx.operator) {
                    .Plus, .Minus, .Multiply, .Divide => {
                        if (left_type == .Bool or right_type == .Bool) {
                            try self.errors_list.append("Invalid infix expression");
                            return null;
                        }
                        // TODO: this is not ideal, but keeping for simplicity at the moment
                        return ast.Type.Float;
                    },
                    //TODO: should probably split equal/notequal and others due to booleans, but ok for now
                    .EqualEqual, .NotEqual, .Greater, .Less, .GreaterEqual, .LessEqual => {
                        return ast.Type.Bool;
                    },
                }
            },
            .Index => unreachable, //TODO: implement
            .Identifier => |ident| {
                const ident_type = self.type_env.lookup(ident.value) orelse {
                    try self.errors_list.append("Unbound identifier");
                    return null;
                };
                return ident_type;
            },
            .IntegerLiteral => {
                return ast.Type.Integer;
            },
            .FloatLiteral => {
                return ast.Type.Float;
            },
            .BooleanLiteral => {
                return ast.Type.Bool;
            },
            .StringLiteral => {
                return ast.Type.String;
            },
            else => unreachable,
        }
        return null;
    }
};

pub fn test_parse_errors(errors: *std.ArrayList([]const u8)) !void {
    if (errors.*.items.len > 0) {
        std.debug.print("Errors:\n", .{});
        for (errors.*.items, 1..) |e, i| {
            std.debug.print("  {d}: {s}\n", .{ i, e });
        }
        return error.ParserHadErrors;
    }
}

fn parse_program(input: []const u8, allocator: std.mem.Allocator) !ast.AstNode {
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;
    const tok = @import("token.zig");

    var lexer = Lexer.init(input, allocator);

    var lexed_tokens = std.ArrayList(tok.Token).init(allocator);
    while (true) {
        const cur_token = try lexer.next_token();
        try lexed_tokens.append(cur_token);
        if (cur_token.token_type == tok.TokenType.EOF) {
            break;
        }
    }

    const slice_tokens = try lexed_tokens.toOwnedSlice();
    var parser = try Parser.init(slice_tokens, allocator);
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        test_parse_errors(parser.errors()) catch {};
        return err;
    };

    return program;
}

test "Infer simple literal expressions" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "4" },
        .{ .expected = ast.Type.Float, .input = "5.4" },
        .{ .expected = ast.Type.String, .input = "\"test\"" },
        .{ .expected = ast.Type.Bool, .input = "true" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        const program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        const typ = (try checker.inferAndCheck(program.Program.program.items[0].ExpressionStatement.expression)).?;

        try std.testing.expectEqual(test_case.expected, typ);
    }
}
