const std = @import("std");
const ast = @import("ast.zig");

pub const Symbol = struct {
    name: []const u8,
    typ: ast.Type,
};

const Scope = struct {
    parent: ?*Scope,
    names: std.StringHashMap(Symbol),

    pub fn init(parent: ?*Scope, allocator: std.mem.Allocator) Scope {
        return Scope{
            .parent = parent,
            .names = std.StringHashMap(Symbol).init(allocator),
        };
    }

    pub fn define(self: *Scope, name: []const u8, typ: ast.Type) error{OutOfMemory}!Symbol {
        try self.names.putNoClobber(name, Symbol{ .name = name, .typ = typ });
        return self.names.get(name).?;
    }

    pub fn resolve(self: *Scope, name: []const u8) error{UnboundIdentifier}!Symbol {
        if (self.names.get(name)) |typ| {
            return typ;
        }
        if (self.parent) |p| {
            return p.resolve(name);
        }
        return error.UnboundIdentifier;
    }
};

pub const TypeEnvironment = struct {
    allocator: std.mem.Allocator,
    current_scope: *Scope,
    types: std.AutoHashMap(*ast.AstNode, ast.Type), // types of expression, e.g. infix, prefix, calls
    defs: std.AutoHashMap(*ast.AstNode, Symbol), // mapping from definition to symbol
    uses: std.AutoHashMap(*ast.AstNode, Symbol), // mapping from reference to symbol, symbol taken from scopes

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!TypeEnvironment {
        const current_scope = try allocator.create(Scope);
        current_scope.* = Scope.init(null, allocator);

        return TypeEnvironment{
            .allocator = allocator,
            .current_scope = current_scope,
            .types = std.AutoHashMap(*ast.AstNode, ast.Type).init(allocator),
            .defs = std.AutoHashMap(*ast.AstNode, Symbol).init(allocator),
            .uses = std.AutoHashMap(*ast.AstNode, Symbol).init(allocator),
        };
    }

    pub fn enterScope(self: *TypeEnvironment) error{OutOfMemory}!void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.current_scope, self.allocator);
        self.current_scope = new_scope;
    }

    pub fn exitScope(self: *TypeEnvironment) void {
        self.current_scope = self.current_scope.parent.?;
    }

    pub fn defineSymbol(self: *TypeEnvironment, name: []const u8, typ: ast.Type) error{OutOfMemory}!Symbol {
        return self.current_scope.define(name, typ);
    }

    pub fn resolveSymbol(self: *TypeEnvironment, name: []const u8) error{UnboundIdentifier}!Symbol {
        return self.current_scope.resolve(name);
    }
};

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    type_env: TypeEnvironment,
    errors_list: std.ArrayList([]const u8),
    current_function: ?*ast.FunctionLiteral,

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!TypeChecker {
        return TypeChecker{
            .allocator = allocator,
            .type_env = try TypeEnvironment.init(allocator),
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
                const exp_type = (try self.inferAndCheck(ls.expression)).?;
                try self.type_env.types.putNoClobber(ls.expression, exp_type);

                if (ls.inferred_type) |it| {
                    if (!std.meta.eql(exp_type, it)) {
                        try self.errors_list.append("Type of identifier does not equal the type of expression in let statement");
                        return null;
                    }
                } else {
                    ls.inferred_type = exp_type;
                }

                const symbol = try self.type_env.defineSymbol(ls.name.Identifier.value, exp_type);
                try self.type_env.defs.putNoClobber(ls.name, symbol);
            },
            .ReturnStatement => |*r| {
                const ret_type = (try self.inferAndCheck(r.return_value)).?;
                if (!std.meta.eql(self.current_function.?.return_type, ret_type)) {
                    try self.errors_list.append(try std.fmt.allocPrint(self.allocator, "The type of return statement expression {any} does not equal the return type of the current function {any}", .{
                        ret_type,
                        self.current_function.?.return_type,
                    }));
                    return null;
                }
                try self.type_env.types.putNoClobber(r.return_value, ret_type);
            },
            .ExpressionStatement => |*es| {
                const exp_type = (try self.inferAndCheck(es.expression)).?;
                try self.type_env.types.putNoClobber(es.expression, exp_type);
                return exp_type;
            },
            .BlockStatement => |*bs| {
                try self.type_env.enterScope();
                var last_stmt_type: ?ast.Type = null;

                for (bs.statements.items) |*stmt| {
                    last_stmt_type = try self.inferAndCheck(stmt);
                }
                self.type_env.exitScope();

                return last_stmt_type orelse ast.Type.Void;
            },
            .WhileLoopStatement => |wls| {
                // TODO: implement later
                _ = wls;
                unreachable;
            },
            .AssignmentStatement => |*as| {
                const ident_type = (try self.inferAndCheck(as.name)).?;
                const expr_type = (try self.inferAndCheck(as.expression)).?;
                if (!std.meta.eql(ident_type, expr_type)) {
                    return null;
                }
                try self.type_env.types.putNoClobber(as.expression, expr_type);
            },
            .FunctionLiteral => |*fl| {
                const heap_inner_ftype = try self.allocator.create(ast.FunctionType);
                heap_inner_ftype.* = try ast.FunctionType.init(&fl.parameters, fl.return_type, self.allocator);
                const func_type = ast.Type{
                    .Function = heap_inner_ftype,
                };
                const symbol = try self.type_env.defineSymbol(fl.name.?, func_type);
                try self.type_env.defs.putNoClobber(program, symbol);

                try self.type_env.enterScope();
                self.current_function = fl;
                // define parameters
                for (fl.parameters.items) |*p| {
                    const f_symbol = try self.type_env.defineSymbol(p.FunctionParameter.ident.Identifier.value, p.FunctionParameter.inferred_type);
                    try self.type_env.defs.putNoClobber(p, f_symbol);
                }
                // check statements
                for (fl.body.BlockStatement.statements.items) |*stmt| {
                    //TODO: check if the return statement is there at all
                    _ = try self.inferAndCheck(stmt);
                }
                self.current_function = null;
                self.type_env.exitScope();
                return func_type;
            },
            .ArrayLiteral => unreachable, //TODO: implement later
            .FunctionCall => |*fc| {
                const func = (try self.inferAndCheck(fc.function)).?;
                if (func.Function.arg_types.items.len != fc.arguments.items.len) {
                    try self.errors_list.append("Argument number mismatch");
                    return null;
                }
                for (func.Function.arg_types.items, 0..) |p, i| {
                    const passed_type = (try self.inferAndCheck(&fc.arguments.items[i])).?;
                    try self.type_env.types.putNoClobber(&fc.arguments.items[i], passed_type);

                    if (!std.meta.eql(p, passed_type)) {
                        try self.errors_list.append("Function parameter type mismatch");
                    }
                }
                const symbol = try self.type_env.resolveSymbol(fc.function.Identifier.value);
                try self.type_env.uses.putNoClobber(program, symbol);

                return func.Function.return_type;
            },
            .If => |iff| {
                const cond_type = (try self.inferAndCheck(iff.condition)).?;
                try self.type_env.types.putNoClobber(iff.condition, cond_type);
                if (cond_type != .Bool) {
                    try self.errors_list.append("Expected boolean expression");
                }

                const cons_type = try self.inferAndCheck(iff.consequence);
                var alt_type: ?ast.Type = null;
                if (iff.alternative) |alt| {
                    alt_type = try self.inferAndCheck(alt);
                    if (cons_type != null and alt_type != null and std.meta.eql(cons_type, alt_type)) {
                        return cons_type.?;
                    }
                    // TODO: else if one of them null, report an error
                    return ast.Type.Void;
                }

                return cons_type orelse ast.Type.Void;
            },
            .Prefix => |*pref| {
                const exp_type = (try self.inferAndCheck(pref.right)).?;
                switch (pref.operator) {
                    .Negation => switch (exp_type) {
                        .Bool => return exp_type,
                        else => {
                            try self.errors_list.append("Unsupported prefix expression");
                            return null;
                        },
                    },
                    .Minus => switch (exp_type) {
                        .Float, .Integer => return exp_type,
                        else => {
                            try self.errors_list.append("Unsupported prefix expression");
                            return null;
                        },
                    },
                }
            },
            .Infix => |infx| {
                const left_type = (try self.inferAndCheck(infx.left)).?;
                const right_type = (try self.inferAndCheck(infx.right)).?;

                //infix on functions doesn't make sense
                if (left_type == .Function or right_type == .Function) {
                    try self.errors_list.append("Invalid infix expression");
                    return null;
                }
                // string and void are immediately unsupported in infix at the moment
                if (left_type == .String or left_type == .Void or right_type == .String or right_type == .Void) {
                    try self.errors_list.append("Invalid infix expression");
                    return null;
                }
                if (!std.meta.eql(left_type, right_type)) {
                    try self.errors_list.append("Infix on different types");
                    return null;
                }

                switch (infx.operator) {
                    .Plus, .Minus, .Multiply, .Divide => {
                        if (left_type == .Bool or right_type == .Bool) {
                            try self.errors_list.append("Invalid infix expression");
                            return null;
                        }
                        if (left_type == .Float or right_type == .Float) {
                            return ast.Type.Float;
                        }

                        // at this point left == right and we're good with returning either type
                        return left_type;
                    },
                    .EqualEqual, .NotEqual => {
                        return ast.Type.Bool;
                    },
                    .Greater, .Less, .GreaterEqual, .LessEqual => {
                        if (left_type == .Bool or right_type == .Bool) {
                            try self.errors_list.append("Invalid infix expression");
                            return null;
                        }
                        return ast.Type.Bool;
                    },
                }
            },
            .Index => unreachable, //TODO: implement
            .Identifier => |ident| {
                //TODO: catch and report the error value
                try self.type_env.uses.putNoClobber(program, try self.type_env.resolveSymbol(ident.value));
                return (try self.type_env.resolveSymbol(ident.value)).typ;
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

pub fn test_errors(errors: *std.ArrayList([]const u8)) !void {
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
    const program = try parser.parse();
    try test_errors(parser.errors());

    return program;
}

test "Infer simple expressions" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "4" },
        .{ .expected = ast.Type.Float, .input = "5.4" },
        .{ .expected = ast.Type.String, .input = "\"test\"" },
        .{ .expected = ast.Type.Bool, .input = "true" },
        .{ .expected = ast.Type.Bool, .input = "!true" },
        .{ .expected = ast.Type.Bool, .input = "!false" },
        .{ .expected = ast.Type.Integer, .input = "-5" },
        .{ .expected = ast.Type.Float, .input = "-7.3" },
        .{ .expected = ast.Type.Integer, .input = "5 - 3" },
        .{ .expected = ast.Type.Integer, .input = "5 + 3" },
        .{ .expected = ast.Type.Integer, .input = "5 / 3" },
        .{ .expected = ast.Type.Integer, .input = "5 * 3" },
        .{ .expected = ast.Type.Float, .input = "5.1 * 3.1" },
        .{ .expected = ast.Type.Bool, .input = "5 == 3" },
        .{ .expected = ast.Type.Bool, .input = "5 != 3" },
        .{ .expected = ast.Type.Bool, .input = "5 > 3" },
        .{ .expected = ast.Type.Bool, .input = "5 < 3" },
        .{ .expected = ast.Type.Bool, .input = "5 >= 3" },
        .{ .expected = ast.Type.Bool, .input = "5 <= 3" },
        .{ .expected = ast.Type.Bool, .input = "true == false" },
        .{ .expected = ast.Type.Bool, .input = "true == true" },
        .{ .expected = ast.Type.Bool, .input = "false == false" },
        .{ .expected = ast.Type.Bool, .input = "false == true" },
        .{ .expected = ast.Type.Bool, .input = "true != false" },
        .{ .expected = ast.Type.Bool, .input = "true != true" },
        .{ .expected = ast.Type.Bool, .input = "false != false" },
        .{ .expected = ast.Type.Bool, .input = "false != true" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        const expression = program.Program.program.items[0].ExpressionStatement.expression;
        try std.testing.expectEqual(test_case.expected, checker.type_env.types.get(expression));
    }
}

test "Infer let statements" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    // TODO: need un-happy path tests
    const tests = [_]TestCase{
        .{ .expected = ast.Type.String, .input = "let a = \"test\"" },
        .{ .expected = ast.Type.String, .input = "let a: string = \"test\"" },
        .{ .expected = ast.Type.Integer, .input = "let a = 4 + 3" },
        .{ .expected = ast.Type.Float, .input = "let a = 4.0 + 3.0" },
        .{ .expected = ast.Type.Bool, .input = "let a = !true" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        try std.testing.expectEqual(test_case.expected, (try checker.type_env.resolveSymbol("a")).typ);
    }
}

test "Infer nested expressions" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "-1 + 2" },
        .{ .expected = ast.Type.Integer, .input = "2 + 3 - 1 * 2" },
        .{ .expected = ast.Type.Integer, .input = "-(4 + 5) / 3" },
        .{ .expected = ast.Type.Float, .input = "-(4.0 + 5.0) / 3.0" },
        .{ .expected = ast.Type.Bool, .input = "!(2 < 3)" },
        .{ .expected = ast.Type.Bool, .input = "!(!true == false)" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        const expression = program.Program.program.items[0].ExpressionStatement.expression;
        try test_errors(&checker.errors_list);
        try std.testing.expectEqual(test_case.expected, checker.type_env.types.get(expression));
    }
}

test "Infer if expressions" {
    const TestCase = struct {
        expected_cond_type: ast.Type,
        expected_expr_type: ast.Type,
        statement_num: usize,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Void,
            .input = "if true == false {}",
            .statement_num = 0,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Void,
            .input = "if !(3 < 5) { let a = 3; } else {}",
            .statement_num = 0,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Void,
            .input = "let a = (5 * 2) < 10; if a {}",
            .statement_num = 1,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Float,
            .input = "let a = (5 * 2) < 10; if a { 3.0 } else { 4.0 + 1.0 }",
            .statement_num = 1,
        },
        //TODO: seems types of block statements are not recorded, might be something to look into
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        const if_expression = program.Program.program.items[test_case.statement_num].ExpressionStatement.expression;
        try std.testing.expectEqual(test_case.expected_cond_type, checker.type_env.types.get(if_expression.If.condition));
        try std.testing.expectEqual(test_case.expected_expr_type, checker.type_env.types.get(if_expression));
    }
}

test "Infer function declaration" {
    const TestCase = struct {
        // TODO: not storing function type yet, as no function type exists
        expected_ret_type: ast.Type,
        expected_param_types: []const ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{
            .expected_ret_type = ast.Type.Integer,
            .expected_param_types = &[_]ast.Type{ast.Type.Integer},
            .input = "fn myfunc(x: int) int { return x; }",
        },
        .{
            .expected_ret_type = ast.Type.Integer,
            .expected_param_types = &[_]ast.Type{ ast.Type.Integer, ast.Type.Integer },
            .input = "fn myfunc(x: int, y: int) int { return x * y; }",
        },
        .{
            .expected_ret_type = ast.Type.Float,
            .expected_param_types = &[_]ast.Type{ ast.Type.Float, ast.Type.Float },
            .input = "fn myfunc(x: float, y: float) float { return x * y; }",
        },
        .{
            .expected_ret_type = ast.Type.Bool,
            .expected_param_types = &[_]ast.Type{},
            .input = "fn myfunc() bool { return !false; }",
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        const function_decl = &program.Program.program.items[0];
        const symbol = checker.type_env.defs.get(function_decl).?;
        try std.testing.expectEqual(test_case.expected_ret_type, symbol.typ.Function.return_type);
        for (0..test_case.expected_param_types.len) |i| {
            try std.testing.expectEqual(test_case.expected_param_types[i], symbol.typ.Function.arg_types.items[i]);
        }
    }
}

test "Infer function call" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "fn myfunc(x: int) int { return x; }; myfunc(3)" },
        // FIXME: \n causes integer overflow panic in lexer
        .{ .expected = ast.Type.Float, .input = "fn myfunc(x: float, y: float) float { return x * y; }; myfunc(1.0, 3.3)" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);

        const call_expr = program.Program.program.items[1].ExpressionStatement.expression;
        const typ = checker.type_env.types.get(call_expr).?;
        try std.testing.expectEqual(test_case.expected, typ);
    }
}

test "Infer mixed expressions" {
    const TestCase = struct {
        input: []const u8,
        expected: ast.Type,
    };
    const tests = [_]TestCase{
        .{
            .expected = ast.Type.Integer,
            .input = "fn myfunc(x: int) int { return 1 + x; }; let foo = 3 + myfunc(2);",
        },
        .{
            .expected = ast.Type.Bool,
            .input = "fn myfunc(z: bool) bool { return !z; }; let foo = (3 < 5) != myfunc(1 > 2);",
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        try std.testing.expectEqual(test_case.expected, (try checker.type_env.resolveSymbol("foo")).typ);
    }
}

test "Infer assignment statements" {
    const TestCase = struct {
        input: []const u8,
        expected: ast.Type,
    };
    const tests = [_]TestCase{
        .{
            .expected = ast.Type.Float,
            .input = "let a = 1.0; a = a + 1.0;",
        },
        .{
            .expected = ast.Type.Integer,
            .input = "let a = 1; a = a + 1;",
        },
        .{
            .expected = ast.Type.Bool,
            .input = "let a = false; a = true;",
        },
        .{
            .expected = ast.Type.String,
            .input = "let a = \"foo\"; a = \"bar\";",
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var program = try parse_program(test_case.input, allocator);
        var checker = try TypeChecker.init(allocator);
        _ = try checker.inferAndCheck(&program);

        try test_errors(&checker.errors_list);
        try std.testing.expectEqual(test_case.expected, (try checker.type_env.resolveSymbol("a")).typ);
        const assign_ident = program.Program.program.items[1].AssignmentStatement.name;
        try std.testing.expectEqual(test_case.expected, checker.type_env.uses.get(assign_ident).?.typ);
    }
}
