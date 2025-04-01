const std = @import("std");
const ast = @import("ast.zig");

const TypeEnv = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(std.StringHashMap(ast.Type)),

    pub fn init(allocator: std.mem.Allocator) !TypeEnv {
        const scopes = std.ArrayList(std.StringHashMap(ast.Type)).init(allocator);
        const type_env = TypeEnv{
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
        const cur_scope = self.scopes.pop().?;
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

    pub fn inferAndCheck(self: *TypeChecker, program: *ast.AstNode) ?ast.Type {
        switch (program.*) {
            .Program => |prog| {
                for (prog) |p| {
                    _ = self.inferAndCheck(p);
                }
            },
            .LetStatement => |ls| {
                const exp_type = self.inferAndCheck(ls.value).?;
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
            .ReturnStatement => |r| {
                const ret_type = self.inferAndCheck(r.return_value).?;
                if (self.current_function.?.return_type != ret_type) {
                    try self.errors_list.append("BAD ERROR");
                }
            },
            .ExpressionStatement => |es| {
                _ = self.inferAndCheck(es.expression);
            },
            .BlockStatement => |bs| {
                try self.type_env.enterScope();
                for (bs.statements.items) |stmt| {
                    _ = self.inferAndCheck(stmt);
                }
                try self.type_env.exitScope();
            },
            .WhileLoopStatement => |wls| {
                // TODO: implement later
                _ = wls;
                unreachable;
            },
            .FunctionLiteral => |fl| {
                try self.type_env.enterScope();
                self.current_function = &fl;
                // define parameters
                for (fl.parameters.items) |p| {
                    try self.type_env.define(p.FunctionParameter.ident, p.FunctionParameter.inferred_type);
                }
                // check statements
                for (fl.body.BlockStatement.statements.items) |stmt| {
                    _ = self.inferAndCheck(stmt);
                }
                self.current_function = null;
                self.type_env.exitScope();
            },
            .ArrayLiteral => unreachable, //TODO: implement later
            .FunctionCall => |fc| {
                if (fc.function.FunctionLiteral.parameters.items.len != fc.arguments.items.len) {
                    try self.errors_list.append("Argument number mismatch");
                }
                for (fc.function.FunctionLiteral.parameters.items, 0..) |p, i| {
                    const param_type = p.FunctionParameter.inferred_type;
                    const passed_type = self.inferAndCheck(fc.arguments.items[i]).?;
                    if (param_type != passed_type) {
                        try self.errors_list.append("Function parameter type mismatch");
                    }
                }
                return fc.function.FunctionLiteral.return_type;
            },
            .If => |iff| {
                const cond_type = self.inferAndCheck(iff.condition).?;
                if (cond_type != .Bool) {
                    try self.errors_list.append("Expected boolean expression");
                }
                //TODO: we allow if expressions, but for now this doesn't infer cons/alt types
                _ = self.inferAndCheck(iff.consequence);
                if (iff.alternative) |alt| {
                    _ = self.inferAndCheck(alt);
                }
            },
            .Prefix => |pref| {
                const exp_type = self.inferAndCheck(pref.right).?;
                if (pref.operator == .Minus and (exp_type == .Float or exp_type == .Integer)) {
                    return exp_type;
                } else if (pref.operator == .Negation and exp_type == .Bool) {
                    return exp_type;
                }
                try self.errors_list.append("Invalid prefix expression type");
                return null;
            },
            .Infix => |infx| {
                const left_type = self.inferAndCheck(infx.left).?;
                const right_type = self.inferAndCheck(infx.right).?;
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
