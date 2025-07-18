//TODO: add declaration points to get even better error messages
//TODO: "did you mean" idea: traverse names and find the closest one
const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");
const diag = @import("diagnostics.zig");

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
    types: std.AutoHashMap(*const ast.AstNode, ast.Type), // types of expression, e.g. infix, prefix, calls
    defs: std.AutoHashMap(*const ast.AstNode, Symbol), // mapping from definition to symbol
    uses: std.AutoHashMap(*const ast.AstNode, Symbol), // mapping from reference to symbol, symbol taken from scopes

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!TypeEnvironment {
        const current_scope = try allocator.create(Scope);
        current_scope.* = Scope.init(null, allocator);

        return TypeEnvironment{
            .allocator = allocator,
            .current_scope = current_scope,
            .types = std.AutoHashMap(*const ast.AstNode, ast.Type).init(allocator),
            .defs = std.AutoHashMap(*const ast.AstNode, Symbol).init(allocator),
            .uses = std.AutoHashMap(*const ast.AstNode, Symbol).init(allocator),
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
    diagnostics: *diag.Diagnostics,
    current_function: ?*const ast.FunctionLiteral,

    pub fn init(diagnostics: *diag.Diagnostics, allocator: std.mem.Allocator) error{OutOfMemory}!TypeChecker {
        return TypeChecker{
            .allocator = allocator,
            .type_env = try TypeEnvironment.init(allocator),
            .diagnostics = diagnostics,
            .current_function = null,
        };
    }

    pub fn inferAndCheck(self: *TypeChecker, program: *const ast.AstNode) error{ OutOfMemory, TypeViolation, UnboundIdentifier }!ast.Type {
        switch (program.*) {
            .Program => |prog| {
                for (prog.program.items) |*p| {
                    _ = self.inferAndCheck(p) catch |err| switch (err) {
                        error.UnboundIdentifier => continue,
                        error.TypeViolation => continue,
                        error.OutOfMemory => return err,
                        else => unreachable,
                    };
                }
            },
            .LetStatement => |ls| {
                const exp_type = try self.inferAndCheck(ls.expression);
                try self.type_env.types.putNoClobber(ls.expression, exp_type);

                if (ls.inferred_type) |it| {
                    if (!std.meta.eql(exp_type, it)) {
                        try self.diagnostics.reportError(
                            diag.let_statement_error_fmt,
                            .{
                                try self.diagnostics.getErrorMarker(),
                                ls.inferred_type.?,
                                ls.inferred_type.?,
                                exp_type,
                                ls.expression.getToken().span.line_number,
                                ls.expression.getToken().span.start,
                                ls.expression.getToken().span.source_chunk,
                                try self.diagnostics.getErrorPointerSpaces(ls.expression.getToken()),
                            },
                        );
                        return error.TypeViolation;
                    }
                }

                const symbol = try self.type_env.defineSymbol(ls.name.Identifier.value, exp_type);
                //TODO: consider moving away from putNoClobber, as it panics and there is no room to handle already defined gracefully with a message
                try self.type_env.defs.putNoClobber(ls.name, symbol);
            },
            .ReturnStatement => |r| {
                const ret_type = try self.inferAndCheck(r.return_value);
                if (!std.meta.eql(self.current_function.?.return_type, ret_type)) {
                    try self.diagnostics.reportError(
                        diag.return_statement_error_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            self.current_function.?.return_type,
                            ret_type,
                            r.return_value.getToken().span.line_number,
                            r.return_value.getToken().span.start,
                            r.return_value.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(r.return_value.getToken()),
                        },
                    );
                    return error.TypeViolation;
                }
                try self.type_env.types.putNoClobber(r.return_value, ret_type);
            },
            .ExpressionStatement => |es| {
                const exp_type = try self.inferAndCheck(es.expression);
                try self.type_env.types.putNoClobber(es.expression, exp_type);
                return exp_type;
            },
            .BlockStatement => |bs| {
                try self.type_env.enterScope();
                var last_stmt_type: ?ast.Type = null;

                for (bs.statements.items) |*stmt| {
                    last_stmt_type = try self.inferAndCheck(stmt);
                }
                self.type_env.exitScope();

                return last_stmt_type orelse ast.Type.Unit;
            },
            .WhileLoopStatement => |wls| {
                const cond_type = try self.inferAndCheck(wls.condition);
                try self.type_env.types.putNoClobber(wls.condition, cond_type);
                if (cond_type != .Bool) {
                    try self.diagnostics.reportError(
                        diag.cond_expression_error_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            cond_type,
                            wls.condition.getToken().span.line_number,
                            wls.condition.getToken().span.start,
                            wls.condition.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(wls.condition.getToken()),
                        },
                    );
                    return error.TypeViolation;
                }
                _ = try self.inferAndCheck(wls.statements);
            },
            .AssignmentStatement => |as| {
                const ident_type = try self.inferAndCheck(as.name);
                const expr_type = try self.inferAndCheck(as.expression);
                if (!std.meta.eql(ident_type, expr_type)) {
                    try self.diagnostics.reportError(
                        diag.assignment_statement_error_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            as.name.Identifier.value,
                            ident_type,
                            expr_type,
                            as.expression.getToken().span.line_number,
                            as.expression.getToken().span.start,
                            as.expression.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(as.expression.getToken()),
                        },
                    );
                    return error.TypeViolation;
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
            .FunctionCall => |fc| {
                var encountered_error = false;
                const func = try self.inferAndCheck(fc.function);
                //FIXME: broken for less arguments (panic)
                if (func.Function.arg_types.items.len != fc.arguments.items.len) {
                    try self.diagnostics.reportError(
                        diag.function_call_error_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            fc.function.Identifier.value,
                            func.Function.arg_types.items.len,
                            fc.arguments.items.len,
                            fc.arguments.items[func.Function.arg_types.items.len].getToken().span.line_number,
                            fc.arguments.items[func.Function.arg_types.items.len].getToken().span.start,
                            fc.token.span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(
                                fc.arguments.items[func.Function.arg_types.items.len].getToken(),
                            ),
                        },
                    );
                    return error.TypeViolation;
                }
                for (func.Function.arg_types.items, 0..) |p, i| {
                    const passed_type = try self.inferAndCheck(&fc.arguments.items[i]);
                    try self.type_env.types.putNoClobber(&fc.arguments.items[i], passed_type);

                    if (!std.meta.eql(p, passed_type)) {
                        try self.diagnostics.reportError(
                            diag.function_call_param_error_fmt,
                            .{
                                try self.diagnostics.getErrorMarker(),
                                i + 1,
                                fc.function.Identifier.value,
                                p,
                                passed_type,
                                fc.arguments.items[i].getToken().span.line_number,
                                fc.arguments.items[i].getToken().span.start,
                                fc.arguments.items[i].getToken().span.source_chunk,
                                try self.diagnostics.getErrorPointerSpaces(fc.arguments.items[i].getToken()),
                            },
                        );
                        // not returning to record more error and print a better error message
                        encountered_error = true;
                    }
                }
                if (encountered_error) {
                    return error.TypeViolation;
                }

                const symbol = try self.type_env.resolveSymbol(fc.function.Identifier.value);
                try self.type_env.uses.putNoClobber(program, symbol);

                return func.Function.return_type;
            },
            .BuiltInCall => |bic| {
                const expr_type = try self.inferAndCheck(bic.argument);
                try self.type_env.types.putNoClobber(bic.argument, expr_type);

                return ast.Type.Unit;
            },
            .If => |iff| {
                const cond_type = try self.inferAndCheck(iff.condition);
                try self.type_env.types.putNoClobber(iff.condition, cond_type);
                if (cond_type != .Bool) {
                    try self.diagnostics.reportError(
                        diag.cond_expression_error_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            cond_type,
                            iff.condition.getToken().span.line_number,
                            iff.condition.getToken().span.start,
                            iff.condition.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(iff.condition.getToken()),
                        },
                    );
                    return error.TypeViolation;
                }

                const cons_type = try self.inferAndCheck(iff.consequence);
                if (iff.alternative) |alt| {
                    const alt_type = try self.inferAndCheck(alt);
                    if (!std.meta.eql(cons_type, alt_type)) {
                        try self.diagnostics.reportError(
                            diag.if_expression_type_error_fmt,
                            .{
                                try self.diagnostics.getErrorMarker(),
                                cons_type,
                                alt_type,
                                iff.getToken().span.line_number,
                                iff.getToken().span.start,
                                iff.getToken().span.source_chunk,
                                try self.diagnostics.getErrorPointerSpaces(iff.getToken()),
                            },
                        );
                        return error.TypeViolation;
                    }
                }
                return cons_type;
            },
            .Prefix => |pref| {
                const exp_type = try self.inferAndCheck(pref.right);
                switch (pref.operator) {
                    .Negation => switch (exp_type) {
                        .Bool => return exp_type,
                        else => {
                            try self.diagnostics.reportError(
                                diag.prefix_expression_type_error_fmt,
                                .{
                                    try self.diagnostics.getErrorMarker(),
                                    "bool",
                                    exp_type,
                                    pref.right.getToken().span.line_number,
                                    pref.right.getToken().span.start,
                                    pref.getToken().span.source_chunk,
                                    try self.diagnostics.getErrorPointerSpaces(pref.right.getToken()),
                                },
                            );
                            return error.TypeViolation;
                        },
                    },
                    .Minus => switch (exp_type) {
                        .Float, .Integer => return exp_type,
                        else => {
                            try self.diagnostics.reportError(
                                diag.prefix_expression_type_error_fmt,
                                .{
                                    try self.diagnostics.getErrorMarker(),
                                    "float",
                                    exp_type,
                                    pref.right.getToken().span.line_number,
                                    pref.right.getToken().span.start,
                                    pref.getToken().span.source_chunk,
                                    try self.diagnostics.getErrorPointerSpaces(pref.right.getToken()),
                                },
                            );
                            return error.TypeViolation;
                        },
                    },
                }
            },
            .Infix => |infx| {
                const left_type = try self.inferAndCheck(infx.left);
                const right_type = try self.inferAndCheck(infx.right);

                try self.type_env.types.putNoClobber(infx.left, left_type);
                try self.type_env.types.putNoClobber(infx.right, right_type);

                //infix on functions doesn't make sense
                if (left_type == .Function or right_type == .Function) {
                    try self.diagnostics.reportError(
                        diag.infix_expression_type_eror_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            left_type,
                            right_type,
                            infx.getToken().span.line_number,
                            infx.getToken().span.start,
                            infx.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(infx.getToken()),
                        },
                    );
                    return error.TypeViolation;
                }
                // string and void are immediately unsupported in infix at the moment
                if (left_type == .String or left_type == .Unit or right_type == .String or right_type == .Unit) {
                    try self.diagnostics.reportError(
                        diag.infix_expression_type_eror_fmt,
                        .{
                            try self.diagnostics.getErrorMarker(),
                            left_type,
                            right_type,
                            infx.getToken().span.line_number,
                            infx.getToken().span.start,
                            infx.getToken().span.source_chunk,
                            try self.diagnostics.getErrorPointerSpaces(infx.getToken()),
                        },
                    );
                    return error.TypeViolation;
                }

                switch (infx.operator) {
                    .Plus, .Minus, .Multiply, .Divide => {
                        if (left_type == .Bool or right_type == .Bool) {
                            try self.diagnostics.reportError(
                                diag.infix_expression_type_eror_fmt,
                                .{
                                    try self.diagnostics.getErrorMarker(),
                                    left_type,
                                    right_type,
                                    infx.getToken().span.line_number,
                                    infx.getToken().span.start,
                                    infx.getToken().span.source_chunk,
                                    try self.diagnostics.getErrorPointerSpaces(infx.getToken()),
                                },
                            );
                            return error.TypeViolation;
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
                            try self.diagnostics.reportError(
                                diag.infix_expression_type_eror_fmt,
                                .{
                                    try self.diagnostics.getErrorMarker(),
                                    left_type,
                                    right_type,
                                    infx.getToken().span.line_number,
                                    infx.getToken().span.start,
                                    infx.getToken().span.source_chunk,
                                    try self.diagnostics.getErrorPointerSpaces(infx.getToken()),
                                },
                            );
                            return error.TypeViolation;
                        }
                        return ast.Type.Bool;
                    },
                }
            },
            .Index => unreachable, //TODO: implement
            .Identifier => |ident| {
                const symbol = self.type_env.resolveSymbol(ident.value) catch |err| {
                    if (err == error.UnboundIdentifier) {
                        try self.diagnostics.reportError(
                            diag.unbound_identifier_error_fmt,
                            .{
                                try self.diagnostics.getErrorMarker(),
                                ident.value,
                                ident.getToken().span.line_number,
                                ident.getToken().span.start,
                                ident.getToken().span.source_chunk,
                                try self.diagnostics.getErrorPointerSpaces(ident.getToken()),
                            },
                        );
                    }
                    return err;
                };
                try self.type_env.uses.putNoClobber(program, try self.type_env.resolveSymbol(ident.value));
                return symbol.typ;
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
        return ast.Type.Unit;
    }
};

fn parse_program(input: []const u8, diagnostics: *diag.Diagnostics, allocator: std.mem.Allocator) !ast.AstNode {
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    var lexer = Lexer.init(input, diagnostics, allocator);

    var lexed_tokens = std.ArrayList(tok.Token).init(allocator);
    while (true) {
        const cur_token = try lexer.next_token();
        try lexed_tokens.append(cur_token);
        if (cur_token.token_type == tok.TokenType.EOF) {
            break;
        }
    }

    const slice_tokens = try lexed_tokens.toOwnedSlice();
    var parser = try Parser.init(slice_tokens, diagnostics, allocator);
    const program = try parser.parse();

    return program;
}

test "Infer simple expressions" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "4;" },
        .{ .expected = ast.Type.Float, .input = "5.4;" },
        .{ .expected = ast.Type.String, .input = "\"test\";" },
        .{ .expected = ast.Type.Bool, .input = "true;" },
        .{ .expected = ast.Type.Bool, .input = "!true;" },
        .{ .expected = ast.Type.Bool, .input = "!false;" },
        .{ .expected = ast.Type.Integer, .input = "-5;" },
        .{ .expected = ast.Type.Float, .input = "-7.3;" },
        .{ .expected = ast.Type.Integer, .input = "5 - 3;" },
        .{ .expected = ast.Type.Float, .input = "5.0 - 3;" },
        .{ .expected = ast.Type.Integer, .input = "5 + 3;" },
        .{ .expected = ast.Type.Integer, .input = "5 / 3;" },
        .{ .expected = ast.Type.Integer, .input = "5 * 3;" },
        .{ .expected = ast.Type.Float, .input = "5.1 * 3.1;" },
        .{ .expected = ast.Type.Float, .input = "5 * 3.1;" },
        .{ .expected = ast.Type.Bool, .input = "5 == 3;" },
        .{ .expected = ast.Type.Bool, .input = "5 != 3;" },
        .{ .expected = ast.Type.Bool, .input = "5 > 3;" },
        .{ .expected = ast.Type.Bool, .input = "5 < 3;" },
        .{ .expected = ast.Type.Bool, .input = "5 >= 3;" },
        .{ .expected = ast.Type.Bool, .input = "5 <= 3;" },
        .{ .expected = ast.Type.Bool, .input = "true == false;" },
        .{ .expected = ast.Type.Bool, .input = "true == true;" },
        .{ .expected = ast.Type.Bool, .input = "false == false;" },
        .{ .expected = ast.Type.Bool, .input = "false == true;" },
        .{ .expected = ast.Type.Bool, .input = "true != false;" },
        .{ .expected = ast.Type.Bool, .input = "true != true;" },
        .{ .expected = ast.Type.Bool, .input = "false != false;" },
        .{ .expected = ast.Type.Bool, .input = "false != true;" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

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
        .{ .expected = ast.Type.String, .input = "let a = \"test\";" },
        .{ .expected = ast.Type.String, .input = "let a: string = \"test\";" },
        .{ .expected = ast.Type.Integer, .input = "let a = 4 + 3;" },
        .{ .expected = ast.Type.Float, .input = "let a = 4.0 + 3.0;" },
        .{ .expected = ast.Type.Float, .input = "let a = 4 / 3.0;" },
        .{ .expected = ast.Type.Bool, .input = "let a = !true;" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

        try std.testing.expectEqual(test_case.expected, (try checker.type_env.resolveSymbol("a")).typ);
    }
}

test "Infer nested expressions" {
    const TestCase = struct {
        expected: ast.Type,
        input: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expected = ast.Type.Integer, .input = "-1 + 2;" },
        .{ .expected = ast.Type.Integer, .input = "2 + 3 - 1 * 2;" },
        .{ .expected = ast.Type.Integer, .input = "-(4 + 5) / 3;" },
        .{ .expected = ast.Type.Float, .input = "-(4 + 5) / 3.0;" },
        .{ .expected = ast.Type.Bool, .input = "!(2 < 3);" },
        .{ .expected = ast.Type.Bool, .input = "!(2 < 3.0);" },
        .{ .expected = ast.Type.Bool, .input = "!(!true == false);" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

        const expression = program.Program.program.items[0].ExpressionStatement.expression;
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
            .expected_expr_type = ast.Type.Unit,
            .input = "if true == false {}",
            .statement_num = 0,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Unit,
            .input = "if !(3 < 5) { let a = 3; } else {}",
            .statement_num = 0,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Unit,
            .input = "let a = (5 * 2) < 10; if a {}",
            .statement_num = 1,
        },
        .{
            .expected_cond_type = ast.Type.Bool,
            .expected_expr_type = ast.Type.Float,
            .input = "let a = (5 * 2) < 10.0; if a { 3.0 } else { 4.0 + 1.0 }",
            .statement_num = 1,
        },
        //TODO: seems types of block statements are not recorded, might be something to look into
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

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
            .input = "fn myfunc(x: int) -> int { return x; }",
        },
        .{
            .expected_ret_type = ast.Type.Integer,
            .expected_param_types = &[_]ast.Type{ ast.Type.Integer, ast.Type.Integer },
            .input = "fn myfunc(x: int, y: int) -> int { return x * y; }",
        },
        .{
            .expected_ret_type = ast.Type.Float,
            .expected_param_types = &[_]ast.Type{ ast.Type.Float, ast.Type.Float },
            .input = "fn myfunc(x: float, y: float) -> float { return x * y; }",
        },
        .{
            .expected_ret_type = ast.Type.Float,
            .expected_param_types = &[_]ast.Type{ ast.Type.Integer, ast.Type.Float },
            .input = "fn myfunc(x: int, y: float) -> float { return x * y; }",
        },
        .{
            .expected_ret_type = ast.Type.Bool,
            .expected_param_types = &[_]ast.Type{},
            .input = "fn myfunc() -> bool { return !false; }",
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

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
        .{ .expected = ast.Type.Integer, .input = "fn myfunc(x: int) -> int { return x; }; myfunc(3);" },
        // FIXME: \n causes integer overflow panic in lexer
        .{ .expected = ast.Type.Float, .input = "fn myfunc(x: float, y: float) -> float { return x * y; }; myfunc(1.0, 3.3);" },
        .{ .expected = ast.Type.Float, .input = "fn myfunc(x: int, y: float) -> float { return x * y; }; myfunc(1, 3.3);" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

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
            .input = "fn myfunc(x: int) -> int { return 1 + x; }; let foo = 3 + myfunc(2);",
        },
        .{
            .expected = ast.Type.Float,
            .input = "fn myfunc(x: int) -> int { return 1 + x; }; let foo = 3.0 + myfunc(2);",
        },
        .{
            .expected = ast.Type.Bool,
            .input = "fn myfunc(z: bool) -> bool { return !z; }; let foo = (3 < 5) != myfunc(1 > 2);",
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

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
            .expected = ast.Type.Float,
            .input = "let a = 1.0; a = a + 1.0;",
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
        var diagnostics = diag.Diagnostics.init(allocator);
        var program = try parse_program(test_case.input, &diagnostics, allocator);
        var checker = try TypeChecker.init(&diagnostics, allocator);
        _ = try checker.inferAndCheck(&program);

        try diagnostics.showAndFail();

        try std.testing.expectEqual(test_case.expected, (try checker.type_env.resolveSymbol("a")).typ);
        const assign_ident = program.Program.program.items[1].AssignmentStatement.name;
        try std.testing.expectEqual(test_case.expected, checker.type_env.uses.get(assign_ident).?.typ);
    }
}

test "Infer builtin print call" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "print(3 + 4);";

    var diagnostics = diag.Diagnostics.init(allocator);
    var program = try parse_program(input, &diagnostics, allocator);
    var checker = try TypeChecker.init(&diagnostics, allocator);
    _ = try checker.inferAndCheck(&program);

    try diagnostics.showAndFail();

    try std.testing.expectEqual(
        ast.Type.Integer,
        checker.type_env.types.get(program.Program.program.items[0].BuiltInCall.argument).?,
    );
}

test "Infer while loop statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "while 1 < 2 { 1 + 1; }";

    var diagnostics = diag.Diagnostics.init(allocator);
    var program = try parse_program(input, &diagnostics, allocator);
    var checker = try TypeChecker.init(&diagnostics, allocator);
    _ = try checker.inferAndCheck(&program);

    try diagnostics.showAndFail();

    const while_loop = program.Program.program.items[0].WhileLoopStatement;

    try std.testing.expectEqual(ast.Type.Bool, checker.type_env.types.get(while_loop.condition));
}
