const c = @cImport({
    @cInclude("llvm-c/Core.h");

    @cInclude("llvm-c/ErrorHandling.h");
    @cInclude("llvm-c/DataTypes.h");
    @cInclude("llvm-c/Types.h");

    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/BitReader.h");

    @cInclude("llvm-c/ExecutionEngine.h");
});
const ast = @import("ast.zig");
const ti = @import("type_inference.zig");
const std = @import("std");

const Symbol = struct {
    name: [:0]const u8,
    type_annotation: ast.Type,
    llvm_value: c.LLVMValueRef,
    llvm_type: c.LLVMTypeRef,
    is_parameter: bool,
};

const SymbolTable = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(std.StringHashMap(Symbol)),

    pub fn init(allocator: std.mem.Allocator) !SymbolTable {
        var st = SymbolTable{
            .allocator = allocator,
            .scopes = std.ArrayList(std.StringHashMap(Symbol)).init(allocator),
        };
        try st.enterScope();
        return st;
    }

    pub fn define(self: *SymbolTable, symbol: Symbol) !void {
        var current_scope = &self.scopes.items[self.scopes.items.len - 1];
        try current_scope.putNoClobber(symbol.name, symbol);
    }

    pub fn enterScope(self: *SymbolTable) !void {
        const new_scope = std.StringHashMap(Symbol).init(self.allocator);
        try self.scopes.append(new_scope);
    }

    pub fn exitScope(self: *SymbolTable) void {
        var current_scope = self.scopes.pop().?;
        current_scope.deinit();
    }

    pub fn lookup(self: *SymbolTable, key: []const u8) ?Symbol {
        var nth_scope = self.scopes.items.len - 1;

        while (nth_scope >= 0) {
            if (self.scopes.items[nth_scope].get(key)) |sym| {
                return sym;
            }
            nth_scope -= 1;
        }
        return null;
    }
};

pub const Compiler = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    symbol_table: SymbolTable,
    current_function: c.LLVMValueRef,

    allocator: std.mem.Allocator,
    type_env: *ti.TypeEnvironment,

    pub fn init(name: [*c]u8, type_env: *ti.TypeEnvironment, allocator: std.mem.Allocator) !Compiler {
        const context = c.LLVMContextCreate();
        return Compiler{
            .context = context,
            .module = c.LLVMModuleCreateWithNameInContext(name, context),
            .builder = c.LLVMCreateBuilderInContext(context),
            .symbol_table = try SymbolTable.init(allocator),
            .current_function = null,
            .allocator = allocator,
            .type_env = type_env,
        };
    }

    pub fn deinit(self: *Compiler) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn run(self: *Compiler, program: *const ast.AstNode) !void {
        const main_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), null, 0, 0);
        const main_fn = c.LLVMAddFunction(self.module, "main", main_type);
        self.current_function = main_fn;
        const main_entry = c.LLVMAppendBasicBlockInContext(self.context, main_fn, "main_block");
        c.LLVMPositionBuilderAtEnd(self.builder, main_entry);

        _ = try self.codegen(program);

        const current_block = c.LLVMGetInsertBlock(self.builder);
        if (c.LLVMGetBasicBlockTerminator(current_block) == null) {
            _ = c.LLVMBuildRet(self.builder, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));
        }

        const ir_string = c.LLVMPrintModuleToString(self.module);
        defer c.LLVMDisposeMessage(ir_string);
        std.debug.print("Generated IR:\n{s}\n", .{ir_string});

        self.analyzeModule();
        self.writeIRToFile();
    }

    fn codegen(self: *Compiler, node: *const ast.AstNode) anyerror!c.LLVMValueRef {
        switch (node.*) {
            .Program => |p| {
                for (p.program.items) |stmt| {
                    std.debug.print("Visiting program item\n", .{});
                    _ = try self.codegen(&stmt);
                }
                return null;
            },
            .LetStatement => |*ls| {
                std.debug.print("Visiting let statement\n", .{});
                const inferred_type = self.type_env.defs.get(ls.name).?.typ;
                const decl_type = self.getLLVMType(inferred_type);
                const current_block = c.LLVMGetInsertBlock(self.builder);
                // const cur_fn_entry_block = c.LLVMGetEntryBasicBlock(self.current_function);
                const temp_builder = c.LLVMCreateBuilder();
                defer c.LLVMDisposeBuilder(temp_builder);
                c.LLVMPositionBuilderAtEnd(temp_builder, current_block);

                const name = try self.allocator.dupeZ(u8, ls.name.Identifier.value);
                const alloca = c.LLVMBuildAlloca(temp_builder, decl_type, name.ptr);
                std.debug.print("Identifier value: {s}\n", .{ls.name.Identifier.token.literal[0..]});

                const init_value = try self.codegen(ls.expression);
                _ = c.LLVMBuildStore(self.builder, init_value, alloca);
                try self.symbol_table.define(Symbol{
                    .name = name,
                    .type_annotation = inferred_type,
                    .llvm_value = alloca,
                    .llvm_type = self.getLLVMType(inferred_type),
                    .is_parameter = false,
                });
                return alloca;
            },
            .IntegerLiteral => |int_lit| {
                std.debug.print("Visiting integer literal\n", .{});
                const int_type = c.LLVMInt64TypeInContext(self.context);
                const lit: c_ulonglong = @intCast(int_lit.value);
                return c.LLVMConstInt(int_type, lit, 0);
            },
            .BooleanLiteral => |bool_lit| {
                std.debug.print("Visiting boolean literal\n", .{});
                const bool_type = c.LLVMInt1TypeInContext(self.context);
                //FIXME: panics :(
                // const bool_type = self.getLLVMType(self.type_env.types.get(@constCast(node)).?);
                const bool_value: c_ulonglong = if (bool_lit.value) @intCast(1) else @intCast(0);
                return c.LLVMConstInt(bool_type, bool_value, 0);
            },
            .FloatLiteral => |f_lit| {
                std.debug.print("Visiting float literal\n", .{});
                const float_type = c.LLVMDoubleTypeInContext(self.context);
                return c.LLVMConstReal(float_type, f_lit.value);
            },
            .Identifier => |ident| {
                const symbol = self.symbol_table.lookup(ident.value).?;
                const symbol_type = self.getLLVMType(symbol.type_annotation);
                if (symbol.is_parameter) {
                    return symbol.llvm_value;
                } else {
                    return c.LLVMBuildLoad2(self.builder, symbol_type, symbol.llvm_value, symbol.name);
                }
            },
            .ExpressionStatement => |stmt| {
                std.debug.print("Visiting expression statement\n", .{});
                return self.codegen(stmt.expression);
            },
            .Infix => |infix| {
                std.debug.print("Visiting infix\n", .{});
                const expr_type = self.type_env.types.get(@constCast(node)).?;
                return self.compileBinOp(infix.left, infix.right, &infix.operator, &expr_type);
            },
            .Prefix => |prefix| {
                std.debug.print("Visiting prefix\n", .{});
                const expr_type = self.type_env.types.get(@constCast(node)).?;
                return self.compileUnOp(prefix.right, &prefix.operator, &expr_type);
            },
            .BlockStatement => |bl_stmt| {
                var compiled_statement: c.LLVMValueRef = null;
                for (bl_stmt.statements.items) |*stmt| {
                    compiled_statement = try self.codegen(stmt);
                }
                return compiled_statement;
            },
            .If => |ifexp| {
                //TODO: still need to turn the statement into an expression and take care of control flow: return, etc.
                std.debug.print("Visiting if expression\n", .{});
                const current_block = c.LLVMGetInsertBlock(self.builder);
                const function = c.LLVMGetBasicBlockParent(current_block);

                const then_bb = c.LLVMAppendBasicBlockInContext(self.context, function, "then");
                const else_bb = c.LLVMAppendBasicBlockInContext(self.context, function, "else");
                const merge_bb = c.LLVMAppendBasicBlockInContext(self.context, function, "ifcont");
                const expr_type = self.type_env.types.get(node).?;

                std.debug.print("Generating if condition\n", .{});
                const condition = try self.codegen(ifexp.condition);

                _ = c.LLVMBuildCondBr(self.builder, condition, then_bb, else_bb);

                // then branch
                c.LLVMPositionBuilderAtEnd(self.builder, then_bb);
                //TODO: the scope should go into block statement
                try self.symbol_table.enterScope();
                std.debug.print("Generating consequence\n", .{});
                const cons = try self.codegen(ifexp.consequence);
                self.symbol_table.exitScope();
                _ = c.LLVMBuildBr(self.builder, merge_bb);

                // else branch
                c.LLVMPositionBuilderAtEnd(self.builder, else_bb);
                var altern: c.LLVMValueRef = null;
                if (ifexp.alternative) |alt| {
                    std.debug.print("Generating alterantive\n", .{});
                    try self.symbol_table.enterScope();

                    altern = try self.codegen(alt);
                    self.symbol_table.exitScope();
                }
                std.debug.print("Generating merge branch\n", .{});
                _ = c.LLVMBuildBr(self.builder, merge_bb);

                std.debug.print("Positioning builder at merge branch\n", .{});
                c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                if (expr_type != .Void) {
                    const phi = c.LLVMBuildPhi(self.builder, self.getLLVMType(expr_type), "ifres");
                    const phiValues = [_]c.LLVMValueRef{ cons, altern };
                    const phiBlocks = [_]c.LLVMBasicBlockRef{ then_bb, else_bb };
                    c.LLVMAddIncoming(phi, @as([*c]c.LLVMValueRef, @ptrCast(@constCast(&phiValues))), @as([*c]c.LLVMBasicBlockRef, @ptrCast(@constCast(&phiBlocks))), 2);
                    return phi;
                }
                return null;
            },
            .FunctionLiteral => |fl| {
                std.debug.print("Visiting function declaration\n", .{});
                const cur_block = c.LLVMGetInsertBlock(self.builder);
                const cur_instr = c.LLVMGetLastInstruction(cur_block);

                try self.symbol_table.enterScope();
                var param_types = try self.allocator.alloc(c.LLVMTypeRef, fl.parameters.items.len);
                for (0..fl.parameters.items.len) |i| {
                    const ast_type = self.type_env.defs.get(&fl.parameters.items[i]).?.typ;
                    param_types[i] = self.getLLVMType(ast_type);
                }
                const func_type = c.LLVMFunctionType(self.getLLVMType(fl.return_type), @as([*c]c.LLVMTypeRef, @ptrCast(@constCast(param_types))), @as(c_uint, @intCast(fl.parameters.items.len)), 0);
                const name = try self.allocator.dupeZ(u8, fl.name.?);
                const function = c.LLVMAddFunction(self.module, name, func_type);
                const entry_block = c.LLVMAppendBasicBlockInContext(self.context, function, "entry");

                c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
                for (fl.parameters.items, 0..) |*fp, i| {
                    const param_name = try self.allocator.dupeZ(u8, fp.FunctionParameter.ident.Identifier.value);
                    const param = c.LLVMGetParam(function, @as(c_uint, @intCast(i)));
                    try self.symbol_table.define(Symbol{
                        .llvm_value = param,
                        .name = param_name,
                        .type_annotation = fp.FunctionParameter.inferred_type,
                        .llvm_type = self.getLLVMType(fp.FunctionParameter.inferred_type),
                        .is_parameter = true,
                    });
                }
                _ = try self.codegen(fl.body);
                self.symbol_table.exitScope();

                try self.symbol_table.define(Symbol{
                    .name = name,
                    .llvm_value = function,
                    .llvm_type = func_type,
                    .type_annotation = fl.return_type,
                    .is_parameter = false,
                });

                if (cur_instr) |_| {
                    c.LLVMPositionBuilder(self.builder, cur_block, cur_instr);
                } else {
                    c.LLVMPositionBuilderAtEnd(self.builder, cur_block);
                }

                return null;
            },
            .ReturnStatement => |rs| {
                std.debug.print("Visiting return statement\n", .{});
                const typ = self.type_env.types.get(rs.return_value).?;
                if (typ == .Void) {
                    _ = c.LLVMBuildRetVoid(self.builder);
                }
                const expr = try self.codegen(rs.return_value);
                _ = c.LLVMBuildRet(self.builder, expr);

                return null;
            },
            .FunctionCall => |fc| {
                std.debug.print("Visiting function call\n", .{});
                const function = self.symbol_table.lookup(fc.function.Identifier.value).?;
                var params = try self.allocator.alloc(c.LLVMValueRef, fc.arguments.items.len);
                for (fc.arguments.items, 0..) |*arg, i| {
                    params[i] = try self.codegen(arg);
                }
                const call = c.LLVMBuildCall2(
                    self.builder,
                    function.llvm_type,
                    function.llvm_value,
                    @as([*c]c.LLVMValueRef, @ptrCast(params)),
                    @as(c_uint, @intCast(fc.arguments.items.len)),
                    function.name,
                );

                return call;
            },
            .WhileLoopStatement => |wls| {
                std.debug.print("Visiting while loop statement\n", .{});
                const current_block = c.LLVMGetInsertBlock(self.builder);
                const function = c.LLVMGetBasicBlockParent(current_block);

                const header_block = c.LLVMAppendBasicBlockInContext(self.context, function, "while.header");
                const body_block = c.LLVMAppendBasicBlockInContext(self.context, function, "while.body");
                const exit_block = c.LLVMAppendBasicBlockInContext(self.context, function, "while.exit");

                _ = c.LLVMBuildBr(self.builder, header_block);

                c.LLVMPositionBuilderAtEnd(self.builder, header_block);
                const condition = try self.codegen(wls.condition);

                _ = c.LLVMBuildCondBr(self.builder, condition, body_block, exit_block);

                c.LLVMPositionBuilderAtEnd(self.builder, body_block);
                try self.symbol_table.enterScope();
                // discard as we can't make use of the block statements node
                _ = try self.codegen(wls.statements);
                self.symbol_table.exitScope();

                _ = c.LLVMBuildBr(self.builder, header_block);

                c.LLVMPositionBuilderAtEnd(self.builder, exit_block);
                return null;
            },
            else => unreachable,
        }
    }

    fn compileUnOp(self: *Compiler, right: *const ast.AstNode, operator: *const ast.UnaryOp, expr_type: *const ast.Type) !c.LLVMValueRef {
        const compiled_right = try self.codegen(right);

        return switch (operator.*) {
            .Negation => c.LLVMBuildNot(self.builder, compiled_right, "bnot"),
            .Minus => switch (expr_type.*) {
                .Float => c.LLVMBuildFNeg(self.builder, compiled_right, "fneg"),
                .Integer => c.LLVMBuildNeg(self.builder, compiled_right, "ineg"),
                else => unreachable,
            },
        };
    }

    fn compileBinOp(self: *Compiler, left: *const ast.AstNode, right: *const ast.AstNode, operator: *const ast.BinaryOp, expr_type: *const ast.Type) !c.LLVMValueRef {
        const compiled_left = try self.codegen(left);
        const compiled_right = try self.codegen(right);

        return switch (operator.*) {
            .Plus => switch (expr_type.*) {
                .Float => c.LLVMBuildFAdd(self.builder, compiled_left, compiled_right, "fadd"),
                .Integer => c.LLVMBuildAdd(self.builder, compiled_left, compiled_right, "iadd"),
                else => unreachable,
            },
            .Minus => switch (expr_type.*) {
                .Float => c.LLVMBuildFSub(self.builder, compiled_left, compiled_right, "fsub"),
                .Integer => c.LLVMBuildSub(self.builder, compiled_left, compiled_right, "iadd"),
                else => unreachable,
            },
            .Multiply => switch (expr_type.*) {
                .Float => c.LLVMBuildFMul(self.builder, compiled_left, compiled_right, "fmul"),
                else => unreachable,
            },
            .Divide => switch (expr_type.*) {
                .Float => c.LLVMBuildFDiv(self.builder, compiled_left, compiled_right, "fmul"),
                else => unreachable,
            },
            .EqualEqual => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, compiled_left, compiled_right, "fecmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, compiled_left, compiled_right, "iecmp"),
                .Bool => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, compiled_left, compiled_right, "becmp"),
                else => unreachable,
            },
            .NotEqual => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, compiled_left, compiled_right, "fnecmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntNE, compiled_left, compiled_right, "inecmp"),
                .Bool => c.LLVMBuildICmp(self.builder, c.LLVMIntNE, compiled_left, compiled_right, "becmp"),
                else => unreachable,
            },
            .Greater => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, compiled_left, compiled_right, "fgtcmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, compiled_left, compiled_right, "igtcmp"),
                else => unreachable,
            },
            .Less => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, compiled_left, compiled_right, "fltcmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, compiled_left, compiled_right, "iltcmp"),
                else => unreachable,
            },
            .GreaterEqual => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, compiled_left, compiled_right, "fgecmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntSGE, compiled_left, compiled_right, "igecmp"),
                else => unreachable,
            },
            .LessEqual => switch (expr_type.*) {
                .Float => c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, compiled_left, compiled_right, "flecmp"),
                .Integer => c.LLVMBuildICmp(self.builder, c.LLVMIntSLE, compiled_left, compiled_right, "ilecmp"),
                else => unreachable,
            },
        };
    }

    fn getLLVMType(self: *Compiler, type_annotation: ast.Type) c.LLVMTypeRef {
        switch (type_annotation) {
            .Integer => return c.LLVMInt64TypeInContext(self.context),
            .Float => return c.LLVMDoubleTypeInContext(self.context),
            .Bool => return c.LLVMInt1TypeInContext(self.context),
            .String => return c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0),
            .Void => return c.LLVMVoidTypeInContext(self.context),
            else => unreachable,
        }
    }

    fn analyzeModule(self: *Compiler) void {
        var err_msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(err_msg);
        _ = c.LLVMVerifyModule(self.module, c.LLVMAbortProcessAction, &err_msg);
        if (err_msg) |m| {
            std.debug.print("{s}", .{m});
        }
    }

    fn writeIRToFile(self: *Compiler) void {
        var err_msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(err_msg);
        _ = c.LLVMPrintModuleToFile(self.module, "module.ll", &err_msg);
        if (err_msg) |m| {
            std.debug.print("{s}", .{m});
        }
    }
};

test "Test simple codegen" {
    // Initialize
    _ = c.LLVMInitializeNativeTarget();
    _ = c.LLVMInitializeNativeAsmPrinter();
    _ = c.LLVMInitializeNativeAsmParser();
    //_ = c.LLVMInitializeNativeDisassembler();
    c.LLVMLinkInMCJIT();

    // Context creation
    //const context = c.LLVMContextCreate();

    // Module creation
    const module = c.LLVMModuleCreateWithName("module");

    // Builder creation
    const builder = c.LLVMCreateBuilder();

    // The "print" built-in function
    var print_param_types = [_]c.LLVMTypeRef{c.LLVMPointerType(c.LLVMInt8Type(), 0)};
    const print_func_type = c.LLVMFunctionType(c.LLVMInt32Type(), &print_param_types, print_param_types.len, 1);
    const print_func = c.LLVMAddFunction(module, "printf", print_func_type);
    c.LLVMSetFunctionCallConv(print_func, c.LLVMCCallConv);

    // The "sum" function prototype creation
    var sum_param_types = [_]c.LLVMTypeRef{ c.LLVMInt32Type(), c.LLVMInt32Type() };
    const sum_func_type = c.LLVMFunctionType(c.LLVMInt32Type(), &sum_param_types, sum_param_types.len, 0);
    const sum_func = c.LLVMAddFunction(module, "sum", sum_func_type);
    const sum_entry = c.LLVMAppendBasicBlock(sum_func, "entry");
    c.LLVMPositionBuilderAtEnd(builder, sum_entry);
    const sum_param1 = c.LLVMGetParam(sum_func, 0);
    const sum_param2 = c.LLVMGetParam(sum_func, 1);
    const sum_binop = c.LLVMBuildAdd(builder, sum_param1, sum_param2, "res");
    _ = c.LLVMBuildRet(builder, sum_binop);

    // Build a "main" function
    const main_func_type = c.LLVMFunctionType(c.LLVMVoidType(), null, 0, 0);
    const main_func = c.LLVMAddFunction(module, "main", main_func_type);
    const main_entry = c.LLVMAppendBasicBlock(main_func, "entry");
    c.LLVMPositionBuilderAtEnd(builder, main_entry);

    // Call "printf" function with arguments
    const str = c.LLVMBuildGlobalStringPtr(builder, "Hello, world!\nsum(2, 3)=%d\n", "");
    var sum_args = [_]c.LLVMValueRef{
        c.LLVMConstInt(c.LLVMInt32Type(), 2, 0),
        c.LLVMConstInt(c.LLVMInt32Type(), 3, 0),
    };
    const res = c.LLVMBuildCall2(builder, sum_func_type, sum_func, &sum_args, sum_args.len, "");

    var args = [_]c.LLVMValueRef{ str, res };
    _ = c.LLVMBuildCall2(builder, print_func_type, print_func, &args, args.len, "");

    _ = c.LLVMBuildRetVoid(builder);
    // End of "main" function

    // Analysis module
    var err_msg: [*c]u8 = null;
    _ = c.LLVMVerifyModule(module, c.LLVMAbortProcessAction, &err_msg);
    c.LLVMDisposeMessage(err_msg);

    // Write module to file
    err_msg = null;
    _ = c.LLVMPrintModuleToFile(module, "module.ll", &err_msg);
    c.LLVMDisposeMessage(err_msg);

    // Write bitcode to file
    //_ = c.LLVMWriteBitcodeToFile(module, "module.bc");

    // Print module to string in console
    //const outs = c.LLVMPrintModuleToString(module);
    //std.debug.print("{s}\n", .{outs});
    //c.LLVMDisposeMessage(outs);

    // Dump module to stdout
    //c.LLVMDumpModule(module);

    // Execute "main" function
    err_msg = null;
    var exec: c.LLVMExecutionEngineRef = null;
    _ = c.LLVMCreateExecutionEngineForModule(&exec, module, &err_msg);
    const main_exec = c.LLVMGetNamedFunction(module, "main");
    _ = c.LLVMRunFunction(exec, main_exec, 0, null);
    c.LLVMDisposeMessage(err_msg);

    // Dispose execution engine
    c.LLVMDisposeExecutionEngine(exec);
    // Dispose the builder
    c.LLVMDisposeBuilder(builder);
    // Dispose the module
    //c.LLVMDisposeModule(module); //NOTE: module is owned and released by the execution engine
    // Dispose the context
    //c.LLVMContextDispose(context);

    // Shutdown LLVM
    c.LLVMShutdown();
}
