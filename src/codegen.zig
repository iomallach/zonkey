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
const std = @import("std");

const Symbol = struct {
    name: [:0]const u8,
    type_annotation: ast.Type,
    llvm_value: c.LLVMValueRef,
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

    pub fn leaveScope(self: *SymbolTable) void {
        const current_scope = self.scopes.pop().?;
        var it = current_scope.iterator();
        for (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.name);
        }
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

    pub fn init(name: [*c]u8, allocator: std.mem.Allocator) !Compiler {
        const context = c.LLVMContextCreate();
        return Compiler{
            .context = context,
            .module = c.LLVMModuleCreateWithNameInContext(name, context),
            .builder = c.LLVMCreateBuilderInContext(context),
            .symbol_table = try SymbolTable.init(allocator),
            .current_function = null,
            .allocator = allocator,
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

        _ = c.LLVMBuildRet(self.builder, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

        self.analyzeModule();
        self.writeIRToFile();
    }

    fn codegen(self: *Compiler, node: *const ast.AstNode) !c.LLVMValueRef {
        switch (node.*) {
            .Program => |p| {
                for (p.program.items) |stmt| {
                    std.debug.print("Visiting program item\n", .{});
                    _ = try self.codegen(&stmt);
                }
                return null;
            },
            .LetStatement => |ls| {
                std.debug.print("Visiting let statement\n", .{});
                const decl_type = self.getLLVMType(ls.type_annotation);
                const cur_fn_entry_block = c.LLVMGetEntryBasicBlock(self.current_function);
                const temp_builder = c.LLVMCreateBuilder();
                defer c.LLVMDisposeBuilder(temp_builder);
                c.LLVMPositionBuilderAtEnd(temp_builder, cur_fn_entry_block);

                const name = try self.allocator.dupeZ(u8, ls.name.Identifier.value);
                const alloca = c.LLVMBuildAlloca(temp_builder, decl_type, name.ptr);
                std.debug.print("Identifier value: {s}\n", .{ls.name.Identifier.token.literal[0..]});

                const init_value = try self.codegen(ls.value);
                _ = c.LLVMBuildStore(self.builder, init_value, alloca);
                try self.symbol_table.define(Symbol{
                    .name = name,
                    .type_annotation = ls.type_annotation,
                    .llvm_value = alloca,
                });
                return alloca;
            },
            .IntegerLiteral => |int_lit| {
                std.debug.print("Visiting integer literal\n", .{});
                const int_type = c.LLVMInt64TypeInContext(self.context);
                const lit: c_ulonglong = @intCast(int_lit.value);
                return c.LLVMConstInt(int_type, lit, 0);
            },
            .Identifier => |ident| {
                const symbol = self.symbol_table.lookup(ident.value).?;
                const symbol_type = self.getLLVMType(symbol.type_annotation);
                return c.LLVMBuildLoad2(self.builder, symbol_type, symbol.llvm_value, symbol.name);
            },
            .ExpressionStatement => |stmt| {
                std.debug.print("Visiting expression statement\n", .{});
                return try self.codegen(stmt.expression);
            },
            .Infix => |infix| {
                std.debug.print("Visiting infix\n", .{});
                if (std.mem.eql(u8, infix.operator, "+")) {
                    const left = try self.codegen(infix.left);
                    const right = try self.codegen(infix.right);
                    return c.LLVMBuildAdd(self.builder, left, right, "add");
                }
                return error.Unreachable;
            },
            else => {
                return error.Unreacheable;
            },
        }
    }

    fn getLLVMType(self: *Compiler, type_annotation: ast.Type) c.LLVMTypeRef {
        return switch (type_annotation) {
            .Integer => c.LLVMInt64TypeInContext(self.context),
            .Float => c.LLVMDoubleTypeInContext(self.context),
            .Bool => c.LLVMInt1TypeInContext(self.context),
            .String => c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0),
            .Void => c.LLVMVoidTypeInContext(self.context),
        };
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
