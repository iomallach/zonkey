const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig").Parser;
const tok = @import("token.zig");
const ast = @import("ast.zig");
const codegen = @import("codegen.zig");
const tc = @import("type_inference.zig");

// pub fn main() !void {
//     const stdin = std.io.getStdIn().reader();
//     const stdout = std.io.getStdOut();
//
//     try stdout.writeAll("Zonkey sila\n");
//
//     var buffer: [1024]u8 = undefined;
//
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();
//     while (true) {
//         try stdout.writeAll("> ");
//         const input = try stdin.readUntilDelimiter(&buffer, '\n');
//         if (std.mem.eql(u8, input, "quit")) {
//             break;
//         }
//
//         var lex = Lexer.Lexer.init(input, allocator);
//         var tokens = try lex_tokens(&lex, allocator);
//         const slice_tokens = try tokens.toOwnedSlice();
//
//         var parser = try Parser.init(slice_tokens, allocator);
//         const program = parser.parse() catch |err| {
//             if (parser.errors().*.items.len > 0) {
//                 std.debug.print("Errors:\n", .{});
//                 for (parser.errors().*.items, 1..) |e, i| {
//                     std.debug.print("  {d}: {s}\n", .{ i, e });
//                 }
//             }
//             return err;
//         };
//         if (parser.errors().*.items.len > 0) {
//             std.debug.print("Errors:\n", .{});
//             for (parser.errors().*.items, 1..) |e, i| {
//                 std.debug.print("  {d}: {s}\n", .{ i, e });
//             }
//         }
//         _ = program;
//
//         try stdout.writeAll(input);
//         try stdout.writeAll("\n");
//     }
// }

pub fn lex_tokens(lexer: *Lexer.Lexer, allocator: std.mem.Allocator) !std.ArrayList(tok.Token) {
    var lexed_tokens = std.ArrayList(tok.Token).init(allocator);

    while (true) {
        const cur_token = try lexer.next_token();
        try lexed_tokens.append(cur_token);
        if (cur_token.token_type == tok.TokenType.EOF) {
            break;
        }
    }
    return lexed_tokens;
}

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

fn processSimpleExpression(input: []const u8, allocator: std.mem.Allocator) !ast.AstNode {
    var lex = Lexer.Lexer.init(input, allocator);
    var tokens = try lex_tokens(&lex, allocator);
    const slice_tokens = try tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = parser.parse() catch |err| {
        if (parser.errors().*.items.len > 0) {
            std.debug.print("Errors:\n", .{});
            for (parser.errors().*.items, 1..) |e, i| {
                std.debug.print("  {d}: {s}\n", .{ i, e });
            }
        }
        return err;
    };
    if (parser.errors().*.items.len > 0) {
        std.debug.print("Errors:\n", .{});
        for (parser.errors().*.items, 1..) |e, i| {
            std.debug.print("  {d}: {s}\n", .{ i, e });
        }
    }
    return program;
}

fn test_codegen() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const prog = try processSimpleExpression("4 + 5", allocator);
    const infix = prog.program.items[0].ExpressionStatement.expression.Infix;
    // _ = infix;

    _ = c.LLVMInitializeNativeTarget();
    _ = c.LLVMInitializeNativeAsmPrinter();
    _ = c.LLVMInitializeNativeAsmParser();
    const context = c.LLVMContextCreate();
    c.LLVMLinkInMCJIT();
    //
    // Module creation
    const module = c.LLVMModuleCreateWithNameInContext("module", context);

    // Builder creation
    const builder = c.LLVMCreateBuilderInContext(context);

    // Define sum
    var sum_param_types = [_]c.LLVMTypeRef{ c.LLVMInt32TypeInContext(context), c.LLVMInt32TypeInContext(context) };
    std.debug.print("Defining sum function\n", .{});
    const sum_func_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(context), &sum_param_types, 2, 0);
    const sum_func = c.LLVMAddFunction(module, "sum_ints", sum_func_type);
    const sum_func_entry = c.LLVMAppendBasicBlockInContext(context, sum_func, "sum_entry");
    c.LLVMPositionBuilderAtEnd(builder, sum_func_entry);
    const param_1 = c.LLVMGetParam(sum_func, 0);
    const param_2 = c.LLVMGetParam(sum_func, 1);
    const sum_binop = c.LLVMBuildAdd(builder, param_1, param_2, "sum");
    _ = c.LLVMBuildRet(builder, sum_binop);
    std.debug.print("Defined function\n", .{});

    // Main
    const main_func_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(context), null, 0, 0);
    const main_func = c.LLVMAddFunction(module, "main", main_func_type);
    const main_entry = c.LLVMAppendBasicBlockInContext(context, main_func, "main_entry");
    // const main_func_type = c.LLVMFunctionType(c.LLVMVoidTypeInContext(context), null, 0, 0);
    // const main_func = c.LLVMAddFunction(module, "main", main_func_type);
    // const main_entry = c.LLVMAppendBasicBlockInContext(context, main_func, "main_entry");
    c.LLVMPositionBuilderAtEnd(builder, main_entry);
    if (std.mem.eql(u8, infix.operator, "+")) {
        std.debug.print("Defining sum op\n", .{});
        const int_type = c.LLVMInt32TypeInContext(context);
        const lhs_long: c_ulonglong = @intCast(infix.left.IntegerLiteral.value);
        const rhs_long: c_ulonglong = @intCast(infix.right.IntegerLiteral.value);
        std.debug.print("Casted\n", .{});
        const lhs = c.LLVMConstInt(int_type, lhs_long, 0);
        const rhs = c.LLVMConstInt(int_type, rhs_long, 0);
        var sum_args = [_]c.LLVMValueRef{ lhs, rhs };
        std.debug.print("Built sum\n", .{});
        const ret = c.LLVMBuildCall2(builder, sum_func_type, sum_func, &sum_args, 2, "");
        std.debug.print("Built call\n", .{});

        _ = c.LLVMBuildRet(builder, ret);
    } else {
        unreachable;
    }
    // _ = c.LLVMBuildRetVoid(builder);

    // Analysis module
    var err_msg: [*c]u8 = null;
    _ = c.LLVMVerifyModule(module, c.LLVMAbortProcessAction, &err_msg);
    c.LLVMDisposeMessage(err_msg);

    std.debug.print("Analyzed\n", .{});

    const triple: [*c]u8 = c.LLVMGetDefaultTargetTriple();
    c.LLVMSetTarget(module, triple);
    // Write module to file
    err_msg = null;
    _ = c.LLVMPrintModuleToFile(module, "module.ll", &err_msg);
    c.LLVMDisposeMessage(err_msg);

    var target: c.LLVMTargetRef = null;
    err_msg = null;
    _ = c.LLVMGetTargetFromTriple(triple, &target, &err_msg);
    c.LLVMDisposeMessage(err_msg);

    const target_machine = c.LLVMCreateTargetMachine(target, triple, "", "", c.LLVMCodeGenLevelDefault, c.LLVMRelocDefault, c.LLVMCodeModelDefault);

    // c.LLVMSetDataLayout(module, c.LLVMCreateTargetDataLayout(target_machine));
    err_msg = null;
    _ = c.LLVMTargetMachineEmitToFile(target_machine, module, "out.s", c.LLVMAssemblyFile, &err_msg);
    c.LLVMDisposeMessage(err_msg);

    err_msg = null;
    _ = c.LLVMTargetMachineEmitToFile(target_machine, module, "out.o", c.LLVMObjectFile, &err_msg);
    c.LLVMDisposeMessage(err_msg);

    // Dispose the builder
    c.LLVMDisposeBuilder(builder);
    // Dispose the module
    c.LLVMDisposeModule(module); //NOTE: module is owned and released by the execution engine
    // Dispose the context
    c.LLVMContextDispose(context);
    c.LLVMDisposeTargetMachine(target_machine);
    c.LLVMDisposeMessage(triple);

    // Shutdown LLVM
    c.LLVMShutdown();
}

pub fn main() !void {
    // try test_codegen();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const file = try std.fs.cwd().openFile("programs/current.zk", .{});
    const code = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    std.debug.print("Code:\n{s}\n", .{code});

    var lex = Lexer.Lexer.init(code, allocator);
    var tokens = try lex_tokens(&lex, allocator);
    const slice_tokens = try tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    var program = parser.parse() catch |err| {
        if (parser.errors().*.items.len > 0) {
            std.debug.print("Errors:\n", .{});
            for (parser.errors().*.items, 1..) |e, i| {
                std.debug.print("  {d}: {s}\n", .{ i, e });
            }
        }
        return err;
    };
    if (parser.errors().*.items.len > 0) {
        std.debug.print("Errors:\n", .{});
        for (parser.errors().*.items, 1..) |e, i| {
            std.debug.print("  {d}: {s}\n", .{ i, e });
        }
        return error.Badaboom;
    }

    var type_checker = try tc.TypeChecker.init(allocator);
    _ = try type_checker.inferAndCheck(&program);
    if (type_checker.errors_list.items.len > 0) {
        std.debug.print("Errors:\n", .{});
        for (type_checker.errors_list.items, 1..) |e, i| {
            std.debug.print("  {d}: {s}\n", .{ i, e });
        }
        return error.Badaboom;
    }

    var compiler = try codegen.Compiler.init(@as([*c]u8, @ptrCast(@constCast("main"))), &type_checker.type_env, allocator);
    defer compiler.deinit();

    for (program.Program.program.items) |stmt| {
        std.debug.print("Statement: {any}\n", .{stmt});
    }
    try compiler.run(&program);
}
