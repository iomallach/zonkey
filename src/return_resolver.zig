const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");

pub const ReturnResolver = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!ReturnResolver {
        return ReturnResolver{
            .allocator = allocator,
        };
    }

    pub fn resolve(self: *ReturnResolver, program: *ast.AstNode) error{OutOfMemory}!void {
        switch (program.*) {
            .Program => |prog| {
                for (prog.program.items) |*p| {
                    _ = try self.resolve(p);
                }
            },
            .FunctionLiteral => |fl| {
                const last_statement = fl.body.BlockStatement.statements.getLastOrNull();
                if (last_statement) |ls| {
                    switch (ls) {
                        .ReturnStatement => {},
                        .ExpressionStatement => |es| {
                            if (!es.discarded) {
                                fl.body.BlockStatementl.statements.items[fl.body.BlockStatement.statements.items.len - 1] = ast.AstNode{ .ReturnStatement = ast.ReturnStatement{
                                    .return_value = es.expression,
                                    .token = es.token,
                                } };
                            } else {
                                //     fl.body.BlockStatement.statements.append(
                                //     ast.AstNode{
                                //         .ReturnStatement = ast.ReturnStatement{
                                //             .return_value =
                                //         }
                                //     }
                                // )
                            }
                        },
                        else => {},
                    }
                } else {
                    // Empty body, a TODO:
                    unreachable;
                }
            },
            else => {},
        }
    }
};
