const std = @import("std");
// const dl = @import("dynlib.zig");

pub fn x(name: []const u8, sym: [:0]const u8) !void {
    var lib = try std.DynLib.open(name);

    const addFn = lib.lookup(fn (i32, i32) callconv(.C) i32, sym) orelse return error.SymbolNotFound;

    //const addFn = try lib.lookup(fn (i32, i32) callconv(.C) i32, sym);

    const result = addFn(12, 34);

    std.debug.print("Hello, world! {d}\n", .{result});
    lib.close();
}

fn workerFunction() !void {
    std.debug.print("Thread {d}\n", .{std.Thread.getCurrentId()});
    try x("libmathstest3.dylib", "add");
    std.time.sleep(1000000000);
}

pub fn main() !void {
    std.debug.print("Thread {d}\n", .{std.Thread.getCurrentId()});
    // try x("libmathstest.dylib", "addex");
    // try x("libmathstest3.dylib", "add");
    while (true) {
        // var t = try std.Thread.spawn(.{}, workerFunction, .{});
        // t.join();
        try workerFunction();
    }
}
