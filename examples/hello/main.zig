const std = @import("std");

fn main__x() i32 {
    return 5;
}

pub fn main() void {
    const x = main__x;
    std.debug.print("Hello {s} {d}\n", .{ "world", x() });
}
