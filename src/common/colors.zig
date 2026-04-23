const std = @import("std");


pub const ColorName = enum {
    black,
    red,
    green,
    yellow,
    blue,
    purple,
    cyan,
    white,
    reset,
};

pub const Effect = enum {
    bold,
    underline,
    background,
    none,
};

pub const Color = struct {
    color: ColorName,
    effect: Effect,
    high_intensity: bool,


    pub const RESET = Color.init(.reset, .none, false);
    pub const RED = Color.init(.red, .none, false);
    pub const BOLD_RED = Color.init(.red, .bold, false);
    pub const YELLOW = Color.init(.yellow, .none, false);

    pub fn init(color: ColorName, effect: Effect, high_intensity: bool) @This() {
        return .{
            .color = color,
            .effect = effect,
            .high_intensity = high_intensity,
        };
    }

    pub fn allocPrint(self: *const @This(), allocator: std.mem.Allocator) ![]const u8 {
        const args = self.get_args();
        var arg_list: []const u8 = undefined;
        if (args.@"0") |v| {
            if (args.@"1") |v2| {
                arg_list = try std.fmt.allocPrint(allocator, "{};{}", .{v, v2});
            } else {
                arg_list = try std.fmt.allocPrint(allocator, "{}", .{v});
            }
        } else if (args.@"1") |v| {
            arg_list = try std.fmt.allocPrint(allocator, "{}", .{v});
        }
        const out = try std.fmt.allocPrint(allocator, "\x1B[{s}m", .{ arg_list });
        return out;
    }

    pub fn print(self: *const @This(), writer: *std.Io.Writer) !void {
        const args = self.get_args();
        try writer.print("\x1B[", .{});
        if (args.@"0") |v| {
            try writer.print("{}", .{v});
        }

        if (args.@"1") |v| {
            if (args.@"0") |_| {
                try writer.print(";", .{});
            }
            try writer.print("{}", .{v});
        }
        try writer.print("m", .{});
    }

    fn get_args(self: *const @This()) struct {?u8, ?u8} {
        var first: ?u8 = switch (self.effect) {
            .none => 0,
            .bold => 1,
            .underline => 4,
            else => null,
        };

        if (self.high_intensity and self.effect == .background) first = 0;

        const second_base: ?u8 = switch(self.color) {
            .black => 0,
            .red => 1,
            .green => 2,
            .yellow => 3,
            .blue => 4,
            .purple => 5,
            .cyan => 6,
            .white => 7,
            else => null
        };

        const second_addition: u8 = if (self.effect == .background and self.high_intensity) 100 
            else if (self.high_intensity) 90
            else if (self.effect == .background) 40
            else 30;
        const second = if (second_base != null) second_base.? + second_addition else second_base;
        return .{
            first,
            second,
        };
    }

};
