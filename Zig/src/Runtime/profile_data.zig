pub const ProfileData = struct {
    counts: std.AutoHashMap(usize, usize),
    // pc -> reg -> value frequency
    value_profiles: std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))),
    // pc -> block_idx -> frequency
    call_profiles: std.AutoHashMap(usize, std.AutoHashMap(usize, usize)),
    allocator: Allocator,

    pub fn init(allocator: Allocator) ProfileData {
        return .{
            .counts = std.AutoHashMap(usize, usize).init(allocator),
            .value_profiles = std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))).init(allocator),
            .call_profiles = std.AutoHashMap(usize, std.AutoHashMap(usize, usize)).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ProfileData) void {
        self.counts.deinit();
        var it = self.value_profiles.iterator();
        while (it.next()) |entry| {
            var reg_it = entry.value_ptr.iterator();
            while (reg_it.next()) |reg_entry| {
                reg_entry.value_ptr.deinit();
            }
            entry.value_ptr.deinit();
        }
        self.value_profiles.deinit();

        var call_it = self.call_profiles.iterator();
        while (call_it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.call_profiles.deinit();
    }

    pub fn record(self: *ProfileData, pc: usize) !void {
        const entry = try self.counts.getOrPut(pc);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    pub fn recordCall(self: *ProfileData, pc: usize, block_idx: usize) !void {
        var pc_entry = try self.call_profiles.getOrPut(pc);
        if (!pc_entry.found_existing) {
            pc_entry.value_ptr.* = std.AutoHashMap(usize, usize).init(self.allocator);
        }
        const freq_entry = try pc_entry.value_ptr.getOrPut(block_idx);
        if (freq_entry.found_existing) {
            freq_entry.value_ptr.* += 1;
        } else {
            freq_entry.value_ptr.* = 1;
        }
    }

    pub fn getDominantCallTarget(self: ProfileData, pc: usize, threshold: usize) ?usize {
        const pc_map = self.call_profiles.get(pc) orelse return null;
        var it = pc_map.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* >= threshold) return entry.key_ptr.*;
        }
        return null;
    }

    pub fn recordValue(self: *ProfileData, pc: usize, reg: u32, val: Value) !void {
        if (val != .int) return;
        const v = val.int;

        var pc_entry = try self.value_profiles.getOrPut(pc);
        if (!pc_entry.found_existing) {
            pc_entry.value_ptr.* = std.AutoHashMap(u32, std.AutoHashMap(u64, usize)).init(self.allocator);
        }
        
        var reg_entry = try pc_entry.value_ptr.getOrPut(reg);
        if (!reg_entry.found_existing) {
            reg_entry.value_ptr.* = std.AutoHashMap(u64, usize).init(self.allocator);
        }

        const freq_entry = try reg_entry.value_ptr.getOrPut(v);
        if (freq_entry.found_existing) {
            freq_entry.value_ptr.* += 1;
        } else {
            freq_entry.value_ptr.* = 1;
        }
    }

    pub fn getStableValue(self: ProfileData, pc: usize, reg: u32, threshold: usize) ?u64 {
        const pc_map = self.value_profiles.get(pc) orelse return null;
        const reg_map = pc_map.get(reg) orelse return null;
        var it = reg_map.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* >= threshold) return entry.key_ptr.*;
        }
        return null;
    }
};
