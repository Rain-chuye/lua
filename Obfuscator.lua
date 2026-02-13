local Obfuscator = {}

-- [[ Robust Lua 5.3.3 Bytecode Parser ]]
local function create_parser(dump)
    local cursor = 1
    local function rb() local b = dump:byte(cursor); cursor = cursor + 1; return b end
    local function rbs(n) local s = dump:sub(cursor, cursor + n - 1); cursor = cursor + n; return s end

    local sig = rbs(4)
    if sig ~= "\27Lua" then error("Invalid bytecode signature") end
    local version = rb(); local format = rb(); rbs(6)
    local int_size = rb(); local sizet_size = rb(); local inst_size = rb()
    local lua_int_size = rb(); local lua_num_size = rb()
    rbs(lua_int_size); rbs(lua_num_size)

    local function r_int() return string.unpack("<i" .. int_size, rbs(int_size)) end
    local function r_sizet() return string.unpack("<I" .. sizet_size, rbs(sizet_size)) end
    local function r_lua_int() return string.unpack("<i" .. lua_int_size, rbs(lua_int_size)) end
    local function r_lua_num() return string.unpack("<d", rbs(lua_num_size)) end

    local function rstr()
        local b = rb()
        if not b or b == 0 then return nil end
        local size = b
        if size == 0xFF then size = r_sizet() end
        return rbs(size - 1)
    end

    local function rproto()
        local p = {}
        p.source = rstr()
        p.line_defined = r_int()
        p.last_line_defined = r_int()
        p.num_params = rb()
        p.is_vararg = rb()
        p.max_stack_size = rb()

        local size_code = r_int()
        p.code = {}
        for i = 1, size_code do p.code[i] = string.unpack("<I4", rbs(4)) end

        local size_k = r_int()
        p.constants = {}
        for i = 1, size_k do
            local t = rb()
            if t == 0 then p.constants[i] = {type = "nil", value = nil}
            elseif t == 1 then p.constants[i] = {type = "boolean", value = rb() ~= 0}
            elseif t == 3 then p.constants[i] = {type = "float", value = r_lua_num()}
            elseif t == 19 then p.constants[i] = {type = "integer", value = r_lua_int()}
            elseif t == 4 or t == 20 then p.constants[i] = {type = "string", value = rstr()}
            else error("Unknown constant type: " .. t) end
        end

        local size_upvalues = r_int()
        p.upvalues = {}
        for i = 1, size_upvalues do p.upvalues[i] = { instack = rb() ~= 0, idx = rb() } end

        local size_protos = r_int()
        p.protos = {}
        for i = 1, size_protos do p.protos[i] = rproto() end

        local size_lineinfo = r_int()
        cursor = cursor + size_lineinfo * 4
        local size_locvars = r_int()
        for i = 1, size_locvars do rstr(); rbs(8) end
        local size_upvalue_names = r_int()
        for i = 1, size_upvalue_names do rstr() end

        return p
    end

    rb() -- upvalues count in header
    return rproto()
end

-- [[ Obfuscation Engine ]]
local function transform(proto)
    local rev_map, state_map = {}, {}
    local op_map = {}
    local ops = {}
    for i = 0, 50 do ops[i+1] = i end
    for i = #ops, 2, -1 do
        local j = math.random(i); ops[i], ops[j] = ops[j], ops[i]
    end
    for i = 0, 50 do
        local new_op = ops[i+1]
        op_map[i] = new_op; rev_map[new_op] = i; state_map[i] = math.random(100000, 999999)
    end

    local function crypt(p)
        for _, k in ipairs(p.constants) do
            if k.type == "string" then
                local s = k.value; local key = math.random(1, 255); local enc = {}
                for j = 1, #s do enc[j] = s:byte(j) ~ key end
                k.type = "enc_string"; k.value = {enc = enc, key = key}
            elseif k.type == "integer" then
                local key = math.random(1, 1000000); k.type = "enc_integer"; k.value = {val = k.value ~ key, key = key}
            end
        end
        for i = 1, #p.code do
            local inst = p.code[i]; local op = inst & 0x3F; local rest = inst & ~0x3F
            p.code[i] = rest | (op_map[op] or op)
        end
        p.source = "=[obfuscated]"; p.line_defined = 0; p.last_line_defined = 0
        for _, sub in ipairs(p.protos) do crypt(sub) end
    end
    crypt(proto)
    return proto, rev_map, state_map
end

-- [[ Code Generator ]]
function Obfuscator.obfuscate(source, intensity)
    intensity = intensity or 2
    local ok, func = pcall(load, source)
    if not ok or not func then error("Load Error: " .. tostring(func)) end
    local dump = string.dump(func)
    local proto = create_parser(dump)
    local obf_proto, rev_map, state_map = transform(proto)

    local function v(name)
        if intensity < 2 then return name end
        local s = ""
        for i = 1, 10 do local r = math.random(97, 122); s = s .. string.char(r) end
        return s
    end

    local reps = {}
    local names = {"REGS", "A", "B", "C", "BX", "CODE", "PC", "GETRK", "CU", "TOP", "UNPACK", "STATE", "SBX", "OPENUPS", "CP", "WRAP", "VA", "P", "E", "U", "K", "ARGS", "I", "OP", "OUTER", "REALOP", "REVMAP", "STATEMAP", "ENTRY", "PRX", "UD", "NP", "NU", "F", "RES", "RESC", "RET", "STEP", "IDX", "LIM", "STR", "CV", "OFF", "N", "CAP"}
    for _, n in ipairs(names) do reps[n] = v(n) end

    local opcodes = {
        [0] = "REGS[A] = REGS[B]",
        [1] = "REGS[A] = GETRK(BX + 256)",
        [2] = "local ax = (CODE[PC] >> 6) & 0x3FFFFFF; PC = PC + 1; REGS[A] = GETRK(ax + 256)",
        [3] = "REGS[A] = (B ~= 0); if C ~= 0 then PC = PC + 1 end",
        [4] = "for i = A, A + B do REGS[i] = nil end",
        [5] = "REGS[A] = CU[B + 1].v",
        [6] = "REGS[A] = CU[B + 1].v[GETRK(C)]",
        [7] = "REGS[A] = REGS[B][GETRK(C)]",
        [8] = "CU[A + 1].v[GETRK(B)] = GETRK(C)",
        [9] = "CU[B + 1].v = REGS[A]",
        [10] = "REGS[A][GETRK(B)] = GETRK(C)",
        [11] = "REGS[A] = {}",
        [12] = "REGS[A+1] = REGS[B]; REGS[A] = REGS[B][GETRK(C)]",
        [13] = "REGS[A] = GETRK(B) + GETRK(C)",
        [14] = "REGS[A] = GETRK(B) - GETRK(C)",
        [15] = "REGS[A] = GETRK(B) * GETRK(C)",
        [16] = "REGS[A] = GETRK(B) % GETRK(C)",
        [17] = "REGS[A] = GETRK(B) ^ GETRK(C)",
        [18] = "REGS[A] = GETRK(B) / GETRK(C)",
        [19] = "REGS[A] = GETRK(B) // GETRK(C)",
        [20] = "REGS[A] = GETRK(B) & GETRK(C)",
        [21] = "REGS[A] = GETRK(B) | GETRK(C)",
        [22] = "REGS[A] = GETRK(B) ~ GETRK(C)",
        [23] = "REGS[A] = GETRK(B) << GETRK(C)",
        [24] = "REGS[A] = GETRK(B) >> GETRK(C)",
        [25] = "REGS[A] = -REGS[B]",
        [26] = "REGS[A] = ~REGS[B]",
        [27] = "REGS[A] = not REGS[B]",
        [28] = "REGS[A] = #REGS[B]",
        [29] = "local STR = REGS[B]; for i = B + 1, C do STR = STR .. REGS[i] end; REGS[A] = STR",
        [30] = "PC = PC + SBX; if A ~= 0 then for i, ENTRY in pairs(OPENUPS) do if ENTRY.idx >= A - 1 then ENTRY.nup.v = REGS[ENTRY.idx]; ENTRY.nup.closed = true; OPENUPS[i] = nil end end end",
        [31] = "if (GETRK(B) == GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [32] = "if (GETRK(B) < GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [33] = "if (GETRK(B) <= GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [34] = "if (not REGS[A]) == (C ~= 0) then PC = PC + 1 end",
        [35] = "if (not REGS[B]) ~= (C ~= 0) then REGS[A] = REGS[B] else PC = PC + 1 end",
        [36] = "local F, np, nr = REGS[A], B-1, C-1; local CA = {}; if np == -1 then for i=A+1,TOP do CA[#CA+1]=REGS[i] end else for i=1,np do CA[i]=REGS[A+i] end end; local RES, RESC = CAP(F(UNPACK(CA))); if nr == -1 then for i=1,RESC do REGS[A+i-1]=RES[i] end; TOP=A+RESC-1 else for i=1,nr do REGS[A+i-1]=RES[i] end; TOP=A+nr-1 end",
        [37] = "local F, np = REGS[A], B-1; local CA = {}; if np == -1 then for i=A+1,TOP do CA[#CA+1]=REGS[i] end else for i=1,np do CA[i]=REGS[A+i] end end; return F(UNPACK(CA))",
        [38] = "local nr = B-1; local RET = {}; if nr == -1 then for i=A,TOP do RET[#RET+1]=REGS[i] end else for i=1,nr do RET[i]=REGS[A+i-1] end end; return UNPACK(RET)",
        [39] = "local STEP, LIM = REGS[A+2], REGS[A+1]; local IDX = REGS[A]+STEP; if (STEP>0 and IDX<=LIM) or (STEP<0 and IDX>=LIM) then PC=PC+SBX; REGS[A]=IDX; REGS[A+3]=IDX end",
        [40] = "REGS[A] = REGS[A] - REGS[A+2]; PC = PC + SBX",
        [41] = "local RES, RESC = CAP(REGS[A](REGS[A+1], REGS[A+2])); for i=1,C do REGS[A+3+i-1]=RES[i] end",
        [42] = "if REGS[A+1] ~= nil then REGS[A]=REGS[A+1]; PC=PC+SBX end",
        [43] = "local N, CV = B, C; if N==0 then N=TOP-A end; if CV==0 then CV=(CODE[PC]>>6)&0x3FFFFFF; PC=PC+1 end; local OFF=(CV-1)*50; for i=1,N do REGS[A][OFF+i]=REGS[A+i] end",
        [44] = "local NP, NU = CP.protos[BX+1], {}; for i=1,#NP.upvalues do local UD = NP.upvalues[i]; if UD.instack then local found = false; for _, ENTRY in pairs(OPENUPS) do if ENTRY.idx == UD.idx and not ENTRY.nup.closed then NU[i] = ENTRY.prx; found = true; break end end; if not found then local nup = {v=nil, closed=false}; local prx = setmetatable({}, {__index=function(_,k) if k=='v' then return nup.closed and nup.v or REGS[UD.idx] end end, __newindex=function(_,k,v) if k=='v' then if nup.closed then nup.v=v else REGS[UD.idx]=v end end end}); NU[i]=prx; table.insert(OPENUPS, {idx=UD.idx, nup=nup, prx=prx}) end else NU[i]=CU[UD.idx+1] end end; REGS[A] = WRAP(NP, NU)",
        [45] = "local N = B-1; if N==-1 then for i=1,#VA do REGS[A+i-1]=VA[i] end; TOP=A+#VA-1 else for i=1,N do REGS[A+i-1]=VA[i] end; TOP=A+N-1 end",
        [46] = "PC = PC",
    }

    local branches = ""
    local keys = {}
    for k in pairs(opcodes) do table.insert(keys, k) end
    for i = #keys, 2, -1 do local j = math.random(i); keys[i], keys[j] = keys[j], keys[i] end
    for i, op in ipairs(keys) do
        local logic = opcodes[op]:gsub("[%a_][%w_]*", function(m) return reps[m] or m end)
        branches = branches .. "            " .. (i==1 and "if" or "elseif") .. " " .. reps.STATE .. " == " .. state_map[op] .. " then\n                " .. logic .. "\n"
    end
    branches = branches .. "            end\n"

    local vm_template = [=[
local function OUTER(P, U)
    local function WRAP(CP, CU)
        return function(...)
            local CODE, K = CP.code, CP.constants
            local PC, REGS, TOP, OPENUPS = 1, {}, 0, {}
            local ARGS = {...}
            for i = 0, CP.num_params - 1 do REGS[i] = ARGS[i+1] end
            local VA = {}
            if CP.is_vararg ~= 0 then for i = CP.num_params + 1, #ARGS do VA[#VA+1] = ARGS[i] end end
            TOP = CP.max_stack_size
            local function GETRK(val)
                if val >= 256 then
                    local kv = K[val-255]
                    if kv.type == "enc_string" then
                        if not kv.cache then local s = ""; for _, b in ipairs(kv.value.enc) do s = s .. string.char(b ~ kv.value.key) end; kv.cache = s end
                        return kv.cache
                    elseif kv.type == "enc_integer" then return kv.value.val ~ kv.value.key end
                    return kv.value
                end
                return REGS[val]
            end
            local UNPACK = table.unpack or unpack
            local function CAP(...) return {...}, select('#', ...) end
            while true do
                local I = CODE[PC]
                if not I then break end
                local OP = I & 0x3F
                local A = (I >> 6) & 0xFF
                local C = (I >> 14) & 0x1FF
                local B = (I >> 23) & 0x1FF
                local BX = (I >> 14) & 0x3FFFF
                local SBX = BX - 131071
                PC = PC + 1
                local STATE = STATEMAP[REVMAP[OP] or -1]
                if STATE then
]=] .. branches .. [=[
                else break end
            end
        end
    end
    return WRAP(P, U)
end
]=]
    vm_template = vm_template:gsub("[%a_][%w_]*", function(m) return reps[m] or m end)

    local function serialize(t)
        if type(t) ~= "table" then return type(t) == "string" and string.format("%q", t) or tostring(t) end
        local s = "{"
        for k, v in pairs(t) do
            local key = type(k) == "number" and "["..k.."]" or "['"..tostring(k).."']"
            s = s .. key .. "=" .. serialize(v) .. ","
        end
        return s .. "}"
    end

    local output = "local " .. reps.REVMAP .. " = " .. serialize(rev_map) .. "\n"
    output = output .. "local " .. reps.STATEMAP .. " = " .. serialize(state_map) .. "\n"
    output = output .. vm_template .. "\n"
    output = output .. "local proto = " .. serialize(obf_proto) .. "\n"
    output = output .. "return " .. reps.OUTER .. "(proto, {{v=_ENV or _G}})()\n"
    return output
end

return Obfuscator
