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
    local op_to_state = {}
    local ops = {}
    for i = 0, 50 do ops[i+1] = i end
    for i = #ops, 2, -1 do
        local j = math.random(i); ops[i], ops[j] = ops[j], ops[i]
    end
    for i = 0, 50 do
        local new_op = ops[i+1]
        op_to_state[new_op] = math.random(100000, 999999)
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
        -- Opcode randomization is implicitly handled by the dispatcher mapping
        p.source = "=[obfuscated]"; p.line_defined = 0; p.last_line_defined = 0
        for _, sub in ipairs(p.protos) do crypt(sub) end
    end
    crypt(proto)
    return proto, op_to_state
end

-- [[ Code Generator ]]
function Obfuscator.obfuscate(source, intensity)
    intensity = intensity or 2
    local ok, func = pcall(load, source)
    if not ok or not func then error("Load Error: " .. tostring(func)) end
    local dump = string.dump(func)
    local proto = create_parser(dump)
    local obf_proto, op_to_state = transform(proto)

    local function v(name)
        if intensity < 2 then return name end
        local s = ""
        for i = 1, 10 do local r = math.random(97, 122); s = s .. string.char(r) end
        return s
    end

    local reps = {}
    local names = {"REGS", "A", "B", "C", "BX", "CODE", "PC", "GETRK", "CU", "TOP", "UNPACK", "STATE", "SBX", "OPENUPS", "CP", "WRAP", "VA", "P", "E", "U", "K", "ARGS", "I", "OP", "OUTER", "STATE_MAP", "ENTRY", "PRX", "UD", "NP", "NU", "F", "RES", "RESC", "RET", "STEP", "IDX", "LIM", "STR", "CV", "OFF", "N", "CAP", "N_ARGS", "N_RES", "NA", "AX"}
    for _, n in ipairs(names) do reps[n] = v(n) end

    local opcodes = {
        [0] = "REGS[A] = REGS[B]",
        [1] = "REGS[A] = GETRK(BX + 256)",
        [2] = "local AX = (CODE[PC] >> 6) & 0x3FFFFFF; PC = PC + 1; REGS[A] = GETRK(AX + 256)",
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
        [36] = "local F, RES, RESC; local NA = 0; local CA = {}; if B == 0 then for i = A + 1, TOP do NA = NA + 1; CA[NA] = REGS[i] end else for i = 1, B - 1 do NA = NA + 1; CA[NA] = REGS[A+i] end end; RES, RESC = CAP(REGS[A](UNPACK(CA, 1, NA))); if C == 0 then for i = 1, RESC do REGS[A+i-1] = RES[i] end; TOP = A + RESC - 1 else for i = 1, C - 1 do REGS[A+i-1] = RES[i] end; TOP = A + (C - 1) - 1 end",
        [37] = "local NA = 0; local CA = {}; if B == 0 then for i = A + 1, TOP do NA = NA + 1; CA[NA] = REGS[i] end else for i = 1, B - 1 do NA = NA + 1; CA[NA] = REGS[A+i] end end; return REGS[A](UNPACK(CA, 1, NA))",
        [38] = "local RET, n_ret = {}, 0; if B == 0 then for i = A, TOP do n_ret = n_ret + 1; RET[n_ret] = REGS[i] end else for i = 1, B - 1 do n_ret = n_ret + 1; RET[n_ret] = REGS[A+i-1] end end; return UNPACK(RET, 1, n_ret)",
        [39] = "local STEP, LIM = REGS[A+2], REGS[A+1]; local IDX = REGS[A]+STEP; if (STEP>0 and IDX<=LIM) or (STEP<0 and IDX>=LIM) then PC=PC+SBX; REGS[A]=IDX; REGS[A+3]=IDX end",
        [40] = "REGS[A] = REGS[A] - REGS[A+2]; PC = PC + SBX",
        [41] = "local RES, RESC = CAP(REGS[A](REGS[A+1], REGS[A+2])); for i=1,C do REGS[A+3+i-1]=RES[i] end",
        [42] = "if REGS[A+1] ~= nil then REGS[A]=REGS[A+1]; PC=PC+SBX end",
        [43] = "local N, CV = B, C; if N==0 then N=TOP-A end; if CV==0 then CV=(CODE[PC]>>6)&0x3FFFFFF; PC=PC+1 end; local OFF=(CV-1)*50; for i=1,N do REGS[A][OFF+i]=REGS[A+i] end",
        [44] = "local NP, NU = CP.protos[BX+1], {}; for i=1,#NP.upvalues do local UD = NP.upvalues[i]; if UD.instack then local found = false; for _, ENTRY in pairs(OPENUPS) do if ENTRY.idx == UD.idx and not ENTRY.nup.closed then NU[i] = ENTRY.prx; found = true; break end end; if not found then local nup = {v=nil, closed=false}; local prx = setmetatable({}, {__index=function(_,k) if k=='v' then return nup.closed and nup.v or REGS[UD.idx] end end, __newindex=function(_,k,v) if k=='v' then if nup.closed then nup.v=v else REGS[UD.idx]=v end end end}); NU[i]=prx; table.insert(OPENUPS, {idx=UD.idx, nup=nup, prx=prx}) end else NU[i]=CU[UD.idx+1] end end; REGS[A] = WRAP(NP, NU)",
        [45] = "local N, n_va = B-1, #VA; if N==-1 then for i=1,n_va do REGS[A+i-1]=VA[i] end; TOP=A+n_va-1 else for i=1,N do REGS[A+i-1]=VA[i] end; TOP=A+N-1 end",
        [46] = "PC = PC",
    }

    local function build_dispatcher(ops_list, indent)
        if #ops_list == 1 then
            local op = ops_list[1]
            local logic = opcodes[op]:gsub("[%a_][%w_]*", function(m) return reps[m] or m end)
            return indent .. logic .. "\n"
        end
        local mid = math.ceil(#ops_list / 2)
        local left = {}
        for i=1, mid do left[i] = ops_list[i] end
        local right = {}
        for i=mid+1, #ops_list do right[i-mid] = ops_list[i] end

        local s = indent .. "if " .. reps.STATE .. " <= " .. op_to_state[ops_list[mid]] .. " then\n"
        s = s .. build_dispatcher(left, indent .. "    ")
        s = s .. indent .. "else\n"
        s = s .. build_dispatcher(right, indent .. "    ")
        s = s .. indent .. "end\n"
        return s
    end

    local sorted_ops = {}
    for op in pairs(opcodes) do table.insert(sorted_ops, op) end
    table.sort(sorted_ops, function(a, b) return op_to_state[a] < op_to_state[b] end)
    local dispatcher = build_dispatcher(sorted_ops, "                ")

    local vm_template = [=[
local function OUTER(P, U)
    local function WRAP(CP, CU)
        return function(...)
            local CODE, K = CP.code, CP.constants
            local PC, REGS, TOP, OPENUPS = 1, {}, 0, {}
            local function CAP(...) return {...}, select('#', ...) end
            local ARGS, N_ARGS = CAP(...)
            for i = 0, CP.num_params - 1 do REGS[i] = ARGS[i+1] end
            local VA, n_va = {}, 0
            if CP.is_vararg ~= 0 then for i = CP.num_params + 1, N_ARGS do n_va = n_va + 1; VA[n_va] = ARGS[i] end end
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
                local STATE = STATE_MAP[OP]
                if STATE then
]=] .. dispatcher .. [=[
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
        local n = 0
        for k, v in pairs(t) do
            local key = type(k) == "number" and "["..k.."]" or "['"..tostring(k).."']"
            s = s .. key .. "=" .. serialize(v) .. ","
            n = n + 1
            if n % 100 == 0 then s = s .. "\n" end
        end
        return s .. "}"
    end

    local output = "local " .. reps.STATE_MAP .. " = " .. serialize(op_to_state) .. "\n"
    output = output .. vm_template .. "\n"
    output = output .. "local proto = " .. serialize(obf_proto) .. "\n"
    output = output .. "return " .. reps.OUTER .. "(proto, {{v=_ENV or _G}})()\n"
    return output
end

return Obfuscator
