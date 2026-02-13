local Obfuscator = {}

-- [[ Helper: Bytecode Parser ]]
local Parser = {}
function Parser.new(dump)
    local self = {}
    local cursor = 1
    local function rb() local b = dump:byte(cursor); cursor = cursor + 1; return b end
    local function rbs(n) local s = dump:sub(cursor, cursor + n - 1); cursor = cursor + n; return s end
    local sizet_size, int_size, inst_size, lua_int_size, lua_num_size
    local function rstr()
        local b = rb()
        if b == 0 then return nil end
        local size = b
        if size == 0xFF then
            size = (sizet_size == 4) and string.unpack("<I4", rbs(4)) or string.unpack("<I8", rbs(8))
        end
        return rbs(size - 1)
    end
    function self.parse()
        local sig = rbs(4)
        if sig ~= "\27Lua" then error("Invalid bytecode signature") end
        local version = rb(); local format = rb(); rbs(6)
        int_size = rb(); sizet_size = rb(); inst_size = rb()
        lua_int_size = rb(); lua_num_size = rb()
        rbs(lua_int_size); rbs(lua_num_size)
        local function rproto()
            local p = {}
            p.source = rstr()
            p.line_defined = string.unpack("<i4", rbs(4))
            p.last_line_defined = string.unpack("<i4", rbs(4))
            p.num_params = rb()
            p.is_vararg = rb()
            p.max_stack_size = rb()
            local size_code = string.unpack("<i4", rbs(4))
            p.code = {}
            for i = 1, size_code do p.code[i] = string.unpack("<I4", rbs(4)) end
            local size_k = string.unpack("<i4", rbs(4))
            p.constants = {}
            for i = 1, size_k do
                local t = rb()
                if t == 0 then p.constants[i] = {type = "nil", value = nil}
                elseif t == 1 then p.constants[i] = {type = "boolean", value = rb() ~= 0}
                elseif t == 3 then p.constants[i] = {type = "float", value = string.unpack("<d", rbs(8))}
                elseif t == 19 then p.constants[i] = {type = "integer", value = string.unpack("<j", rbs(8))}
                elseif t == 4 or t == 20 then p.constants[i] = {type = "string", value = rstr()}
                else error("Unknown constant type: " .. t) end
            end
            local size_upvalues = string.unpack("<i4", rbs(4))
            p.upvalues = {}
            for i = 1, size_upvalues do p.upvalues[i] = { instack = rb() ~= 0, idx = rb() } end
            local size_protos = string.unpack("<i4", rbs(4))
            p.protos = {}
            for i = 1, size_protos do p.protos[i] = rproto() end
            local size_lineinfo = string.unpack("<i4", rbs(4))
            cursor = cursor + size_lineinfo * 4
            local size_locvars = string.unpack("<i4", rbs(4))
            for i = 1, size_locvars do rstr(); rbs(8) end
            local size_upvalue_names = string.unpack("<i4", rbs(4))
            for i = 1, size_upvalue_names do rstr() end
            return p
        end
        rb()
        return rproto()
    end
    return self
end

-- [[ Helper: Transformer ]]
local Transformer = {}
function Transformer.obfuscate(proto)
    local op_map, rev_map, state_map = {}, {}, {}
    local ops = {}
    for i = 0, 63 do ops[i+1] = i end
    for i = #ops, 2, -1 do local j = math.random(i); ops[i], ops[j] = ops[j], ops[i] end
    for i = 0, 63 do
        local new_op = ops[i+1]
        op_map[i] = new_op
        rev_map[new_op] = i
        state_map[i] = math.random(100000, 999999)
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
            p.code[i] = rest | op_map[op]
        end
        p.source = "=[obfuscated]"; p.line_defined = 0; p.last_line_defined = 0
        for _, sub in ipairs(p.protos) do crypt(sub) end
    end
    crypt(proto)
    return proto, rev_map, state_map
end

-- [[ Main API ]]
function Obfuscator.obfuscate(source, intensity)
    intensity = intensity or 2
    local ok, func = pcall(load, source)
    if not ok or not func then error("Failed to load source: " .. tostring(func)) end
    local dump = string.dump(func)
    local p = Parser.new(dump).parse()

    local d_test = string.dump(load("local a; a=a"))
    local pt = Parser.new(d_test).parse()
    if (pt.code[1] & 0x3F) ~= 0 then
        local op_decode = {[0]=27,[1]=47,[2]=24,[3]=3,[4]=14,[5]=37,[6]=6,[7]=29,[8]=42,[9]=32,[10]=10,[11]=4,[12]=13,[13]=8,[14]=26,[15]=33,[16]=40,[17]=30,[18]=22,[19]=35,[20]=39,[21]=43,[22]=46,[23]=2,[24]=23,[25]=48,[26]=49,[27]=36,[28]=34,[29]=5,[30]=7,[31]=28,[32]=15,[33]=0,[34]=18,[35]=1,[36]=31,[37]=38,[38]=11,[39]=16,[40]=17,[41]=45,[42]=44,[43]=19,[44]=12,[45]=9,[46]=20,[47]=21,[48]=41,[49]=25}
        local function norm(pr)
            for i=1,#pr.code do local inst = pr.code[i]; local op = inst & 0x3F; if op_decode[op] then pr.code[i] = (inst & ~0x3F) | op_decode[op] end end
            for _, s in ipairs(pr.protos) do norm(s) end
        end
        norm(p)
    end

    local obf_p, rev, states = Transformer.obfuscate(p)

    local var_cache = {}
    local function v(name)
        if intensity < 2 then return name end
        if not var_cache[name] then
            local chars = "abcdefghijklmnopqrstuvwxyz"
            local s = ""
            for i = 1, 10 do local r = math.random(#chars); s = s .. chars:sub(r, r) end
            var_cache[name] = s
        end
        return var_cache[name]
    end

    local function serialize(t)
        local s = "{"
        for k, val in pairs(t) do
            local key = type(k) == "number" and "[" .. k .. "]" or "['" .. tostring(k) .. "']"
            if type(val) == "table" then s = s .. key .. "=" .. serialize(val) .. ","
            elseif type(val) == "string" then s = s .. key .. "=" .. string.format("%q", val) .. ","
            else s = s .. key .. "=" .. tostring(val) .. "," end
        end
        return s .. "}"
    end

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
        [29] = "local s = REGS[B]; for i = B + 1, C do s = s .. REGS[i] end; REGS[A] = s",
        [30] = "PC = PC + SBX; if A ~= 0 then for i, entry in pairs(OPENUPS) do if entry.index >= A - 1 then entry.nup.v = REGS[entry.index]; entry.nup.closed = true; OPENUPS[i] = nil end end end",
        [31] = "if (GETRK(B) == GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [32] = "if (GETRK(B) < GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [33] = "if (GETRK(B) <= GETRK(C)) ~= (A ~= 0) then PC = PC + 1 end",
        [34] = "if (not REGS[A]) == (C ~= 0) then PC = PC + 1 end",
        [35] = "if (not REGS[B]) ~= (C ~= 0) then REGS[A] = REGS[B] else PC = PC + 1 end",
        [36] = "local f = REGS[A]; local np, nr = B-1, C-1; local ca = {}; if np == -1 then for i=A+1,TOP do ca[#ca+1]=REGS[i] end else for i=1,np do ca[i]=REGS[A+i] end end; local res, resc; local function cap(...) res={...}; resc=select('#', ...) end; cap(f(UNPACK(ca))); if nr == -1 then for i=1,resc do REGS[A+i-1]=res[i] end; TOP=A+resc-1 else for i=1,nr do REGS[A+i-1]=res[i] end; TOP=A+nr-1 end",
        [37] = "local f = REGS[A]; local np = B-1; local ca = {}; for i=1,np do ca[i]=REGS[A+i] end; return f(UNPACK(ca))",
        [38] = "local nr = B-1; local ret = {}; if nr == -1 then for i=A,TOP do ret[#ret+1]=REGS[i] end else for i=1,nr do ret[i]=REGS[A+i-1] end end; return UNPACK(ret)",
        [39] = "local step = REGS[A+2]; local idx = REGS[A]+step; local lim = REGS[A+1]; if (step>0 and idx<=lim) or (step<0 and idx>=lim) then PC=PC+SBX; REGS[A]=idx; REGS[A+3]=idx end",
        [40] = "REGS[A] = REGS[A] - REGS[A+2]; PC = PC + SBX",
        [41] = "local r = {REGS[A](REGS[A+1], REGS[A+2])}; for i=1,C do REGS[A+3+i-1]=r[i] end",
        [42] = "if REGS[A+1] ~= nil then REGS[A]=REGS[A+1]; PC=PC+SBX end",
        [43] = "local n, cv = B, C; if n==0 then n=TOP-A end; if cv==0 then cv=(CODE[PC]>>6)&0x3FFFFFF; PC=PC+1 end; local off=(cv-1)*50; for i=1,n do REGS[A][off+i]=REGS[A+i] end",
        [44] = "local np = CP.protos[BX+1]; local nu = {}; for i=1,#np.upvalues do local ud = np.upvalues[i]; if ud.instack then local f = false; for _, entry in pairs(OPENUPS) do if entry.index == ud.idx and not entry.nup.closed then nu[i] = entry.proxy; f = true; break end end; if not f then local nup = {v=nil, closed=false}; local prx = setmetatable({}, {__index=function(_,k) if k=='v' then return nup.closed and nup.v or REGS[ud.idx] end end, __newindex=function(_,k,v) if k=='v' then if nup.closed then nup.v=v else REGS[ud.idx]=v end end end}); nu[i]=prx; table.insert(OPENUPS, {index=ud.idx, nup=nup, proxy=prx}) end else nu[i]=CU[ud.idx+1] end end; REGS[A] = WRAP(np, CE, nu)",
        [45] = "local n = B-1; if n==-1 then for i=1,#VA do REGS[A+i-1]=VA[i] end; TOP=A+#VA-1 else for i=1,n do REGS[A+i-1]=VA[i] end; TOP=A+n-1 end",
        [46] = "PC = PC",
    }

    local logic_names = {"REGS", "A", "B", "C", "BX", "CODE", "PC", "GETRK", "CU", "TOP", "UNPACK", "STATE", "SBX", "OPENUPS", "CP", "WRAP", "CE", "VA", "P", "E", "U", "K", "ARGS", "I", "OP", "RUNNING", "OUTER", "REALOP", "rev_map", "state_map"}
    local reps = {}
    for _, n in ipairs(logic_names) do reps[n] = v(n) end

    local op_indices = {}
    for op in pairs(opcodes) do table.insert(op_indices, op) end
    for i = #op_indices, 2, -1 do local j = math.random(i); op_indices[i], op_indices[j] = op_indices[j], op_indices[i] end

    local branches = ""
    local first = true
    for _, op in ipairs(op_indices) do
        local logic = opcodes[op]
        local rl = logic:gsub("[%a_][%w_]*", function(m) return reps[m] or m end)
        branches = branches .. "            " .. (first and "if" or "elseif") .. " " .. reps.STATE .. " == " .. states[op] .. " then\n"
        branches = branches .. "                " .. rl .. "\n"
        if op ~= 37 and op ~= 38 then
            branches = branches .. "                " .. reps.STATE .. " = -1\n"
        end
        first = false
    end
    branches = branches .. "            end\n"

    local vm_template = [=[
local function OUTER(P, E, U)
    local function WRAP(CP, CE, CU)
        return function(...)
            local CODE, K = CP.code, CP.constants
            local PC, REGS, TOP = 1, {}, 0
            local OPENUPS = {}
            local ARGS = {...}
            for i = 0, CP.num_params - 1 do REGS[i] = ARGS[i+1] end
            local VA = {}
            if CP.is_vararg ~= 0 then
                for i = CP.num_params + 1, #ARGS do VA[#VA+1] = ARGS[i] end
            end
            local function GETRK(val)
                if val >= 256 then
                    local kv = K[val-255]
                    if kv.type == "enc_string" then
                        if not kv.cache then
                            local s = ""
                            for _, b in ipairs(kv.value.enc) do s = s .. string.char(b ~ kv.value.key) end
                            kv.cache = s
                        end
                        return kv.cache
                    elseif kv.type == "enc_integer" then
                        return kv.value.val ~ kv.value.key
                    end
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
                local REALOP = rev_map[OP]
                local STATE = state_map[REALOP]
                while STATE ~= -1 do
]=] .. branches .. [=[
                end
            end
        end
    end
    return WRAP(P, E, U)
end
]=]

    vm_template = vm_template:gsub("[%a_][%w_]*", function(m) return reps[m] or m end)

    local output = "local " .. reps.rev_map .. " = " .. serialize(rev) .. "\n"
    output = output .. "local " .. reps.state_map .. " = " .. serialize(states) .. "\n"
    output = output .. vm_template .. "\n"
    output = output .. string.format("return %s(proto, _G, {{v=_G}})()\n", reps.OUTER)
    output = "local proto = " .. serialize(obf_p) .. "\n" .. output
    return output
end

return Obfuscator
