local engine = {}
local parser = require "obfuscator.parser"
local compiler = require "obfuscator.compiler"
local transformer = require "obfuscator.transformer"
local utils = require "obfuscator.utils"

local function serialize(o)
    if type(o) == "number" then return tostring(o)
    elseif type(o) == "string" then return string.format("%q", o)
    elseif type(o) == "boolean" then return tostring(o)
    elseif type(o) == "table" then
        local s = "{"
        for k, v in pairs(o) do
            local key = type(k) == "number" and "[" .. k .. "]" or "[\"" .. k .. "\"]"
            s = s .. key .. "=" .. serialize(v) .. ","
        end
        return s .. "}"
    else return "nil" end
end

local function encrypt_to_table(data, key)
    local res = {}
    for i = 1, #data do
        local b = data:byte(i)
        local k = (key + i) % 256
        res[i] = b ~ k
    end
    return res
end

function engine.obfuscate(source, config)
    config = config or {
        hash_ids = true,
        cff = true,
        virtualize = true
    }

    local ast = parser.parse(source)
    if config.hash_ids then transformer.hash_identifiers(ast) end
    if config.cff then transformer.apply_cff_recursive(ast) end

    if config.virtualize then
        local proto = compiler.compile(ast)
        local proto_str = "return " .. serialize(proto)
        local key = math.random(1, 255)
        local encrypted_proto_tab = encrypt_to_table(proto_str, key)

        local xor_mba = "((b | m) - (b & m))"
        local opaque_true = utils.generate_opaque_predicate(true)

        local vm_template = [===[
local function _D(d, k)
    local r = {}
    for i = 1, #d do
        local b = d[i]
        local m = (k + i) % 256
        r[i] = string.char({XOR_MBA})
    end
    return table.concat(r)
end

local function _V(p, e, u, ...)
    e = e or _G
    local c = p.code
    local k = p.constants
    local pc = 1
    local r = {}
    local args = {...}
    for i = 1, p.num_params do r[i - 1] = args[i] end
    local varg = {}
    if p.is_vararg then
        for i = p.num_params + 1, #args do table.insert(varg, args[i]) end
    end
    local open_uv = {}
    local last_ret = {}

    while true do
        local inst = c[pc]
        if not inst then break end
        if ({OPAQUE_TRUE} or (pc < 0)) then
            local o = inst.op
            local a, b, cr = inst.a, inst.b, inst.c

            if o == 1 then r[a] = k[b + 1]
            elseif o == 2 then r[a] = nil
            elseif o == 3 then r[a] = r[b]
            elseif o == 4 then r[a] = e[k[b + 1]]
            elseif o == 5 then e[k[a + 1]] = r[b]
            elseif o == 6 then r[a] = r[b][((inst.c_is_k and k[cr + 1]) or r[cr])]
            elseif o == 7 then r[a][((inst.b_is_k and k[b + 1]) or r[b])] = ((inst.c_is_k and k[cr + 1]) or r[cr])
            elseif o == 8 then r[a] = {}
            elseif o == 10 then r[a] = (r[b] + r[cr])
            elseif o == 11 then r[a] = (r[b] - r[cr])
            elseif o == 12 then r[a] = (r[b] * r[cr])
            elseif o == 13 then r[a] = (r[b] / r[cr])
            elseif o == 14 then r[a] = (r[b] % r[cr])
            elseif o == 15 then r[a] = (r[b] ^ r[cr])
            elseif o == 19 then r[a] = (r[b] .. r[cr])
            elseif o == 29 then r[a] = (r[b] < r[cr])
            elseif o == 30 then r[a] = (r[b] <= r[cr])
            elseif o == 31 then r[a] = (r[b] == r[cr])
            elseif o == 17 then r[a] = (not r[b])
            elseif o == 16 then r[a] = (- r[b])
            elseif o == 18 then r[a] = (# r[b])
            elseif o == 34 then r[a] = (r[b] & r[cr])
            elseif o == 35 then r[a] = (r[b] | r[cr])
            elseif o == 36 then r[a] = (r[b] ~ r[cr])
            elseif o == 37 then r[a] = (r[b] << r[cr])
            elseif o == 38 then r[a] = (r[b] >> r[cr])
            elseif o == 39 then r[a] = (~ r[b])
            elseif o == 32 then r[a] = u[b + 1][1]
            elseif o == 33 then u[a + 1][1] = r[b]
            elseif o == 20 then pc = (pc + a)
            elseif o == 21 then if (not r[a]) then pc = (pc + b) end
            elseif o == 22 then if r[a] then pc = (pc + b) end
            elseif o == 23 then
                local func = r[a]
                local ca = {}
                if (b == 0) then
                    for j = 1, #last_ret do table.insert(ca, last_ret[j]) end
                else
                    for j = 1, (b - 1) do table.insert(ca, r[(a + j)]) end
                end
                local ret = {func(table.unpack(ca))}
                last_ret = ret
                if (cr == 0) then
                    -- Keep all results in last_ret
                elseif (cr > 1) then
                    for j = 1, (cr - 1) do r[((a + j) - 1)] = ret[j] end
                end
            elseif o == 24 then
                local res = {}
                if (b == 0) then
                    res = last_ret
                else
                    for j = 1, (b - 1) do table.insert(res, r[((a + j) - 1)]) end
                end
                return table.unpack(res)
            elseif o == 26 then
                local sub = p.protos[(b + 1)]
                local uv = {}
                for j, info in ipairs(sub.upvalues) do
                    if info.is_local then
                        if (not open_uv[info.index]) then open_uv[info.index] = {r[info.index]} end
                        uv[j] = open_uv[info.index]
                    else uv[j] = u[info.index + 1] end
                end
                r[a] = function(...)
                    for idx, tab in pairs(open_uv) do tab[1] = r[idx] end
                    local res = { _V(sub, e, uv, ...) }
                    for idx, tab in pairs(open_uv) do r[idx] = tab[1] end
                    return table.unpack(res)
                end
            elseif o == 25 then
                if (b == 0) then
                    r[a] = varg -- Return as table for simpler handling if b=0
                else
                    for j = 1, (b - 1) do r[((a + j) - 1)] = varg[j] end
                end
            end
            pc = (pc + 1)
        else
            pc = (pc - 1)
            if (pc < -100) then break end
        end
    end
end

local P = load(_D(__PROTO_DATA_MARKER__, {KEY}))()
return _V(P, ...)
]===]
        local final_code = vm_template:gsub("{XOR_MBA}", function() return xor_mba end)
                                      :gsub("{OPAQUE_TRUE}", function() return opaque_true end)
                                      :gsub("{KEY}", function() return tostring(key) end)
                                      :gsub("__PROTO_DATA_MARKER__", function() return serialize(encrypted_proto_tab) end)
        return final_code
    else
        return transformer.to_lua(ast)
    end
end

return engine
