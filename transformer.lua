local Transformer = {}

function Transformer.random_id()
    return math.random(1000, 9999)
end

function Transformer.obfuscate(proto)
    local op_map = {}
    local rev_map = {}
    local state_map = {}

    local ops = {}
    for i = 0, 46 do ops[i+1] = i end
    -- Shuffle
    for i = #ops, 2, -1 do
        local j = math.random(i)
        ops[i], ops[j] = ops[j], ops[i]
    end

    for i = 0, 46 do
        local new_op = ops[i+1]
        op_map[i] = new_op
        rev_map[new_op] = i
        state_map[i] = Transformer.random_id()
    end

    local function encrypt(p)
        for _, k in ipairs(p.constants) do
            if k.type == "string" then
                local s = k.value
                local key = math.random(1, 255)
                local enc = {}
                for j = 1, #s do enc[j] = string.byte(s, j) ~ key end
                k.type = "enc_string"
                k.value = {enc = enc, key = key}
            elseif k.type == "integer" then
                local key = math.random(1, 1000000)
                k.type = "enc_integer"
                k.value = {val = k.value ~ key, key = key}
            end
        end
        for i = 1, #p.code do
            local inst = p.code[i]
            local op = inst & 0x3F
            local rest = inst & ~0x3F
            p.code[i] = rest | op_map[op]
        end
        p.source = "=[obfuscated]"
        p.line_defined = 0
        p.last_line_defined = 0
        for _, sub in ipairs(p.protos) do encrypt(sub) end
    end
    encrypt(proto)

    return proto, rev_map, state_map
end

return Transformer
