local Parser = {}

function Parser.new(dump)
    local self = {}
    local cursor = 1

    local function read_byte()
        local b = string.byte(dump, cursor)
        cursor = cursor + 1
        return b
    end

    local function read_bytes(n)
        local s = string.sub(dump, cursor, cursor + n - 1)
        cursor = cursor + n
        return s
    end

    local sizet_size, int_size, inst_size, lua_int_size, lua_num_size

    local function read_string()
        local size = read_byte()
        if size == 0xFF then
            if sizet_size == 4 then
                size = string.unpack("<I4", read_bytes(4))
            else
                size = string.unpack("<I8", read_bytes(8))
            end
        end
        if size == 0 then return nil end
        return read_bytes(size - 1)
    end

    function self.parse()
        local sig = read_bytes(4)
        if sig ~= "\27Lua" then error("Invalid signature") end
        local version = read_byte()
        local format = read_byte()
        local data = read_bytes(6)
        int_size = read_byte()
        sizet_size = read_byte()
        inst_size = read_byte()
        lua_int_size = read_byte()
        lua_num_size = read_byte()

        -- Check constants
        read_bytes(lua_int_size) -- lua_Integer check
        read_bytes(lua_num_size) -- lua_Number check

        local function read_proto()
            local p = {}
            p.source = read_string()
            p.line_defined = string.unpack("<i4", read_bytes(4))
            p.last_line_defined = string.unpack("<i4", read_bytes(4))
            p.num_params = read_byte()
            p.is_vararg = read_byte()
            p.max_stack_size = read_byte()

            -- Code
            local size_code = string.unpack("<i4", read_bytes(4))
            p.code = {}
            for i = 1, size_code do
                p.code[i] = string.unpack("<I4", read_bytes(4))
            end

            -- Constants
            local size_k = string.unpack("<i4", read_bytes(4))
            p.constants = {}
            for i = 1, size_k do
                local t = read_byte()
                if t == 0 then -- Nil
                    p.constants[i] = {type = "nil", value = nil}
                elseif t == 1 then -- Boolean
                    p.constants[i] = {type = "boolean", value = read_byte() ~= 0}
                elseif t == 3 then -- Float
                    p.constants[i] = {type = "float", value = string.unpack("<d", read_bytes(8))}
                elseif t == 19 then -- Integer
                    p.constants[i] = {type = "integer", value = string.unpack("<j", read_bytes(8))}
                elseif t == 4 or t == 20 then -- String
                    p.constants[i] = {type = "string", value = read_string()}
                else
                    error("Unknown constant type: " .. t)
                end
            end

            -- Upvalues
            local size_upvalues = string.unpack("<i4", read_bytes(4))
            p.upvalues = {}
            for i = 1, size_upvalues do
                p.upvalues[i] = { instack = read_byte() ~= 0, idx = read_byte() }
            end

            -- Protos
            local size_protos = string.unpack("<i4", read_bytes(4))
            p.protos = {}
            for i = 1, size_protos do p.protos[i] = read_proto() end

            -- Debug Info
            local size_lineinfo = string.unpack("<i4", read_bytes(4))
            cursor = cursor + size_lineinfo * 4
            local size_locvars = string.unpack("<i4", read_bytes(4))
            for i = 1, size_locvars do
                read_string() -- varname
                read_bytes(8) -- startpc, endpc
            end
            local size_upvalue_names = string.unpack("<i4", read_bytes(4))
            for i = 1, size_upvalue_names do read_string() end

            return p
        end

        local upvalues_count = read_byte()
        return read_proto()
    end

    return self
end

return Parser
