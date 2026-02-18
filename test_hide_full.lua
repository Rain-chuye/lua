local s = "hello world"
local function test()
    print("Testing hidden libraries...")

    -- String
    local up = s:upper()
    print("Upper:", up)

    -- Math
    local r = math.sqrt(16)
    print("Sqrt 16:", r)

    -- Table
    local t = {3, 1, 2}
    table.sort(t)
    print("Sorted table:", t[1], t[2], t[3])

    -- Coroutine
    local co = coroutine.create(function()
        coroutine.yield("co-result")
    end)
    local _, res = coroutine.resume(co)
    print("Coroutine yield:", res)

    -- Global environment check
    if string == nil then
        print("SUCCESS: 'string' is nil in global env")
    else
        print("FAILURE: 'string' is STILL visible")
    end

    if math == nil then
        print("SUCCESS: 'math' is nil in global env")
    end

    -- Pcall check (xpcall might be used by debuggers)
    local status, err = pcall(function() return string.upper(s) end)
    if not status then
        print("SUCCESS: Direct global access failed as expected:", err)
    end
end

test()
