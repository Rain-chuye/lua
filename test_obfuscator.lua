local Obfuscator = require("obfuscator_engine")

local source = [[
local function 斐波那契(n)
    if n <= 1 then return n end
    return 斐波那契(n - 1) + 斐波那契(n - 2)
end

local function 最终测试(a, b)
    print("正在计算...")
    local result = a + b
    print("计算完毕")
    return result
end

print("斐波那契(10) =", 斐波那契(10))
print("100 + 200 =", 最终测试(100, 200))
]]

print("--- OBFUSCATING ---")
local status, result = pcall(Obfuscator.obfuscate, source)
if not status then
    print("Obfuscation error:", result)
    os.exit(1)
end

print("--- SAVING ---")
local f = io.open("obfuscated_test.lua", "w")
f:write(result)
f:close()
print("Saved to obfuscated_test.lua")

print("--- EXECUTION ---")
local f_exec = load(result)
if not f_exec then
    print("Load error!")
    os.exit(1)
end

local status_exec, err = pcall(f_exec)
if not status_exec then
    print("Execution error:", err)
else
    print("Execution Success")
end
