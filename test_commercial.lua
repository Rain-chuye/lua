local Obfuscator = require("obfuscator_engine")
local source = [[
local function 最终测试(a, b)
    print("正在计算...")
    return a + b
end
print("结果是:", 最终测试(100, 200))
]]
local obf = Obfuscator.obfuscate(source)
print("--- GENERATED SOURCE ---")
print(obf)
print("\n--- EXECUTION ---")
local f, err = load(obf)
if not f then print("Load error:", err) else f() end
