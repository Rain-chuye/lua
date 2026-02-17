local Obfuscator = require("obfuscator_engine")
local source = [[
local function 斐波那契(n)
    if n < 2 then return n end
    return 斐波那契(n - 1) + 斐波那契(n - 2)
end
print("斐波那契(10) =", 斐波那契(10))
]]
local obf = Obfuscator.obfuscate(source)
print("--- GENERATED SOURCE (PREVIEW) ---")
print(obf:sub(1, 1000))
print("\n--- EXECUTION ---")
local f, err = load(obf)
if not f then print("Load error:", err) else f() end
