local Obfuscator = require("main")

local source = [[
$a = [1, 2, 3]
function add(x, y)
  return x + y
end
print("Result: " .. add(a[1], a[2]))
if add(1, 1) == 2 then
  print("Math works!")
end
defer print("Program exiting...")
]]

local options = {
  int_rate = 0.8,
  junk_rate = 0.1,
  identify_deps = true,
}

print("--- Original Source ---")
print(source)
print("--- Obfuscating ---")

local success, obfuscated = pcall(Obfuscator.obfuscate, source, options)

if success then
  print("--- Obfuscated Code ---")
  print(obfuscated)

  -- Test running the obfuscated code
  print("--- Execution Result ---")
  local f, err = load(obfuscated)
  if f then
    f()
  else
    print("Load Error: " .. err)
  end
else
  print("Obfuscation Failed: " .. obfuscated)
end
