-- Final test script for ternary operator and shorthand if compatibility
print("Testing ternary operator...")
local a = true ? 1 : 2
print("true ? 1 : 2 =", a)
assert(a == 1)

local b = false ? 1 : 2
print("false ? 1 : 2 =", b)
assert(b == 2)

-- Test names disambiguation
local x = 10
local y = 20
local c = true ? x : y
print("true ? x : y =", c)
assert(c == 10)

-- Test shorthand if compatibility (newline heuristic)
local d = 0
if true then
    d = 1
end
-- The newline before '?' prevents it from being parsed as ternary
? d == 1 then
    print("Shorthand if works (condition true)")
`   print("Shorthand if works (condition false)")

print("All tests passed!")
