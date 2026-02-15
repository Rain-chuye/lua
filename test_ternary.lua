-- Test script for ternary operator and Chinese error messages
local a = true ? 1 : 2
print("true ? 1 : 2 =", a)
assert(a == 1)

local b = false ? 1 : 2
print("false ? 1 : 2 =", b)
assert(b == 2)

local c = true ? (false ? 3 : 4) : 5
print("true ? (false ? 3 : 4) : 5 =", c)
assert(c == 4)

local d = false ? 3 : (true ? 4 : 5)
print("false ? 3 : (true ? 4 : 5) =", d)
assert(d == 4)

-- Test short-circuiting
local side_effect = 0
local function set_side_effect(val)
    side_effect = val
    return val
end

local e = true ? 10 : set_side_effect(20)
print("true ? 10 : set_side_effect(20) =", e)
assert(e == 10)
assert(side_effect == 0)

local f = false ? set_side_effect(30) : 40
print("false ? set_side_effect(30) : 40 =", f)
assert(f == 40)
assert(side_effect == 0)

-- Test with standard operators
local g = (10 > 5) ? "yes" : "no"
print("(10 > 5) ? 'yes' : 'no' =", g)
assert(g == "yes")

-- Test precedence (lower than OR)
local h = false or true ? "passed" : "failed"
print("false or true ? 'passed' : 'failed' =", h)
assert(h == "passed")

print("All ternary tests passed!")

-- Test translated error messages
print("\nTesting error messages:")
local tests = {
    { name = "Arithmetic on nil", func = function() return 1 + nil end },
    { name = "Table index nil", func = function() local t = {}; t[nil] = 1 end },
    { name = "Call nil", func = function() local f; f() end },
    { name = "Index nil", func = function() local t; return t.foo end },
}

for _, t in ipairs(tests) do
    local status, err = pcall(t.func)
    print(t.name .. ":", err)
end
