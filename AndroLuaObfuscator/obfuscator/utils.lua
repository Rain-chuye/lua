local utils = {}

function utils.hash_name(name)
    local h = 5381
    for i = 1, #name do
        h = ((h << 5) + h) + string.byte(name, i)
    end
    return string.format("v_%x", h & 0xFFFFFFFF)
end

-- MBA (Mixed Boolean-Arithmetic) generators
utils.mba = {}

function utils.mba.generate_add(x, y)
    local choices = {
        string.format("((%s ~ %s) + 2 * (%s & %s))", x, y, x, y),
        string.format("((%s | %s) + (%s & %s))", x, y, x, y),
        string.format("(2 * (%s | %s) - (%s ~ %s))", x, y, x, y)
    }
    return choices[math.random(1, #choices)]
end

function utils.mba.generate_xor(x, y)
    local choices = {
        string.format("((%s | %s) - (%s & %s))", x, y, x, y),
        string.format("((%s + %s) - 2 * (%s & %s))", x, y, x, y)
    }
    return choices[math.random(1, #choices)]
end

-- Opaque Predicates
function utils.generate_opaque_predicate(is_true)
    local x = math.random(1, 1000)
    local y = math.random(1, 1000)

    if is_true then
        local true_exprs = {
            string.format("((%d * %d + %d) %% 2 == 0)", x, x, x),
            string.format("((%d %% 3 == 0) or (%d %% 3 == 1) or (%d %% 3 == 2))", x, x, x),
            string.format("((%d & %d) <= %d)", x, y, x)
        }
        return true_exprs[math.random(1, #true_exprs)]
    else
        local false_exprs = {
            string.format("((%d * %d + %d) %% 2 != 0)", x, x, x),
            string.format("((%d & %d) > %d)", x, y, x),
            string.format("((%d + 1) == %d)", x, x)
        }
        return false_exprs[math.random(1, #false_exprs)]
    end
end

return utils
