local parser = {}

local patterns = {
    white = "^[ \t\n\r]+",
    comment_long = "^%-%-%[(=*)%[.-%]%1%]",
    comment_short = "^%-%-[^\n]*",
    ident = "^[%a_][%w_]*",
}

local number_patterns = {
    "^0x[%da-fA-F]+%.?[%da-fA-F]*[pP][%+%-]?%d+",
    "^%d+%.?%d*[eE][%+%-]?%d+",
    "^%.?%d+[eE][%+%-]?%d+",
    "^0x[%da-fA-F]+%.?[%da-fA-F]*",
    "^%d+%.?%d*",
    "^%.%d+"
}

local multi_ops = {
    "%.%.%.", "%.%.", "==", "~=", "<=", ">=", "<<", ">>", "//", "::"
}

local single_ops = "[%+%-%.%*%/%%%^#&~|<>=%(%)%{%}%[%];:,]"

local keywords = {
    ["and"] = true, ["break"] = true, ["do"] = true, ["else"] = true, ["elseif"] = true,
    ["end"] = true, ["false"] = true, ["for"] = true, ["function"] = true, ["goto"] = true,
    ["if"] = true, ["in"] = true, ["local"] = true, ["nil"] = true, ["not"] = true,
    ["or"] = true, ["repeat"] = true, ["return"] = true, ["then"] = true, ["true"] = true,
    ["until"] = true, ["while"] = true
}

function parser.lex(code)
    local tokens = {}
    local pos = 1
    while pos <= #code do
        local sub = code:sub(pos)
        local matched = false

        local w = sub:match(patterns.white)
        if w then pos = pos + #w; matched = true end

        if not matched then
            local c = sub:match(patterns.comment_long) or sub:match(patterns.comment_short)
            if c then pos = pos + #c; matched = true end
        end

        if not matched then
            local q = sub:sub(1,1)
            if q == '"' or q == "'" then
                local s_pos = 2
                while s_pos <= #sub do
                    local char = sub:sub(s_pos, s_pos)
                    if char == "\\" then
                        s_pos = s_pos + 2
                    elseif char == q then
                        local full_s = sub:sub(1, s_pos)
                        table.insert(tokens, {type = "string", value = full_s})
                        pos = pos + #full_s; matched = true
                        break
                    else
                        s_pos = s_pos + 1
                    end
                end
            elseif sub:match("^%[=*%[") then
                local eq = sub:match("^%[(=*)%[")
                local close = "]" .. eq .. "]"
                local _, end_p = sub:find(close, 1, true)
                if end_p then
                    local full_s = sub:sub(1, end_p)
                    table.insert(tokens, {type = "string", value = full_s})
                    pos = pos + #full_s; matched = true
                end
            end
        end

        if not matched then
            local id = sub:match(patterns.ident)
            if id then
                if keywords[id] then
                    table.insert(tokens, {type = "keyword", value = id})
                else
                    table.insert(tokens, {type = "ident", value = id})
                end
                pos = pos + #id; matched = true
            end
        end

        if not matched then
            for _, pat in ipairs(number_patterns) do
                local n = sub:match(pat)
                if n then
                    table.insert(tokens, {type = "number", value = n})
                    pos = pos + #n; matched = true
                    break
                end
            end
        end

        if not matched then
            for _, op_pat in ipairs(multi_ops) do
                local m = sub:match("^" .. op_pat)
                if m then
                    table.insert(tokens, {type = "op", value = m})
                    pos = pos + #m; matched = true
                    break
                end
            end
        end

        if not matched then
            local op = sub:match("^" .. single_ops)
            if op then
                table.insert(tokens, {type = "op", value = op})
                pos = pos + #op; matched = true
            end
        end

        if not matched and pos <= #code then
            error("Unexpected character at " .. pos .. ": " .. code:sub(pos, pos))
        end
    end
    table.insert(tokens, {type = "eof", value = "eof"})
    return tokens
end

function parser.parse(code)
    local tokens = parser.lex(code)
    local index = 1

    local function peek(n) return tokens[index + (n or 0)] end
    local function consume() local t = tokens[index]; index = index + 1; return t end
    local function expect(type, value)
        local t = peek()
        if not t or t.type ~= type or (value and t.value ~= value) then
            error("Expected " .. type .. " '" .. (value or "") .. "' but got " .. (t and t.type or "EOF") .. " '" .. (t and t.value or "") .. "' at index " .. index)
        end
        return consume()
    end

    local parse_exp, parse_stmt, parse_block

    local prec = {
        ["or"] = 1, ["and"] = 2, ["<"] = 3, [">"] = 3, ["<="] = 3, [">="] = 3, ["~="] = 3, ["=="] = 3,
        ["|"] = 4, ["~"] = 5, ["&"] = 6, ["<<"] = 7, [">>"] = 7, [".."] = 8, ["+"] = 9, ["-"] = 9,
        ["*"] = 10, ["/"] = 10, ["//"] = 10, ["%"] = 10, ["^"] = 12,
    }

    local function parse_params()
        expect("op", "(")
        local params = {}
        if peek().type ~= "op" or peek().value ~= ")" then
            repeat
                if peek().type == "op" and peek().value == "..." then
                    table.insert(params, consume().value)
                    break
                end
                table.insert(params, expect("ident").value)
            until not (peek().type == "op" and peek().value == "," and consume())
        end
        expect("op", ")")
        return params
    end

    local function parse_primary()
        local t = consume()
        if t.type == "number" then return {type = "Number", value = tonumber(t.value)}
        elseif t.type == "string" then
            local v = t.value
            if v:sub(1,1) == "[" then
                v = v:match("^%[=*%[(.*)%]=*%]$")
            else
                v = v:sub(2, -2) -- Strip quotes
                -- Handle basic escapes?
                v = v:gsub("\\n", "\n"):gsub("\\t", "\t"):gsub("\\\"", "\""):gsub("\\'", "'"):gsub("\\\\", "\\")
            end
            return {type = "String", value = v}
        elseif t.type == "op" and t.value == "..." then return {type = "Vararg"}
        elseif t.type == "keyword" then
            if t.value == "nil" then return {type = "Nil"}
            elseif t.value == "true" then return {type = "Boolean", value = true}
            elseif t.value == "false" then return {type = "Boolean", value = false}
            elseif t.value == "function" then
                local params = parse_params()
                local body = parse_block()
                expect("keyword", "end")
                return {type = "Function", params = params, body = body}
            end
        elseif t.type == "ident" then
            local node = {type = "Identifier", name = t.value}
            while true do
                local p = peek()
                if p.type == "op" then
                    if p.value == "." then
                        consume()
                        node = {type = "Member", base = node, property = expect("ident").value}
                    elseif p.value == "[" then
                        consume()
                        local key = parse_exp()
                        expect("op", "]")
                        node = {type = "Index", base = node, index = key}
                    elseif p.value == "(" or p.value == "{" or p.type == "string" then
                        local args = {}
                        if p.value == "(" then
                            consume()
                            if peek().value ~= ")" then
                                repeat table.insert(args, parse_exp()) until not (peek().value == "," and consume())
                            end
                            expect("op", ")")
                        elseif p.value == "{" then
                            table.insert(args, parse_exp())
                        else
                            table.insert(args, {type = "String", value = consume().value})
                        end
                        node = {type = "Call", base = node, args = args}
                    elseif p.value == ":" then
                        consume()
                        local method = expect("ident").value
                        local args = {}
                        local ap = peek()
                        if ap.value == "(" then
                            consume()
                            if peek().value ~= ")" then
                                repeat table.insert(args, parse_exp()) until not (peek().value == "," and consume())
                            end
                            expect("op", ")")
                        elseif ap.value == "{" then
                            table.insert(args, parse_exp())
                        else
                            table.insert(args, {type = "String", value = consume().value})
                        end
                        node = {type = "MethodCall", base = node, method = method, args = args}
                    else break end
                else break end
            end
            return node
        elseif t.type == "op" and t.value == "(" then
            local e = parse_exp()
            expect("op", ")")
            return e
        elseif t.type == "op" and t.value == "{" then
            local fields = {}
            if peek().value ~= "}" then
                repeat
                    if peek().type == "op" and peek().value == "[" then
                        consume()
                        local k = parse_exp()
                        expect("op", "]")
                        expect("op", "=")
                        local v = parse_exp()
                        table.insert(fields, {key = k, value = v})
                    elseif peek().type == "ident" and peek(1).value == "=" then
                        local k = consume().value
                        consume() -- =
                        local v = parse_exp()
                        table.insert(fields, {key = {type = "String", value = k}, value = v})
                    else
                        table.insert(fields, {value = parse_exp()})
                    end
                    if peek().value == "," or peek().value == ";" then consume() end
                until peek().value == "}"
            end
            expect("op", "}")
            return {type = "Table", fields = fields}
        end
        error("Unexpected token in expression: " .. t.type .. " " .. t.value)
    end

    local function parse_unary()
        local t = peek()
        if t.type == "op" and (t.value == "-" or t.value == "#" or t.value == "~") then
            consume()
            return {type = "Unary", op = t.value, argument = parse_unary()}
        elseif t.type == "keyword" and t.value == "not" then
            consume()
            return {type = "Unary", op = "not", argument = parse_unary()}
        end
        return parse_primary()
    end

    function parse_exp(min_prec)
        min_prec = min_prec or 0
        local node = parse_unary()
        while true do
            local t = peek()
            if t.type == "op" or (t.type == "keyword" and (t.value == "and" or t.value == "or")) then
                local p = prec[t.value]
                if p and p > min_prec then
                    consume()
                    local right = parse_exp(p)
                    node = {type = "Binary", op = t.value, left = node, right = right}
                else break end
            else break end
        end
        return node
    end

    function parse_stmt()
        local t = peek()
        if t.type == "keyword" then
            if t.value == "local" then
                consume()
                if peek().value == "function" then
                    consume()
                    local name = expect("ident").value
                    local params = parse_params()
                    local body = parse_block()
                    expect("keyword", "end")
                    return {type = "LocalFunction", name = name, params = params, body = body}
                end
                local names = {}
                repeat table.insert(names, expect("ident").value) until not (peek().value == "," and consume())
                local values = {}
                if peek().value == "=" then
                    consume()
                    repeat table.insert(values, parse_exp()) until not (peek().value == "," and consume())
                end
                return {type = "LocalAssign", names = names, values = values}
            elseif t.value == "if" then
                consume()
                local condition = parse_exp()
                expect("keyword", "then")
                local then_block = parse_block()
                local else_ifs = {}
                while peek().value == "elseif" do
                    consume()
                    local c = parse_exp()
                    expect("keyword", "then")
                    table.insert(else_ifs, {condition = c, block = parse_block()})
                end
                local else_block
                if peek().value == "else" then
                    consume()
                    else_block = parse_block()
                end
                expect("keyword", "end")
                return {type = "If", condition = condition, then_block = then_block, else_ifs = else_ifs, else_block = else_block}
            elseif t.value == "while" then
                consume()
                local condition = parse_exp()
                expect("keyword", "do")
                local body = parse_block()
                expect("keyword", "end")
                return {type = "While", condition = condition, body = body}
            elseif t.value == "for" then
                consume()
                local name = expect("ident").value
                if peek().value == "=" then
                    consume()
                    local start = parse_exp()
                    expect("op", ",")
                    local stop = parse_exp()
                    local step
                    if peek().value == "," then
                        consume()
                        step = parse_exp()
                    end
                    expect("keyword", "do")
                    local body = parse_block()
                    expect("keyword", "end")
                    return {type = "ForRange", name = name, start = start, stop = stop, step = step, body = body}
                else
                    local names = {name}
                    while peek().value == "," do consume(); table.insert(names, expect("ident").value) end
                    expect("keyword", "in")
                    local exps = {}
                    repeat table.insert(exps, parse_exp()) until not (peek().value == "," and consume())
                    expect("keyword", "do")
                    local body = parse_block()
                    expect("keyword", "end")
                    return {type = "ForIn", names = names, expressions = exps, body = body}
                end
            elseif t.value == "return" then
                consume()
                local values = {}
                if peek().type ~= "keyword" and peek().type ~= "eof" and peek().value ~= "end" and peek().value ~= "else" and peek().value ~= "elseif" and peek().value ~= "until" then
                    repeat table.insert(values, parse_exp()) until not (peek().value == "," and consume())
                end
                return {type = "Return", values = values}
            elseif t.value == "break" then
                consume()
                return {type = "Break"}
            elseif t.value == "goto" then
                consume()
                return {type = "Goto", label = expect("ident").value}
            elseif t.value == "do" then
                consume()
                local body = parse_block()
                expect("keyword", "end")
                return {type = "Do", body = body}
            end
        elseif t.type == "op" and t.value == "::" then
            consume()
            local label = expect("ident").value
            expect("op", "::")
            return {type = "Label", name = label}
        end

        local left = parse_exp()
        if peek().value == "=" or peek().value == "," then
            local targets = {left}
            while peek().value == "," do consume(); table.insert(targets, parse_exp()) end
            expect("op", "=")
            local values = {}
            repeat table.insert(values, parse_exp()) until not (peek().value == "," and consume())
            return {type = "Assignment", targets = targets, values = values}
        end
        return {type = "ExpressionStatement", expression = left}
    end

    function parse_block()
        local body = {}
        while peek().type ~= "eof" and peek().value ~= "end" and peek().value ~= "else" and peek().value ~= "elseif" and peek().value ~= "until" do
            if peek().value == ";" then consume() else
                table.insert(body, parse_stmt())
            end
        end
        return body
    end

    local ast = {type = "Chunk", body = parse_block()}
    return ast
end

return parser
