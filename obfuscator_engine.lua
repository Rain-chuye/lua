math.randomseed(math.floor(os.time() + os.clock()))
local ops_list = { "DEFER", "LOADK", "GETVAR", "SETVAR", "GETVAR_G", "SETVAR_G", "GETTABLE", "SETTABLE", "NEWTABLE", "CALL", "RET", "ADD", "SUB", "MUL", "DIV", "IDIV", "MOD", "POW", "BAND", "BOR", "BXOR", "SHL", "SHR", "EQ", "LT", "LE", "CONCAT", "NOT", "UNM", "BNOT", "LEN", "CLOSURE", "LOAD_VA", "LOAD_VARARG", "PICK_RESULT", "POP", "DUP", "SWAP", "JMP", "JMP_IF_FALSE" }

local Lexer = {}
function Lexer.new(source) return setmetatable({ source = source, pos = 1, tokens = {}, line = 1 }, { __index = Lexer }) end
function Lexer:peek(n) n = n or 0; return self.source:sub(self.pos + n, self.pos + n) end
function Lexer:consume() local c = self:peek(); self.pos = self.pos + 1; if c == "\n" then self.line = self.line + 1 end; return c end
function Lexer:tokenize()
    while self.pos <= #self.source do
        local c = self:peek()
        if c:match("%s") then self:consume()
        elseif c == "-" and self:peek(1) == "-" then
            self:consume(); self:consume()
            if self:peek() == "[" then
                local s = self.pos; self:consume()
                local sep = ""
                while self:peek() == "=" do sep = sep .. self:consume() end
                if self:peek() == "[" then
                    self:consume()
                    local close = "]" .. sep .. "]"
                    while self.pos <= #self.source do
                        if self.source:sub(self.pos, self.pos + #close - 1) == close then
                            for _=1, #close do self:consume() end; break
                        else self:consume() end
                    end
                else
                    while self.pos <= #self.source and self:peek() ~= "\n" do self:consume() end
                end
            else
                while self.pos <= #self.source and self:peek() ~= "\n" do self:consume() end
            end
        elseif c == "[" and (self:peek(1) == "[" or self:peek(1) == "=") then
            local s = self.pos; self:consume()
            local sep = ""
            while self:peek() == "=" do sep = sep .. self:consume() end
            if self:peek() == "[" then
                self:consume()
                local content = {}; local close = "]" .. sep .. "]"
                while self.pos <= #self.source do
                    if self.source:sub(self.pos, self.pos + #close - 1) == close then
                        for _=1, #close do self:consume() end; break
                    else table.insert(content, self:consume()) end
                end
                table.insert(self.tokens, { type = "string", value = table.concat(content), line = self.line })
            else
                table.insert(self.tokens, { type = "operator", value = "[", line = self.line })
                for _=1, #sep do table.insert(self.tokens, { type = "operator", value = "=", line = self.line }); self:consume() end
            end
        elseif c == "$" then
            self:consume(); local s = self.pos; while self:peek():match("[%w_]") or (self:peek() ~= "" and self:peek():byte() > 127) do self:consume() end
            local v = self.source:sub(s, self.pos - 1)
            table.insert(self.tokens, { type = "keyword", value = "local", line = self.line })
            table.insert(self.tokens, { type = "name", value = v, line = self.line })
        elseif c:match("[%a_]") or c:byte() > 127 then
            local s = self.pos; while self:peek():match("[%w_]") or (self:peek() ~= "" and self:peek():byte() > 127) do self:consume() end
            local v = self.source:sub(s, self.pos - 1)
            local keywords = { ["local"]=1, ["function"]=1, ["return"]=1, ["if"]=1, ["then"]=1, ["else"]=1, ["elseif"]=1, ["end"]=1, ["while"]=1, ["do"]=1, ["repeat"]=1, ["until"]=1, ["for"]=1, ["in"] = 1, ["break"]=1, ["nil"]=1, ["true"]=1, ["false"]=1, ["not"]=1, ["and"]=1, ["or"]=1, ["goto"]=1, ["continue"]=1, ["default"]=1, ["defer"]=1, ["when"]=1, ["case"]=1, ["switch"]=1, ["lambda"]=1 }
            table.insert(self.tokens, { type = keywords[v] and "keyword" or "name", value = v, line = self.line })
        elseif c:match("%d") then
            local s = self.pos
            if c == "0" and self:peek(1):lower() == "x" then
                self:consume(); self:consume()
                while self:peek():match("[%da-fA-F]") do self:consume() end
            else
                while self:peek():match("%d") or self:peek() == "." do self:consume() end
                if self:peek():lower() == "e" then
                    self:consume()
                    if self:peek() == "+" or self:peek() == "-" then self:consume() end
                    while self:peek():match("%d") do self:consume() end
                end
            end
            table.insert(self.tokens, { type = "number", value = self.source:sub(s, self.pos - 1), line = self.line })
        elseif c == "'" or c == "\"" then
            local q = self:consume(); local res = {}
            while self.pos <= #self.source and self:peek() ~= q do
                local nc = self:consume()
                if nc == "\\" then
                    local esc = self:consume()
                    if esc == "n" then table.insert(res, "\n")
                    elseif esc == "r" then table.insert(res, "\r")
                    elseif esc == "t" then table.insert(res, "\t")
                    elseif esc == "\\" then table.insert(res, "\\")
                    elseif esc == q then table.insert(res, q)
                    elseif esc:match("%d") then
                        local d = esc; while self:peek():match("%d") and #d < 3 do d = d .. self:consume() end
                        table.insert(res, string.char(tonumber(d)))
                    else table.insert(res, nc) end
                else table.insert(res, nc) end
            end
            self:consume(); table.insert(self.tokens, { type = "string", value = table.concat(res), line = self.line })
        elseif c:match("[%p]") then
            local tri = self.source:sub(self.pos, self.pos + 2)
            local duo = self.source:sub(self.pos, self.pos + 1)
            if tri == "..." then table.insert(self.tokens, { type = "operator", value = "...", line = self.line }); self.pos = self.pos + 3
            elseif duo == "==" or duo == "~=" or duo == "!=" or duo == "<=" or duo == ">=" or duo == ".." or duo == "<<" or duo == ">>" or duo == "//" then
                local val = duo; if val == "!=" then val = "~=" end
                table.insert(self.tokens, { type = "operator", value = val, line = self.line }); self.pos = self.pos + 2
            else
                local val = self:consume()
                if val == "!" then val = "not" end
                table.insert(self.tokens, { type = "operator", value = val, line = self.line })
            end
        else self:consume() end
    end
    table.insert(self.tokens, { type = "eof", value = "eof", line = self.line }); return self.tokens
end

local Parser = {}
function Parser.new(tokens) return setmetatable({ tokens = tokens, pos = 1 }, { __index = Parser }) end
function Parser:peek(n) n = n or 0; return self.tokens[self.pos + n] end
function Parser:consume() local t = self:peek(); self.pos = self.pos + 1; return t end
function Parser:expect(v) local t = self:consume(); if not t or (t.value ~= v and t.type ~= v) then error("Line " .. (t and t.line or "unknown") .. ": expected " .. v .. " but got " .. (t and t.value or "nil")) end; return t end
function Parser:parse() return self:parseBlock() end
function Parser:parseBlock()
    local body = {}
    while self:peek().type ~= "eof" and not ({ ["end"]=1, ["else"]=1, ["elseif"]=1, ["until"]=1, ["case"]=1, ["when"]=1, ["default"]=1 })[self:peek().value] do
        if self:peek().value == ";" then self:consume() else table.insert(body, self:parseStatement()) end
    end
    return { type = "Block", body = body }
end
function Parser:parseStatement()
    local t = self:peek()
    if t.value == "local" then
        self:consume()
        if self:peek().value == "function" then
            self:consume(); local name = self:expect("name").value; local args = self:parseArgs(); local body = self:parseBlock(); self:expect("end")
            return { type = "LocalFunction", name = name, args = args, body = body }
        else
            local vars = {}
            while true do
                if self:peek().value == "*" then self:consume() end
                table.insert(vars, self:expect("name").value)
                if self:peek().value == "," then self:consume() else break end
            end
            local values = {}
            if self:peek().value == "=" then self:consume(); table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end end
            return { type = "LocalAssign", vars = vars, values = values }
        end
    elseif t.value == "return" then
        self:consume(); local values = {}
        if not ({ ["end"]=1, ["else"]=1, ["elseif"]=1, ["until"]=1, ["eof"]=1 })[self:peek().value] then table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end end
        return { type = "Return", values = values }
    elseif t.value == "if" then
        self:consume(); local cond = self:parseExpr(); if self:peek().value == "then" then self:consume() end; local body = self:parseBlock(); local eifs = {}
        while self:peek().value == "elseif" do self:consume(); local c = self:parseExpr(); if self:peek().value == "then" then self:consume() end; table.insert(eifs, { cond = c, body = self:parseBlock() }) end
        local eb; if self:peek().value == "else" then self:consume(); eb = self:parseBlock() end
        self:expect("end"); return { type = "If", cond = cond, body = body, elseifs = eifs, elseBlock = eb }
    elseif t.value == "while" then self:consume(); local cond = self:parseExpr(); if self:peek().value == "do" then self:consume() end; local body = self:parseBlock(); self:expect("end"); return { type = "While", cond = cond, body = body }
    elseif t.value == "defer" then self:consume(); local body = self:parseBlock(); self:expect("end"); return { type = "Defer", body = body }
    elseif t.value == "continue" then self:consume(); return { type = "Continue" }
    elseif t.value == "break" then self:consume(); return { type = "Break" }
    elseif t.value == "switch" then
        self:consume(); local expr = self:parseExpr(); local cases = {}
        while self:peek().value == "case" or self:peek().value == "when" do
            self:consume(); local val = self:parseExpr(); if self:peek().value == "then" then self:consume() end; table.insert(cases, { val = val, body = self:parseBlock() })
        end
        local def; if self:peek().value == "default" then self:consume(); def = self:parseBlock() end
        self:expect("end"); return { type = "Switch", expr = expr, cases = cases, ["default"] = def }
    elseif t.value == "for" then
        self:consume(); local name = self:expect("name").value
        if self:peek().value == "=" then
            self:consume(); local init = self:parseExpr(); self:expect(","); local limit = self:parseExpr()
            local step = { type = "Number", value = 1 }; if self:peek().value == "," then self:consume() ; step = self:parseExpr() end
            if self:peek().value == "do" then self:consume() end; local body = self:parseBlock(); self:expect("end")
            return { type = "ForRange", var = name, init = init, limit = limit, step = step, body = body }
        else
            local names = { name }; while self:peek().value == "," do self:consume(); table.insert(names, self:expect("name").value) end
            if self:peek().value == "in" then self:consume() end; local iter = self:parseExpr(); if self:peek().value == "do" then self:consume() end; local body = self:parseBlock(); self:expect("end")
            return { type = "ForIn", vars = names, iter = iter, body = body }
        end
    elseif t.value == "repeat" then
        self:consume(); local body = self:parseBlock(); self:expect("until"); local cond = self:parseExpr(); return { type = "Repeat", body = body, cond = cond }
    elseif t.value == "function" then
        self:consume(); local name = self:expect("name").value; local node = { type = "Var", name = name }
        while self:peek().value == "." or self:peek().value == ":" do
            local sep = self:consume().value; local kn = self:expect("name").value
            node = { type = "Index", table = node, key = { type = "String", value = kn } }
            if sep == ":" then node.is_method = true; break end
        end
        local args = self:parseArgs()
        if node.is_method then table.insert(args, 1, "self") end
        local body = self:parseBlock(); self:expect("end")
        return { type = "Assign", vars = { node }, values = { { type = "FunctionDef", args = args, body = body } } }
    else
        local p = self:parsePrimaryExpr()
        if self:peek().value == "=" or self:peek().value == "," then
            local vars = { p }; while self:peek().value == "," do self:consume(); table.insert(vars, self:parsePrimaryExpr()) end
            self:expect("="); local values = { self:parseExpr() }; while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end
            return { type = "Assign", vars = vars, values = values }
        else return p end
    end
end
function Parser:parseArgs()
    if self:peek().value ~= "(" then return {} end
    self:expect("("); local args = {}
    while self:peek().value ~= ")" do
        if self:peek().value == "..." then table.insert(args, self:consume().value); break end
        table.insert(args, self:expect("name").value)
        if self:peek().value == "," then self:consume() else break end
    end
    self:expect(")"); return args
end
function Parser:parseExpr() return self:parseOr() end
function Parser:parseOr() local node = self:parseAnd(); while self:peek().value == "or" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseAnd() } end; return node end
function Parser:parseAnd() local node = self:parseCompare(); while self:peek().value == "and" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseCompare() } end; return node end
function Parser:parseCompare() local node = self:parseBitOr(); local ops = { ["=="]=1, ["~="]=1, ["<"]=1, [">"]=1, ["<="]=1, [">="]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitOr() } end; return node end
function Parser:parseBitOr() local node = self:parseBitXor(); while self:peek().value == "|" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitXor() } end; return node end
function Parser:parseBitXor() local node = self:parseBitAnd(); while self:peek().value == "~" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitAnd() } end; return node end
function Parser:parseBitAnd() local node = self:parseBitShift(); while self:peek().value == "&" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitShift() } end; return node end
function Parser:parseBitShift() local node = self:parseConcat(); while self:peek().value == "<<" or self:peek().value == ">>" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseConcat() local node = self:parseAdd(); if self:peek().value == ".." then local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseAdd() local node = self:parseMul(); while self:peek().value == "+" or self:peek().value == "-" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseMul() } end; return node end
function Parser:parseMul() local node = self:parseUnary(); local ops = { ["*"]=1, ["/"]=1, ["//"]=1, ["%"]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseUnary() } end; return node end
function Parser:parseUnary() local ops = { ["not"]="not", ["-"]="-", ["~"]="~", ["#"]="#" }; if ops[self:peek().value] then local op = self:consume().value; return { type = "UnaryOp", op = op, right = self:parseUnary() } end; return self:parsePow() end
function Parser:parsePow() local node = self:parsePrimaryExpr(); if self:peek().value == "^" then local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parsePow() } end; return node end
function Parser:parsePrimaryExpr()
    local tk = self:peek(); local node
    if tk.type == "number" then node = { type = "Number", value = tonumber(self:consume().value) }
    elseif tk.type == "string" then node = { type = "String", value = self:consume().value }
    elseif tk.value == "nil" then self:consume(); node = { type = "Nil" }
    elseif tk.value == "true" or tk.value == "false" then node = { type = "Boolean", value = self:consume().value == "true" }
    elseif tk.value == "(" then self:consume(); node = self:parseExpr(); self:expect(")")
    elseif tk.value == "{" then
        self:consume(); local fields = {}
        while self:peek().value ~= "}" do
            local field = {}
            if (self:peek().type == "name" or self:peek().type == "keyword") and self:peek(1).value == "=" then
                field.key = { type = "String", value = self:consume().value }; self:consume(); field.value = self:parseExpr()
            elseif self:peek().value == "[" then
                self:consume(); field.key = self:parseExpr(); self:expect("]"); self:expect("="); field.value = self:parseExpr()
            else field.value = self:parseExpr() end
            table.insert(fields, field); if self:peek().value == "," or self:peek().value == ";" then self:consume() end
        end; self:expect("}"); node = { type = "Table", fields = fields }
    elseif tk.value == "[" then
        self:consume(); local vals = {}; if self:peek().value ~= "]" then
            table.insert(vals, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(vals, self:parseExpr()) end
        end; self:expect("]"); node = { type = "Table", values = vals, is_array = true }
    elseif tk.value == "lambda" then
        self:consume(); local args = {}
        if self:peek().value == "(" then args = self:parseArgs()
        elseif self:peek().type == "name" then table.insert(args, self:consume().value) end
        local body; if self:peek().value == ":" then self:consume(); body = { type = "Block", body = { { type = "Return", values = { self:parseExpr() } } } }
        else body = self:parseBlock(); self:expect("end") end; node = { type = "FunctionDef", args = args, body = body }
    elseif tk.value == "..." then self:consume(); node = { type = "Vararg" }
    elseif tk.type == "name" or tk.type == "keyword" then node = { type = "Var", name = self:consume().value }
    else error("Line " .. (tk and tk.line or "unknown") .. ": unexpected token " .. (tk and tk.value or "nil")) end
    while true do
        local t = self:peek()
        if t.value == "." then self:consume(); local key = self:consume().value; node = { type = "Index", table = node, key = { type = "String", value = key } }
        elseif t.value == "[" then self:consume(); node = { type = "Index", table = node, key = self:parseExpr() }; self:expect("]")
        elseif t.value == ":" then self:consume(); local n = self:expect("name").value; local a = self:parseCallArgs(); node = { type = "MemberCall", table = node, member = n, args = a }
        elseif t.value == "(" or t.type == "string" or t.value == "{" then node = { type = "Call", func = node, args = self:parseCallArgs() }
        else break end
    end; return node
end
function Parser:parseCallArgs()
    local t = self:peek()
    if t.value == "(" then
        self:consume(); local args = {}
        if self:peek().value ~= ")" then table.insert(args, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(args, self:parseExpr()) end end
        self:expect(")"); return args
    elseif t.type == "string" then return { { type = "String", value = self:consume().value } }
    elseif t.value == "{" then return { self:parsePrimaryExpr() }
    else error("Expected call arguments") end
end

local Obfuscator = {}
function Obfuscator.deepCopy(n)
    if type(n) ~= "table" then return n end
    local r = {}
    for k, v in pairs(n) do r[k] = Obfuscator.deepCopy(v) end
    return r
end

function Obfuscator.applyMBA(ast)
    local function walk(n, depth)
        if not n or type(n) ~= "table" or depth > 8 then return n end
        if n.type == "BinaryOp" then
            local l, r = walk(n.left, depth + 1), walk(n.right, depth + 1)
            local op = n.op
            if op == "+" and math.random() > 0.3 then
                if math.random() > 0.5 then
                    return walk({ type = "BinaryOp", op = "+",
                        left = { type = "BinaryOp", op = "~", left = l, right = r },
                        right = { type = "BinaryOp", op = "*", left = { type = "Number", value = 2 }, right = { type = "BinaryOp", op = "&", left = Obfuscator.deepCopy(l), right = Obfuscator.deepCopy(r) } }
                    }, depth + 1)
                else
                    return walk({ type = "BinaryOp", op = "+",
                        left = { type = "BinaryOp", op = "|", left = l, right = r },
                        right = { type = "BinaryOp", op = "&", left = Obfuscator.deepCopy(l), right = Obfuscator.deepCopy(r) }
                    }, depth + 1)
                end
            elseif op == "-" and math.random() > 0.3 then
                local bnot_r = { type = "UnaryOp", op = "~", right = r }
                return walk({ type = "BinaryOp", op = "+", left = l, right = { type = "BinaryOp", op = "+", left = bnot_r, right = { type = "Number", value = 1 } } }, depth + 1)
            elseif op == "~" and math.random() > 0.3 then
                return walk({ type = "BinaryOp", op = "-",
                    left = { type = "BinaryOp", op = "|", left = l, right = r },
                    right = { type = "BinaryOp", op = "&", left = Obfuscator.deepCopy(l), right = Obfuscator.deepCopy(r) }
                }, depth + 1)
            elseif op == "&" and math.random() > 0.3 then
                local x_xor_y = { type = "BinaryOp", op = "~", left = l, right = r }
                local sum = { type = "BinaryOp", op = "+", left = Obfuscator.deepCopy(l), right = Obfuscator.deepCopy(r) }
                return walk({ type = "BinaryOp", op = "//", left = { type = "BinaryOp", op = "-", left = sum, right = x_xor_y }, right = { type = "Number", value = 2 } }, depth + 1)
            end
        elseif n.type == "UnaryOp" and n.op == "~" and math.random() > 0.3 then
            return walk({ type = "BinaryOp", op = "-", left = { type = "UnaryOp", op = "-", right = walk(n.right, depth + 1) }, right = { type = "Number", value = 1 } }, depth + 1)
        end
        for k, v in pairs(n) do if type(v) == "table" and k ~= "left" and k ~= "right" then n[k] = walk(v, depth) end end
        return n
    end
    return walk(ast, 0)
end

function Obfuscator.desugar(ast)
    local function walk(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v) end
        elseif n.type == "If" then
            n.cond = walk(n.cond); n.body = walk(n.body)
            if n.elseifs and #n.elseifs > 0 then
                local curr_else = n.elseBlock
                for i = #n.elseifs, 1, -1 do
                    local ei = n.elseifs[i]
                    curr_else = { type = "Block", body = { { type = "If", cond = walk(ei.cond), body = walk(ei.body), elseifs = {}, elseBlock = curr_else } } }
                end
                n.elseBlock = curr_else; n.elseifs = {}
            end
            if n.elseBlock then n.elseBlock = walk(n.elseBlock) end
        elseif n.type == "While" then n.cond = walk(n.cond); n.body = walk(n.body)
        elseif n.type == "ForRange" then
            local var = n.var; local init = walk(n.init); local limit = walk(n.limit); local step = walk(n.step); local body = walk(n.body)
            local increment = { type = "Assign", vars = { {type="Var", name=var} }, values = { {type="BinaryOp", op="+", left={type="Var", name=var}, right=step} } }
            local while_body = { type = "Block", body = { body, { type = "ContinueTarget" }, increment } }
            return walk({ type = "Block", body = { { type = "LocalAssign", vars = { var }, values = { init } }, { type = "While", cond = { type = "BinaryOp", op = "<=", left = { type = "Var", name = var }, right = limit }, body = while_body } } })
        elseif n.type == "ForIn" then
            local vars = n.vars; local iter = walk(n.iter); local body = walk(n.body)
            local fn = string.format("_0xF%X", math.random(0x100, 0xFFF))
            local sn = string.format("_0xS%X", math.random(0x100, 0xFFF))
            local vn = string.format("_0xV%X", math.random(0x100, 0xFFF))
            local kn = string.format("_0xK%X", math.random(0x100, 0xFFF))
            local setup = { type = "LocalAssign", vars = { fn, sn, vn, kn }, values = { iter, { type = "Boolean", value = true } } }
            local while_body = { type = "Block", body = { { type = "LocalAssign", vars = vars, values = { { type = "Call", func = { type = "Var", name = fn }, args = { { type = "Var", name = sn }, { type = "Var", name = vn } } } } } } }
            local if_break = { type = "If", cond = { type = "BinaryOp", op = "==", left = { type = "Var", name = vars[1] }, right = { type = "Nil" } }, body = { type = "Block", body = { { type = "Assign", vars = { { type = "Var", name = kn } }, values = { { type = "Boolean", value = false } } } } }, elseBlock = { type = "Block", body = {} }, elseifs = {} }
            table.insert(if_break.elseBlock.body, { type = "Assign", vars = { { type = "Var", name = vn } }, values = { { type = "Var", name = vars[1] } } })
            for _, s in ipairs(body.body) do table.insert(if_break.elseBlock.body, s) end
            table.insert(if_break.elseBlock.body, { type = "ContinueTarget" })
            table.insert(while_body.body, if_break)
            return walk({ type = "Block", body = { setup, { type = "While", cond = { type = "Var", name = kn }, body = while_body } } })
        elseif n.type == "Switch" then
            local sn = string.format("_0xS%X", math.random(0x100, 0xFFF))
            local expr = walk(n.expr); local cases = n.cases; local def = n["default"]
            local setup = { type = "LocalAssign", vars = { sn }, values = { expr } }
            local if_node = nil; for i, c in ipairs(cases) do
                local cond = { type = "BinaryOp", op = "==", left = { type = "Var", name = sn }, right = walk(c.val) }
                if not if_node then if_node = { type = "If", cond = cond, body = walk(c.body), elseifs = {}, elseBlock = nil }
                else table.insert(if_node.elseifs, { cond = cond, body = walk(c.body) }) end
            end
            if def and if_node then if_node.elseBlock = walk(def) end
            return walk({ type = "Block", body = { setup, if_node or (def and walk(def) or nil) } })
        elseif n.type == "LocalFunction" then return walk({ type = "LocalAssign", vars = { n.name }, values = { { type = "FunctionDef", args = n.args, body = n.body } }, is_recursive = true })
        elseif n.type == "FunctionDef" or n.type == "Lambda" then
            if n.type == "Lambda" then n = { type = "FunctionDef", args = n.args, body = { type = "Block", body = { { type = "Return", values = { n.body } } } } } end
            n.body = walk(n.body); return n
        elseif n.type == "Call" or n.type == "MemberCall" then if n.func then n.func = walk(n.func) end; if n.table then n.table = walk(n.table) end; for i, v in ipairs(n.args) do n.args[i] = walk(v) end
        elseif n.type == "Assign" or n.type == "LocalAssign" then if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
        elseif n.type == "BinaryOp" then n.left = walk(n.left); n.right = walk(n.right)
        elseif n.type == "UnaryOp" then n.right = walk(n.right)
        elseif n.type == "Index" then n.table = walk(n.table); n.key = walk(n.key)
        elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v) end
        elseif n.type == "Defer" then n.body = walk(n.body)
        end; return n
    end; return walk(ast)
end

function Obfuscator.obfuscateIdentifiers(ast)
    local gm = {}; local gr = {}; local c = 0; local function gnn() c = c + 1; local name = string.format("_0x%X", c + 0xABCDEF); while gr[name] or gm[name] do c = c + 1; name = string.format("_0x%X", c + 0xABCDEF) end; return name end
    local function walk(n, s)
        if not n or type(n) ~= "table" then return end
        if n.type == "Block" then local ns = { parent = s, locals = {} }; for i, v in ipairs(n.body) do walk(v, ns) end
        elseif n.type == "LocalAssign" then
            if n.is_recursive then for i, v in ipairs(n.vars) do local nn = gnn(); s.locals[v] = nn; n.vars[i] = nn end; if n.values then for _, v in ipairs(n.values) do walk(v, s) end end
            else if n.values then for _, v in ipairs(n.values) do walk(v, s) end end; for i, v in ipairs(n.vars) do local nn = gnn(); s.locals[v] = nn; n.vars[i] = nn end end
        elseif n.type == "Var" then local cur = s; local f = false; while cur do if cur.locals[n.name] then n.name = cur.locals[n.name]; f = true; break end; cur = cur.parent end; if not f then if n.name == "_G" or n.name == "_ENV" then return end; if not gr[n.name] then local nn = gnn(); gr[n.name] = nn; gm[nn] = n.name end; n.name = gr[n.name] end
        elseif n.type == "FunctionDef" then local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args or {}) do if v ~= "..." then local nn = gnn(); fs.locals[v] = nn; n.args[i] = nn end end; walk(n.body, fs)
        elseif n.type == "If" then walk(n.cond, s); walk(n.body, s); for _, v in ipairs(n.elseifs or {}) do walk(v.cond, s); walk(v.body, s) end; walk(n.elseBlock, s)
        elseif n.type == "While" then walk(n.cond, s); walk(n.body, s)
        elseif n.type == "ForRange" then walk(n.init, s); walk(n.limit, s); walk(n.step, s); walk(n.body, s)
        elseif n.type == "ForIn" then walk(n.iter, s); walk(n.body, s)
        elseif n.type == "Switch" then walk(n.expr, s); for _, c in ipairs(n.cases) do walk(c.val, s); walk(c.body, s) end; walk(n["default"], s)
        elseif n.type == "Defer" then walk(n.body, s)
        elseif n.type == "Table" then if n.is_array then for _, v in ipairs(n.values) do walk(v, s) end else for _, f in ipairs(n.fields) do walk(f.key, s); walk(f.value, s) end end
        elseif n.type == "Assign" then for _, v in ipairs(n.vars) do walk(v, s) end; for _, v in ipairs(n.values) do walk(v, s) end
        elseif n.type == "Call" or n.type == "MemberCall" then if n.func then walk(n.func, s) end; if n.table then walk(n.table, s) end; for _, v in ipairs(n.args) do walk(v, s) end
        elseif n.type == "BinaryOp" then walk(n.left, s); walk(n.right, s)
        elseif n.type == "UnaryOp" then walk(n.right, s)
        elseif n.type == "Index" then walk(n.table, s); walk(n.key, s)
        elseif n.type == "Return" then for _, v in ipairs(n.values) do walk(v, s) end
        end
    end; walk(ast, { locals = {} }); return gm
end

local Virtualizer = {}
function Virtualizer.virtualize(ast, gm)
    local ops_list = { "DEFER", "LOADK", "GETVAR", "SETVAR", "GETVAR_G", "SETVAR_G", "GETTABLE", "SETTABLE", "NEWTABLE", "CALL", "RET", "ADD", "SUB", "MUL", "DIV", "IDIV", "MOD", "POW", "BAND", "BOR", "BXOR", "SHL", "SHR", "EQ", "LT", "LE", "CONCAT", "NOT", "UNM", "BNOT", "LEN", "CLOSURE", "LOAD_VA", "LOAD_VARARG", "PICK_RESULT", "POP", "DUP", "SWAP", "JMP", "JMP_IF_FALSE" }
    local XK = math.random(1, 0xFFFFFF); local SX1 = math.random(1, 255); local SX2 = math.random(1, 255); local SX3 = math.random(1, 255)
    local function encrypt_k(v)
        local s = (type(v) == "string" and "s" or type(v) == "number" and "n" or type(v) == "boolean" and "b" or "x") .. tostring(v == nil and "" or v == true and "t" or v == false and "f" or v)
        local r = {}; for i=1, #s do
            local b = s:byte(i)
            b = ((b ~ SX1) + SX2) % 256 ~ SX3
            table.insert(r, string.char(b))
        end; return table.concat(r)
    end
    local function pp(block, args)
        local insts = {}; local ks = { n = 0 }; local function addK(v) for i=1, ks.n do if ks[i] == v then return i end end; ks.n = ks.n + 1; ks[ks.n] = v; return ks.n end
        local function emit(op, arg)
            if math.random() < 0.1 then table.insert(insts, { op = "LOADK", arg = addK(math.random()) }); table.insert(insts, { op = "POP", arg = 1 }) end
            table.insert(insts, { op = op, arg = arg }); return #insts
        end
        local locals_map = {}; local next_local = 1
        local function get_local_idx(name) if not locals_map[name] then locals_map[name] = next_local; next_local = next_local + 1 end; return locals_map[name] end
        if args then for i, a in ipairs(args) do if a ~= "..." then emit("LOAD_VA", i); emit("SETVAR", get_local_idx(a)) end end end
        local loop_stack = {}
        local function walk(n, multi, is_stmt)
            if not n then return end
            if n.type == "Block" then for _, v in ipairs(n.body) do walk(v, false, true) end
            elseif n.type == "LocalAssign" or n.type == "Assign" then
                local vs, vals = n.vars or {}, n.values or {}
                local last_is_call = vals[#vals] and (vals[#vals].type == "Call" or vals[#vals].type == "MemberCall")
                if last_is_call and #vs > #vals then
                    for i = 1, #vals - 1 do walk(vals[i]) end
                    walk(vals[#vals], true)
                else
                    for i = 1, #vals do walk(vals[i]) end
                    if #vals < #vs then for i = #vals + 1, #vs do emit("LOADK", addK(nil)) end end
                end
                for i = #vs, 1, -1 do local v = vs[i]
                    if type(v) == "table" and v.type == "Index" then walk(v.table); walk(v.key); emit("SETTABLE", 0)
                    elseif type(v) == "table" and v.type == "Var" then
                        if gm[v.name] then emit("SETVAR_G", addK(v.name)) else emit("SETVAR", get_local_idx(v.name)) end
                    else
                        if gm[v] then emit("SETVAR_G", addK(v)) else emit("SETVAR", get_local_idx(v)) end
                    end
                end
            elseif n.type == "Var" then
                if gm[n.name] then emit("GETVAR_G", addK(n.name)) else emit("GETVAR", get_local_idx(n.name)) end
            elseif n.type == "Vararg" then emit("LOAD_VARARG", 0)
            elseif n.type == "Number" or n.type == "String" or n.type == "Boolean" or n.type == "Nil" then emit("LOADK", addK(n.value))
            elseif n.type == "Call" then
                walk(n.func); for _, v in ipairs(n.args) do walk(v) end;
                local wanted = is_stmt and 0 or (multi and 255 or 1)
                emit("CALL", #n.args + wanted * 256)
            elseif n.type == "MemberCall" then
                walk(n.table); emit("DUP", 0); emit("LOADK", addK(n.member)); emit("GETTABLE", 0); emit("SWAP", 0); for _, v in ipairs(n.args) do walk(v) end;
                local wanted = is_stmt and 0 or (multi and 255 or 1)
                emit("CALL", #n.args + 1 + wanted * 256)
            elseif n.type == "Index" then walk(n.table); walk(n.key); emit("GETTABLE", 0)
            elseif n.type == "Table" then
                emit("NEWTABLE", 0)
                if n.is_array then
                    for i, v in ipairs(n.values) do emit("DUP", 0); emit("LOADK", addK(i)); walk(v); emit("SETTABLE", 0) end
                else
                    local ai = 1; for _, f in ipairs(n.fields) do
                        emit("DUP", 0); if f.key then walk(f.key) else emit("LOADK", addK(ai)); ai = ai + 1 end
                        walk(f.value); emit("SETTABLE", 0)
                    end
                end
            elseif n.type == "Defer" then emit("DEFER", addK(pp(n.body)))
            elseif n.type == "ContinueTarget" then
                local loop = loop_stack[#loop_stack]
                if loop then
                    loop.continue_target = #insts + 1
                    for _, j in ipairs(loop.continue_jumps) do insts[j].arg = loop.continue_target end
                end
            elseif n.type == "Continue" then
                local loop = loop_stack[#loop_stack]
                if not loop then error("continue outside loop") end
                local j = emit("JMP", loop.continue_target or 0)
                if not loop.continue_target then table.insert(loop.continue_jumps, j) end
            elseif n.type == "Break" then
                local loop = loop_stack[#loop_stack]
                if not loop then error("break outside loop") end
                local j = emit("JMP", 0)
                table.insert(loop.break_jumps, j)
            elseif n.type == "BinaryOp" then
                if n.op == "and" then
                    walk(n.left); emit("DUP", 0); local j = emit("JMP_IF_FALSE", 0); emit("POP", 1); walk(n.right); insts[j].arg = #insts + 1
                elseif n.op == "or" then
                    walk(n.left); emit("DUP", 0); emit("NOT", 0); local j = emit("JMP_IF_FALSE", 0); emit("POP", 1); walk(n.right); insts[j].arg = #insts + 1
                elseif n.op == "~=" then
                    walk(n.left); walk(n.right); emit("EQ", 0); emit("NOT", 0)
                elseif n.op == ">" then
                    walk(n.right); walk(n.left); emit("LT", 0)
                elseif n.op == ">=" then
                    walk(n.right); walk(n.left); emit("LE", 0)
                else
                    walk(n.left); walk(n.right);
                    local m = { ["+"]="ADD", ["-"]="SUB", ["*"]="MUL", ["/"]="DIV", ["//"]="IDIV", ["%"]="MOD", ["&"]="BAND", ["|"]="BOR", ["~"]="BXOR", ["<<"]="SHL", [">>"]="SHR", ["=="]="EQ", ["<"]="LT", ["<="]="LE", [".."]="CONCAT", ["^"]="POW" };
                    emit(m[n.op], 0)
                end
            elseif n.type == "UnaryOp" then walk(n.right); local m = { ["not"]="NOT", ["-"]="UNM", ["~"]="BNOT", ["#"]="LEN" }; emit(m[n.op], 0)
            elseif n.type == "FunctionDef" then emit("CLOSURE", addK(pp(n.body, n.args)))
            elseif n.type == "Return" then for _, v in ipairs(n.values) do walk(v) end; emit("RET", #n.values)
            elseif n.type == "If" then walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); local j2 = emit("JMP", 0); insts[j1].arg = #insts + 1; if n.elseBlock then walk(n.elseBlock) end; insts[j2].arg = #insts + 1
            elseif n.type == "While" then
                local s = #insts + 1
                local loop = { start = s, continue_target = nil, continue_jumps = {}, break_jumps = {} }
                table.insert(loop_stack, loop)
                walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); emit("JMP", s)
                table.remove(loop_stack)
                insts[j1].arg = #insts + 1
                local ct = loop.continue_target or s
                for _, cj in ipairs(loop.continue_jumps) do insts[cj].arg = ct end
                for _, bj in ipairs(loop.break_jumps) do insts[bj].arg = #insts + 1 end
            end
        end; walk(block, false, true); if #insts == 0 or insts[#insts].op ~= "RET" then emit("RET", 0) end
        local fused = {}; local o2n = {}; local i = 1
        while i <= #insts do
            o2n[i] = #fused + 1
            local c = insts[i]; local n_i = insts[i+1]
            table.insert(fused, c); i = i + 1
        end
        o2n[#insts+1] = #fused + 1
        for _, inst in ipairs(fused) do if inst.op == "JMP" or inst.op == "JMP_IF_FALSE" then inst.arg = o2n[inst.arg] end end
        return { b = fused, k = ks }
    end
    local main = pp(ast); local function sp(p)
        local ops = {}; local ids_to_op = {}; local ops_to_idx = {}
        for i, op in ipairs(ops_list) do ops[op] = {}; ops_to_idx[op] = i end
        for i, op in ipairs(ops_list) do
            local id = i - 1
            ids_to_op[id] = ops_to_idx[op]
            table.insert(ops[op], id)
        end
        for id = #ops_list, 255 do
            local op = ops_list[math.random(#ops_list)]
            ids_to_op[id] = ops_to_idx[op]
            table.insert(ops[op], id)
        end
        local b = {}; local rk = math.random(0, 255); local ck_init = math.random(0, 0xFFFFFF); local ck = ck_init
        for i, v in ipairs(p.b) do
            local op_id = ops[v.op][math.random(#ops[v.op])]
            local pck = op_id + (v.arg or 0) * 256
            if v.arg2 then pck = op_id + (v.arg % 65536) * 256 + (v.arg2 % 65536) * 16777216 end
            local encrypted = pck ~ ((rk + i) % 256)
            encrypted = (encrypted ~ (ck % 16777216))
            ck = (ck * 13 + encrypted) % 16777216
            table.insert(b, encrypted)
        end
        local em = { n = 255 }; for i = 0, 255 do em[i] = ids_to_op[i] end
        local k = { n = p.k.n }; for i = 1, p.k.n do local v = p.k[i]; if type(v) == "table" and v.b then k[i] = sp(v) else k[i] = encrypt_k(v) end end
        local cs = 0; for i=1, #b do cs = (cs + b[i]) % 1000000 end
        return { b = b, k = k, m = em, sk = rk, cs = cs, shk = math.random(0, 0xFFFFFF), ck = ck_init }
    end
    local egm = {}; for k, v in pairs(gm) do egm[k] = encrypt_k(v) end; return sp(main), egm, {SX1, SX2, SX3}
end

local Wrapper = {}
function Wrapper._sp(p, vn_func) return "{cs=" .. (p.cs or 0) .. ",b={" .. table.concat(p.b, ",") .. "},\nk=" .. Wrapper.sk(p.k, vn_func) .. ",\nm=" .. Wrapper.sk(p.m, vn_func) .. ",\nsk=" .. (p.sk or 0) .. ",\nshk=" .. p.shk .. ",\nck=" .. (p.ck or 0) .. "\n}" end
function Wrapper.sk(ks, vn)
    local s = "{"; local n = ks.n or 0; if n == 0 then for k, v in pairs(ks) do if type(k) == "number" and k > n then n = k end end end
    for i=0, n do local v = ks[i]
        if type(v) == "string" then s = s .. "[" .. i .. "]=\"" .. v:gsub(".", function(c) return "\\" .. string.format("%03d", c:byte()) end) .. "\","
        elseif type(v) == "number" then s = s .. "[" .. i .. "]=" .. v .. ","
        elseif type(v) == "table" and v.b then s = s .. "[" .. i .. "]=" .. Wrapper._sp(v, vn) .. ","
        end
    end; return s .. "}"
end

function Wrapper.generate(main, gm, sxs)
    local vmap = {}
    local function vn(n) if not vmap[n] then vmap[n] = string.format("_0x%X", math.random(0x100000, 0x999999)) end; return vmap[n] end
    local state_map = {}
    for i = 1, #ops_list do state_map[i] = math.random(0x1000, 0x999999) end
    local function sm(t)
        local s = "{"; for k, v in pairs(t) do
            s = s .. "[\"" .. k .. "\"]=\"" .. v:gsub(".", function(c) return "\\" .. string.format("%03d", c:byte()) end) .. "\","
        end; return s .. "}"
    end
    local function oq(v) return "(" .. (v - 100) .. " + 100)" end
    local sx1, sx2, sx3 = sxs[1], sxs[2], sxs[3]
    local body = "return (function(...)\n"
    body = body .. "local "..vn("_L_ENV").." = _ENV or _G; local "..vn("_L_G").." = _G; local "..vn("_L_VAULT").." = {};\n"
    body = body .. "local "..vn("_GTY")..", "..vn("_GIP")..", "..vn("_GERR")..", "..vn("_GTON")..", "..vn("_GTOS")..", "..vn("_GPCL")..", "..vn("_GUPK")..", "..vn("_GPAI")..", "..vn("_GSEL")..", "..vn("_GCLK").." = type, ipairs, error, tonumber, tostring, pcall, (table and table.unpack or unpack), pairs, select, os.clock\n"
    body = body .. "local "..vn("_L_NAMES").." = {'string','table','math','io','os','debug','coroutine','package','utf8','bit32','print','type','pairs','ipairs','next','error','tonumber','tostring','pcall','select','assert','unpack','load','loadfile','dofile','setmetatable','getmetatable'}\n"
    body = body .. "local "..vn("_L_HIDE").." = {['string']=1,['table']=1,['math']=1,['io']=1,['os']=1,['debug']=1,['coroutine']=1,['package']=1,['utf8']=1,['bit32']=1,['load']=1,['loadfile']=1,['dofile']=1}\n"
    body = body .. "for _, n in "..vn("_GIP").."("..vn("_L_NAMES")..") do "..vn("_L_VAULT").."[n] = "..vn("_L_ENV").."[n] or ("..vn("_L_G").." and "..vn("_L_G").."[n]); if "..vn("_L_HIDE").."[n] then "..vn("_L_ENV").."[n] = nil; if "..vn("_L_G").." then "..vn("_L_G").."[n] = nil end end end\n"
    body = body .. "local "..vn("_M_FL").." = ("..vn("_L_VAULT").."['math'] or math).floor; local "..vn("_S_CH")..", "..vn("_T_CO")..", "..vn("_S_SU")..", "..vn("_T_IN").." = ("..vn("_L_VAULT").."['string'] or string).char, ("..vn("_L_VAULT").."['table'] or table).concat, ("..vn("_L_VAULT").."['string'] or string).sub, ("..vn("_L_VAULT").."['table'] or table).insert\n"
    body = body .. "local "..vn("_S_BY").." = ("..vn("_L_VAULT").."['string'] or string).byte\n"
    body = body .. "local "..vn("_GSM")..", "..vn("_GGM").." = "..vn("_L_VAULT").."['setmetatable'], "..vn("_L_VAULT").."['getmetatable']\n"
    body = body .. "if "..vn("_GSM").." then "..vn("_GPCL").."("..vn("_GSM")..", '', nil) end\n"
    body = body .. "local "..vn("_GM_RAW").." = " .. sm(gm) .. "\n"
    body = body .. "local function "..vn("_d").."(v) if "..vn("_GTY").."(v) ~= 'string' then return v end; local r = {}; for i=1, #v do r[#r+1] = "..vn("_S_CH").."((("..vn("_S_BY").."(v, i) ~ "..sx3..") - "..sx2..") % 256 ~ "..sx1..") end; local rs = "..vn("_T_CO").."(r); local t, val = "..vn("_S_SU").."(rs,1,1), "..vn("_S_SU").."(rs,2); if t == 's' then return val elseif t == 'n' then return "..vn("_GTON").."(val) elseif t == 'b' then return val == 't' else return nil end end\n"
    body = body .. "local function "..vn("_GFG").."(en_val) if "..vn("_L_VAULT").."['debug'] and "..vn("_L_VAULT").."['debug'].gethook() then "..vn("_GERR").."('trace detected') end; local dn = "..vn("_d").."(en_val); if dn == '_G' then return "..vn("_L_G").." end; if dn == '_ENV' then return "..vn("_L_ENV").." end; local on_en = "..vn("_GM_RAW").."[dn]; local on = on_en and "..vn("_d").."(on_en) or dn; local res = "..vn("_L_VAULT").."[on] or "..vn("_L_ENV").."[on]; if not res then "..vn("_GERR").."('GFG fail: ' .. "..vn("_GTOS").."(on)) end; return res end\n"
    body = body .. "local "..vn("_T_LIMIT").." = "..vn("_GCLK").."();\n"
    body = body .. "local function "..vn("_EXEC").."("..vn("_PR")..", ...)\n"
    body = body .. "  local "..vn("_S")..", "..vn("_SS")..", "..vn("_P")..", "..vn("_K")..", "..vn("_V")..", "..vn("_RK")..", "..vn("_DEF")..", "..vn("_SHK")..", "..vn("_CK").." = {}, 0, 1, "..vn("_PR")..".k, {}, "..vn("_PR")..".sk, {}, "..vn("_PR")..".shk, "..vn("_PR")..".ck or 0\n"
    body = body .. "  local "..vn("_CS").." = 0; for i=1, #"..vn("_PR")..".b do "..vn("_CS").." = ("..vn("_CS").." + "..vn("_PR")..".b[i]) % 1000000 end; if "..vn("_CS").." ~= "..vn("_PR")..".cs then "..vn("_GERR").."('integrity check failed') end\n"
    body = body .. "  local "..vn("_D")..", "..vn("_ARG").." = 0x12345, 0; while "..vn("_D").." ~= 0 do\n"
    body = body .. "    if "..vn("_D").." == 0x12345 then\n"
    body = body .. "      if "..vn("_GCLK").."() - "..vn("_T_LIMIT").." > 30 then "..vn("_GERR").."('timeout') end\n"
    body = body .. "      if "..vn("_P").." > #"..vn("_PR")..".b then "..vn("_D").." = 0; break end\n"
    body = body .. "      local _EPCK = "..vn("_PR")..".b["..vn("_P").."];\n"
    body = body .. "      local _PCK = (_EPCK ~ ("..vn("_CK").." % 16777216)) ~ (("..vn("_RK").." + "..vn("_P")..") % 256);\n"
    body = body .. "      "..vn("_CK").." = ("..vn("_CK").." * 13 + _EPCK) % 16777216;\n"
    body = body .. "      "..vn("_P").." = "..vn("_P").." + 1; local _OPI = _PCK % 256; "..vn("_ARG").." = "..vn("_M_FL").."(_PCK / 256); local _OI = "..vn("_PR")..".m[_OPI]\n"

    local function si(idx) return vn("_S").."[" .. idx .. " ~ "..vn("_SHK").."]" end
    local S, SS, ARG, K, V, D, GFG, d, GTY, GERR, GPCL, GUPK, DEF, GGM, GSM, GSEL, L_ENV = vn("_S"), vn("_SS"), vn("_ARG"), vn("_K"), vn("_V"), vn("_D"), vn("_GFG"), vn("_d"), vn("_GTY"), vn("_GERR"), vn("_GPCL"), vn("_GUPK"), vn("_DEF"), vn("_GGM"), vn("_GSM"), vn("_GSEL"), vn("_L_ENV")

    local handlers = {
        LOADK = SS.." = "..SS.." + 1; "..si(SS).." = "..d.."("..K.."["..ARG.."]); "..D.." = 0x12345",
        GETVAR = SS.." = "..SS.." + 1; "..si(SS).." = "..V.."["..ARG.."]; "..D.." = 0x12345",
        SETVAR = V.."["..ARG.."] = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; "..D.." = 0x12345",
        GETVAR_G = SS.." = "..SS.." + 1; "..si(SS).." = "..GFG.."("..K.."["..ARG.."]); "..D.." = 0x12345",
        SETVAR_G = L_ENV.."["..d.."("..K.."["..ARG.."])] = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; "..D.." = 0x12345",
        GETTABLE = "local k = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local t = "..si(SS).."; "..si(SS).." = t[k]; "..D.." = 0x12345",
        SETTABLE = "local v = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local k = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local t = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; t[k] = v; "..D.." = 0x12345",
        NEWTABLE = SS.." = "..SS.." + 1; "..si(SS).." = {}; "..D.." = 0x12345",
        CALL = "local n_as, n_re = "..ARG.." % 256, "..vn("_M_FL").."("..ARG.." / 256); local as = {}; for i=1, n_as do as[n_as-i+1] = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1 end; local f = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; if not f then "..GERR.."('CALL: nil function') end; local re = {"..GPCL.."(f, "..GUPK.."(as, 1, n_as))}; if not re[1] then "..GERR.."(re[2]) end; if n_re == 255 then for i=2, #re do "..SS.." = "..SS.." + 1; "..si(SS).." = re[i] end else for i=1, n_re do "..SS.." = "..SS.." + 1; "..si(SS).." = re[i+1] end end; "..D.." = 0x12345",
        RET = "local re = {}; for i=1, "..ARG.." do re["..ARG.."-i+1] = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1 end; for i=#"..DEF..",1,-1 do "..DEF.."[i]() end; return "..GUPK.."(re)",
        ADD = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; if "..GTY.."(l) == 'number' and "..GTY.."(r) == 'number' and l % 1 == 0 and r % 1 == 0 then "..si(SS).." = (l ~ r) + 2 * (l & r) else "..si(SS).." = l + r end; "..D.." = 0x12345",
        SUB = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; if "..GTY.."(l) == 'number' and "..GTY.."(r) == 'number' and l % 1 == 0 and r % 1 == 0 then "..si(SS).." = (l + (~r)) + 1 else "..si(SS).." = l - r end; "..D.." = 0x12345",
        MUL = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l * r; "..D.." = 0x12345",
        DIV = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l / r; "..D.." = 0x12345",
        POW = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l ^ r; "..D.." = 0x12345",
        IDIV = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l // r; "..D.." = 0x12345",
        MOD = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l % r; "..D.." = 0x12345",
        BAND = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l & r; "..D.." = 0x12345",
        BOR = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l | r; "..D.." = 0x12345",
        BXOR = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l ~ r; "..D.." = 0x12345",
        SHL = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l << r; "..D.." = 0x12345",
        SHR = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l >> r; "..D.." = 0x12345",
        EQ = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l == r; "..D.." = 0x12345",
        LT = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l < r; "..D.." = 0x12345",
        LE = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l <= r; "..D.." = 0x12345",
        CONCAT = "local r = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; local l = "..si(SS).."; "..si(SS).." = l .. r; "..D.." = 0x12345",
        NOT = si(SS).." = not "..si(SS).."; "..D.." = 0x12345",
        UNM = si(SS).." = -"..si(SS).."; "..D.." = 0x12345",
        BNOT = si(SS).." = ~"..si(SS).."; "..D.." = 0x12345",
        LEN = si(SS).." = #"..si(SS).."; "..D.." = 0x12345",
        CLOSURE = "local pr = "..K.."["..ARG.."]; "..SS.." = "..SS.." + 1; "..si(SS).." = function(...) return "..vn("_EXEC").."(pr, ...) end; "..D.." = 0x12345",
        LOAD_VA = SS.." = "..SS.." + 1; "..si(SS).." = ("..GSEL.."("..ARG..", ...)); "..D.." = 0x12345",
        LOAD_VARARG = "local va = {...}; for i=1, #va do "..SS.." = "..SS.." + 1; "..si(SS).." = va[i] end; "..D.." = 0x12345",
        PICK_RESULT = "-- not implemented; "..D.." = 0x12345",
        POP = "for i=1, "..ARG.." do "..si(SS).." = nil; "..SS.." = "..SS.." - 1 end; "..D.." = 0x12345",
        DUP = "local val = "..si(SS.." - "..ARG).."; "..SS.." = "..SS.." + 1; "..si(SS).." = val; "..D.." = 0x12345",
        SWAP = "local a = "..si(SS).."; "..si(SS).." = "..si(SS.."-1").."; "..si(SS.."-1").." = a; "..D.." = 0x12345",
        JMP = vn("_P").." = "..ARG.."; "..D.." = 0x12345",
        JMP_IF_FALSE = "local v = "..si(SS).."; "..si(SS).." = nil; "..SS.." = "..SS.." - 1; if not v then "..vn("_P").." = "..ARG.." end; "..D.." = 0x12345",
        DEFER = "local f = "..d.."("..K.."["..ARG.."]); "..vn("_T_IN").."("..DEF..", function() "..vn("_EXEC").."(f) end); "..D.." = 0x12345"
    }
    local states = {}
    for op_idx, state in pairs(state_map) do table.insert(states, {idx=op_idx, state=state}) end
    for i = #states, 2, -1 do local j = math.random(i); states[i], states[j] = states[j], states[i] end

    -- Multi-Tier Partitioning
    local function partition(list, depth)
        if #list <= 3 or depth > 2 then
            local s = ""
            for i, item in ipairs(list) do
                s = s .. (i == 1 and "    if " or "    elseif ") .. D .. " == " .. oq(item.state) .. " then\n"
                s = s .. "      " .. handlers[ops_list[item.idx]]:gsub("0x12345", oq(0x12345)) .. "\n"
            end
            return s .. "    end\n"
        end
        local mid = math.floor(#list / 2)
        local left = {}; for i=1, mid do table.insert(left, list[i]) end
        local right = {}; for i=mid+1, #list do table.insert(right, list[i]) end
        local pivot = list[mid].state
        local s = "    if " .. D .. " <= " .. pivot .. " then\n"
        s = s .. partition(left, depth + 1)
        s = s .. "    else\n"
        s = s .. partition(right, depth + 1)
        s = s .. "    end\n"
        return s
    end

    table.sort(states, function(a, b) return a.state < b.state end)

    for _, s in ipairs(states) do body = body .. "      if _OI == "..s.idx.." then "..D.." = "..oq(s.state).." end\n" end
    body = body .. "    end\n"
    body = body .. partition(states, 0)
    body = body .. "  end\n\n  for _i=1, #"..DEF.." do "..DEF.."[_i]() end\nend\nreturn "..vn("_EXEC").."(" .. Wrapper._sp(main, vn) .. ", ...)\nend)(...)"
    return body
end

function Obfuscator.obfuscate(source)
    local lex = Lexer.new(source); local tokens = lex:tokenize(); local ast = Parser.new(tokens):parse()
    ast = Obfuscator.desugar(ast); ast = Obfuscator.applyMBA(ast);
    local gm = Obfuscator.obfuscateIdentifiers(ast); local main, egm, sxs = Virtualizer.virtualize(ast, gm)
    return Wrapper.generate(main, egm, sxs)
end

if _G.arg and _G.arg[1] and _G.arg[2] then
    local f = io.open(_G.arg[1], "rb"); local src = f:read("*all"); f:close()
    local ok, obf = pcall(Obfuscator.obfuscate, src)
    if ok then local out = io.open(_G.arg[2], "wb"); out:write(obf); out:close() else print(obf); os.exit(1) end
end
return Obfuscator
