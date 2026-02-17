math.randomseed(math.floor(os.time() + os.clock()))
local Lexer = {}
function Lexer.new(source) return setmetatable({ source = source, pos = 1, tokens = {}, line = 1 }, { __index = Lexer }) end
function Lexer:peek(n) n = n or 0; return self.source:sub(self.pos + n, self.pos + n) end
function Lexer:consume() local c = self:peek(); self.pos = self.pos + 1; if c == "\n" then self.line = self.line + 1 end; return c end
function Lexer:tokenize()
    while self.pos <= #self.source do
        local c = self:peek()
        if c:match("%s") then self:consume()
        elseif c:match("[%a_]") or c:byte() > 127 then
            local s = self.pos; while self:peek():match("[%w_]") or (self:peek() ~= "" and self:peek():byte() > 127) do self:consume() end
            local v = self.source:sub(s, self.pos - 1)
            local keywords = { ["local"]=1, ["function"]=1, ["return"]=1, ["if"]=1, ["then"]=1, ["else"]=1, ["elseif"]=1, ["end"]=1, ["while"]=1, ["do"]=1, ["repeat"]=1, ["until"]=1, ["for"]=1, ["in"] = 1, ["break"]=1, ["nil"]=1, ["true"]=1, ["false"]=1, ["not"]=1, ["and"]=1, ["or"]=1, ["goto"]=1 }
            table.insert(self.tokens, { type = keywords[v] and "keyword" or "name", value = v, line = self.line })
        elseif c:match("%d") then
            local s = self.pos
            if c == "0" and self:peek(1):lower() == "x" then
                self:consume(); self:consume()
                while self:peek():match("[%da-fA-F%.]") do self:consume() end
                if self:peek():lower() == "p" then
                    self:consume()
                    if self:peek() == "+" or self:peek() == "-" then self:consume() end
                    while self:peek():match("%d") do self:consume() end
                end
            else
                while self:peek():match("[%d%.]") do self:consume() end
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
                    if esc == "x" then
                        local hex = self:consume() .. self:consume()
                        table.insert(res, string.char(tonumber(hex, 16)))
                    elseif esc == "u" then
                        self:expect_char("{")
                        local hex = ""
                        while self:peek() ~= "}" do hex = hex .. self:consume() end
                        self:consume()
                        local code = tonumber(hex, 16)
                        if code < 0x80 then table.insert(res, string.char(code))
                        elseif code < 0x800 then table.insert(res, string.char(0xC0 + math.floor(code / 0x40), 0x80 + (code % 0x40)))
                        elseif code < 0x10000 then table.insert(res, string.char(0xE0 + math.floor(code / 0x1000), 0x80 + (math.floor(code / 0x40) % 0x40), 0x80 + (code % 0x40)))
                        end
                    elseif esc:match("%d") then
                        local dec = esc; if self:peek():match("%d") then dec = dec .. self:consume() end; if self:peek():match("%d") then dec = dec .. self:consume() end
                        table.insert(res, string.char(tonumber(dec)))
                    elseif esc == "n" then table.insert(res, "\n")
                    elseif esc == "r" then table.insert(res, "\r")
                    elseif esc == "t" then table.insert(res, "\t")
                    elseif esc == "\\" then table.insert(res, "\\")
                    elseif esc == "z" then while self:peek():match("%s") do self:consume() end
                    elseif esc == q then table.insert(res, q)
                    else table.insert(res, esc) end
                else table.insert(res, nc) end
            end
            self:consume(); table.insert(self.tokens, { type = "string", value = table.concat(res), line = self.line })
        elseif c == "[" and (self:peek(1) == "[" or self:peek(1) == "=") then
            local s_idx = self.pos; self:consume()
            local sep = ""
            while self:peek() == "=" do sep = sep .. self:consume() end
            if self:peek() == "[" then
                self:consume()
                if self:peek() == "\n" then self:consume() end
                local s = self.pos
                local closing = "]" .. sep .. "]"
                while self.pos <= #self.source and self.source:sub(self.pos, self.pos + #closing - 1) ~= closing do self:consume() end
                table.insert(self.tokens, { type = "string", value = self.source:sub(s, self.pos - 1), line = self.line })
                for _=1, #closing do self:consume() end
            else
                self.pos = s_idx
                table.insert(self.tokens, { type = "operator", value = self:consume(), line = self.line })
            end
        elseif c == "-" and self:peek(1) == "-" then
            self:consume(); self:consume()
            if self:peek() == "[" and self:peek(1) == "[" then
                self:consume(); self:consume()
                while self.pos <= #self.source and self.source:sub(self.pos, self.pos + 1) ~= "]]" do self:consume() end
                self:consume(); self:consume()
            else
                while self.pos <= #self.source and self:peek() ~= "\n" do self:consume() end
            end
        elseif c:match("[%p]") then
            local tri = self.source:sub(self.pos, self.pos + 2)
            local duo = self.source:sub(self.pos, self.pos + 1)
            local found = false
            if tri == "..." then
                table.insert(self.tokens, { type = "operator", value = "...", line = self.line }); self.pos = self.pos + 3; found = true
            else
                local ops = { "==", "~=", "<=", ">=", "..", "::", "<<", ">>", "//" }
                for _, op in ipairs(ops) do if duo == op then table.insert(self.tokens, { type = "operator", value = op, line = self.line }); self.pos = self.pos + 2; found = true; break end end
            end
            if not found then table.insert(self.tokens, { type = "operator", value = self:consume(), line = self.line }) end
        else self:consume() end
    end
    table.insert(self.tokens, { type = "eof", value = "eof", line = self.line }); return self.tokens
end
function Lexer:expect_char(c) if self:consume() ~= c then error("Expected " .. c) end end

local Parser = {}
function Parser.new(tokens) return setmetatable({ tokens = tokens, pos = 1 }, { __index = Parser }) end
function Parser:peek(n) n = n or 0; return self.tokens[self.pos + n] end
function Parser:consume() local t = self:peek(); self.pos = self.pos + 1; return t end
function Parser:expect(v) local t = self:consume(); if not t or (t.value ~= v and t.type ~= v) then error("Line " .. (t and t.line or "unknown") .. ": expected " .. v .. " but got " .. (t and t.value or "nil")) end; return t end
function Parser:parse() return self:parseBlock() end
function Parser:parseBlock()
    local body = {}
    while self:peek().type ~= "eof" and not ({ ["end"]=1, ["else"]=1, ["elseif"]=1, ["until"]=1 })[self:peek().value] do
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
            local vars = { self:expect("name").value }; while self:peek().value == "," do self:consume(); table.insert(vars, self:expect("name").value) end
            local values = {}
            if self:peek().value == "=" then self:consume(); table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end end
            return { type = "LocalAssign", vars = vars, values = values }
        end
    elseif t.value == "return" then
        self:consume(); local values = {}
        if not ({ ["end"]=1, ["else"]=1, ["elseif"]=1, ["until"]=1, ["eof"]=1 })[self:peek().value] then table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end end
        return { type = "Return", values = values }
    elseif t.value == "if" then
        self:consume(); local cond = self:parseExpr(); self:expect("then"); local body = self:parseBlock(); local eifs = {}
        while self:peek().value == "elseif" do self:consume(); local c = self:parseExpr(); self:expect("then"); table.insert(eifs, { cond = c, body = self:parseBlock() }) end
        local eb; if self:peek().value == "else" then self:consume(); eb = self:parseBlock() end
        self:expect("end"); return { type = "If", cond = cond, body = body, elseifs = eifs, elseBlock = eb }
    elseif t.value == "while" then self:consume(); local cond = self:parseExpr(); self:expect("do"); local body = self:parseBlock(); self:expect("end"); return { type = "While", cond = cond, body = body }
    elseif t.value == "repeat" then
        self:consume(); local body = self:parseBlock(); self:expect("until"); local cond = self:parseExpr(); return { type = "Repeat", body = body, cond = cond }
    elseif t.value == "for" then
        self:consume(); local var = self:expect("name").value
        if self:peek().value == "=" then
            self:consume(); local start = self:parseExpr(); self:expect(","); local stop = self:parseExpr(); local step
            if self:peek().value == "," then self:consume(); step = self:parseExpr() end
            self:expect("do"); local body = self:parseBlock(); self:expect("end")
            return { type = "ForRange", var = var, start = start, stop = stop, step = step, body = body }
        else
            local vars = { var }; while self:peek().value == "," do self:consume(); table.insert(vars, self:expect("name").value) end
            self:expect("in"); local iter = { self:parseExpr() }; while self:peek().value == "," do self:consume(); table.insert(iter, self:parseExpr()) end
            self:expect("do"); local body = self:parseBlock(); self:expect("end"); return { type = "ForIn", vars = vars, iter = iter, body = body }
        end
    elseif t.value == "break" then self:consume(); return { type = "Break" }
    elseif t.value == "goto" then self:consume(); return { type = "Goto", label = self:expect("name").value }
    elseif t.value == "::" then self:consume(); local name = self:expect("name").value; self:expect("::"); return { type = "Label", name = name }
    elseif t.value == "function" then
        self:consume(); local name = self:expect("name").value; local node = { type = "Var", name = name }
        while self:peek().value == "." do self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } } end
        local is_method = false
        if self:peek().value == ":" then self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } }; is_method = true end
        local args = self:parseArgs(); if is_method then table.insert(args, 1, "self") end
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
    self:expect("("); local args = {}
    if self:peek().value ~= ")" then
        if self:peek().value == "..." then table.insert(args, self:consume().value)
        else args = { self:expect("name").value }; while self:peek().value == "," do self:consume(); if self:peek().value == "..." then table.insert(args, self:consume().value); break else table.insert(args, self:expect("name").value) end end end
    end; self:expect(")"); return args
end
function Parser:parseExpr() return self:parseOr() end
function Parser:parseOr() local node = self:parseAnd(); while self:peek().value == "or" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseAnd() } end; return node end
function Parser:parseAnd() local node = self:parseCompare(); while self:peek().value == "and" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseCompare() } end; return node end
function Parser:parseCompare() local node = self:parseBitOr(); local ops = { ["=="]=1, ["~="]=1, ["<"]=1, [">"]=1, ["<="]=1, [">="]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitOr() } end; return node end
function Parser:parseBitOr() local node = self:parseBitXor(); while self:peek().value == "|" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitXor() } end; return node end
function Parser:parseBitXor() local node = self:parseBitAnd(); while self:peek().value == "~" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitAnd() } end; return node end
function Parser:parseBitAnd() local node = self:parseShift(); while self:peek().value == "&" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseShift() } end; return node end
function Parser:parseShift() local node = self:parseConcat(); while self:peek().value == "<<" or self:peek().value == ">>" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseConcat() local node = self:parseAdd(); if self:peek().value == ".." then local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseAdd() local node = self:parseMul(); while self:peek().value == "+" or self:peek().value == "-" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseMul() } end; return node end
function Parser:parseMul() local node = self:parseUnary(); local ops = { ["*"]=1, ["/"]=1, ["%"]=1, ["//"]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseUnary() } end; return node end
function Parser:parseUnary() local ops = { ["not"]="not", ["#"]="#", ["-"]="-", ["~"]="~" }; if ops[self:peek().value] then local op = self:consume().value; return { type = "UnaryOp", op = op, right = self:parseUnary() } end; return self:parsePow() end
function Parser:parsePow() local node = self:parsePrimaryExpr(); if self:peek().value == "^" then local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseUnary() } end; return node end
function Parser:parsePrimaryExpr()
    local tk = self:peek(); local node
    if tk.type == "number" then node = { type = "Number", value = self:consume().value }
    elseif tk.type == "string" then node = { type = "String", value = self:consume().value }
    elseif tk.value == "nil" then self:consume(); node = { type = "Nil" }
    elseif tk.value == "true" or tk.value == "false" then node = { type = "Boolean", value = self:consume().value == "true" }
    elseif tk.value == "..." then self:consume(); node = { type = "Vararg" }
    elseif tk.value == "{" then node = self:parseTable()
    elseif tk.value == "function" then self:consume(); local args = self:parseArgs(); local body = self:parseBlock(); self:expect("end"); node = { type = "FunctionDef", args = args, body = body }
    elseif tk.value == "(" then self:consume(); node = self:parseExpr(); self:expect(")")
    elseif tk.type == "name" then node = { type = "Var", name = self:consume().value }
    else error("Line " .. (tk and tk.line or "unknown") .. ": unexpected token " .. (tk and tk.value or "nil")) end
    while true do
        local t = self:peek()
        if t.value == "." then self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } }
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
    elseif t.value == "{" then return { self:parseTable() }
    elseif t.type == "string" then return { { type = "String", value = self:consume().value } }
    else error("Expected call arguments") end
end
function Parser:parseTable()
    self:expect("{"); local fields = {}
    while self:peek().value ~= "}" do
        if self:peek().value == "[" then self:consume(); local k = self:parseExpr(); self:expect("]"); self:expect("="); table.insert(fields, { key = k, value = self:parseExpr() })
        elseif self:peek().type == "name" and self:peek(1).value == "=" then local k = { type = "String", value = self:consume().value }; self:expect("="); table.insert(fields, { key = k, value = self:parseExpr() })
        else table.insert(fields, { value = self:parseExpr() }) end
        if self:peek().value == "," or self:peek().value == ";" then self:consume() else break end
    end; self:expect("}"); return { type = "Table", fields = fields }
end

local Obfuscator = {}
local sc = 0; local function gsn() sc = sc + 1; return "_syn_" .. sc end
function Obfuscator.desugar(ast)
    local function walk(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v) end
        elseif n.type == "If" then n.cond = walk(n.cond); n.body = walk(n.body); for _, v in ipairs(n.elseifs) do v.cond = walk(v.cond); v.body = walk(v.body) end; n.elseBlock = walk(n.elseBlock)
        elseif n.type == "While" then n.cond = walk(n.cond); n.body = walk(n.body)
        elseif n.type == "Repeat" then
            local body = walk(n.body); local cond = walk(n.cond); return walk({ type = "While", cond = { type = "Boolean", value = true }, body = { type = "Block", body = { body, { type = "If", cond = cond, body = { type = "Block", body = { { type = "Break" } } }, elseifs = {}, elseBlock = nil } } } })
        elseif n.type == "ForRange" then
            local _start, _stop, _step = gsn(), gsn(), gsn(); local init = { type = "LocalAssign", vars = { _start, _stop, _step }, values = { walk(n.start), walk(n.stop), n.step and walk(n.step) or { type = "Number", value = "1" } } }
            local decl = { type = "LocalAssign", vars = { n.var }, values = { { type = "Var", name = _start } } }
            local cond_pos = { type = "BinaryOp", op = "and", left = { type = "BinaryOp", op = ">=", left = { type = "Var", name = _step }, right = { type = "Number", value = "0" } }, right = { type = "BinaryOp", op = "<=", left = { type = "Var", name = n.var }, right = { type = "Var", name = _stop } } }
            local cond_neg = { type = "BinaryOp", op = "and", left = { type = "BinaryOp", op = "<", left = { type = "Var", name = _step }, right = { type = "Number", value = "0" } }, right = { type = "BinaryOp", op = ">=", left = { type = "Var", name = n.var }, right = { type = "Var", name = _stop } } }
            local cond = { type = "BinaryOp", op = "or", left = cond_pos, right = cond_neg }
            local update = { type = "Assign", vars = { { type = "Var", name = n.var } }, values = { { type = "BinaryOp", op = "+", left = { type = "Var", name = n.var }, right = { type = "Var", name = _step } } } }
            table.insert(n.body.body, update); return walk({ type = "Block", body = { init, decl, { type = "While", cond = cond, body = n.body } } })
        elseif n.type == "ForIn" then
            local f, s, v = gsn(), gsn(), gsn(); local init = { type = "LocalAssign", vars = { f, s, v }, values = n.iter }
            for i, val in ipairs(init.values) do init.values[i] = walk(val) end
            local call = { type = "Call", func = { type = "Var", name = f }, args = { { type = "Var", name = s }, { type = "Var", name = v } } }
            local loop_vars = { type = "LocalAssign", vars = n.vars, values = { call } }
            local check = { type = "BinaryOp", op = "~=", left = { type = "Var", name = n.vars[1] }, right = { type = "Nil" } }
            local if_break = { type = "If", cond = { type = "UnaryOp", op = "not", right = check }, body = { type = "Block", body = { { type = "Break" } } }, elseifs = {}, elseBlock = nil }
            local update_v = { type = "Assign", vars = { { type = "Var", name = v } }, values = { { type = "Var", name = n.vars[1] } } }
            table.insert(n.body.body, 1, loop_vars); table.insert(n.body.body, 2, if_break); table.insert(n.body.body, 3, update_v)
            return walk({ type = "Block", body = { init, { type = "While", cond = { type = "Boolean", value = true }, body = n.body } } })
        elseif n.type == "LocalFunction" then return walk({ type = "LocalAssign", vars = { n.name }, values = { { type = "FunctionDef", args = n.args, body = n.body } }, is_recursive = true })
        elseif n.type == "FunctionDef" then n.body = walk(n.body)
        elseif n.type == "Call" or n.type == "MemberCall" then if n.func then n.func = walk(n.func) end; if n.table then n.table = walk(n.table) end; for i, v in ipairs(n.args) do n.args[i] = walk(v) end
        elseif n.type == "Assign" or n.type == "LocalAssign" then if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
        elseif n.type == "BinaryOp" then n.left = walk(n.left); n.right = walk(n.right)
        elseif n.type == "UnaryOp" then n.right = walk(n.right)
        elseif n.type == "Index" then n.table = walk(n.table); n.key = walk(n.key)
        elseif n.type == "Table" then for _, f in ipairs(n.fields) do if f.key then f.key = walk(f.key) end; f.value = walk(f.value) end
        elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v) end
        end; return n
    end; return walk(ast)
end
function Obfuscator.injectFakeBranches(ast)
    local function walk(n)
        if not n or type(n) ~= "table" then return end
        if n.type == "Block" then
            if n.is_flat or n.is_fake then return end
            local new_body = {}; for _, stmt in ipairs(n.body) do
                if math.random() > 0.7 and (stmt.type == "Assign" or stmt.type == "Call") then
                    local is_true = math.random() > 0.5
                    local cond
                    if is_true then
                        local a = math.random(1, 10)
                        cond = { type = "BinaryOp", op = "==", left = { type = "Number", value = tostring(a) }, right = { type = "Number", value = tostring(a) } }
                    else
                        local a = math.random(1, 10)
                        cond = { type = "BinaryOp", op = "~=", left = { type = "Number", value = tostring(a) }, right = { type = "Number", value = tostring(a) } }
                    end
                    local fake_var = gsn(); local fake = { type = "Block", body = { { type = "Assign", vars = { { type = "Var", name = fake_var } }, values = { { type = "Number", value = tostring(math.random(1, 100)) } } } }, is_fake = true }
                    if is_true then
                        table.insert(new_body, { type = "If", cond = cond, body = { type = "Block", body = { stmt }, is_fake = true }, elseifs = {}, elseBlock = fake })
                    else
                        table.insert(new_body, { type = "If", cond = cond, body = fake, elseifs = {}, elseBlock = { type = "Block", body = { stmt }, is_fake = true } })
                    end
                    walk(stmt)
                else table.insert(new_body, stmt); walk(stmt) end
            end; n.body = new_body
        elseif n.type == "If" then walk(n.body); for _, v in ipairs(n.elseifs) do walk(v.body) end; walk(n.elseBlock)
        elseif n.type == "While" then walk(n.body)
        elseif n.type == "FunctionDef" then walk(n.body)
        end
    end; walk(ast)
end
function Obfuscator.flattenControlFlow(ast)
    local function flatten(block)
        if not block or block.type ~= "Block" or #block.body < 2 or block.is_flat then return block end
        local dispatcher_var = gsn(); local state_var = gsn(); local states = {}; local current_state = 1
        local function add_state(stmts) table.insert(states, { id = current_state, body = stmts }); current_state = current_state + 1; return current_state - 1 end
        local i = 1; while i <= #block.body do
            local stmt = block.body[i]
            if stmt.type == "If" or stmt.type == "While" then add_state({ stmt }); i = i + 1
            else local chunk = {}; while i <= #block.body and block.body[i].type ~= "If" and block.body[i].type ~= "While" do table.insert(chunk, block.body[i]); i = i + 1 end; add_state(chunk) end
        end
        if #states < 2 then return block end
        local hoisted = {}; local final_states = {}
        for _, s in ipairs(states) do
            local new_s_body = {}
            for _, stmt in ipairs(s.body) do
                if stmt.type == "LocalAssign" then
                    for _, v in ipairs(stmt.vars) do table.insert(hoisted, v) end
                    if #stmt.values > 0 then local as = { type = "Assign", vars = {}, values = stmt.values }; for _, v in ipairs(stmt.vars) do table.insert(as.vars, { type = "Var", name = v }) end; table.insert(new_s_body, as) end
                else table.insert(new_s_body, stmt) end
            end
            table.insert(final_states, { id = s.id, body = new_s_body })
        end
        states = final_states
        for j = 1, #states - 1 do table.insert(states[j].body, { type = "Assign", vars = { { type = "Var", name = state_var } }, values = { { type = "Number", value = tostring(states[j+1].id) } } }) end
        table.insert(states[#states].body, { type = "Assign", vars = { { type = "Var", name = state_var } }, values = { { type = "Number", value = "0" } } })
        local cases = {}; for _, s in ipairs(states) do table.insert(cases, { cond = { type = "BinaryOp", op = "==", left = { type = "Var", name = state_var }, right = { type = "Number", value = tostring(s.id) } }, body = { type = "Block", body = s.body, is_flat = true } }) end
        local dispatcher = { type = "While", cond = { type = "BinaryOp", op = "~=", left = { type = "Var", name = state_var }, right = { type = "Number", value = "0" } }, body = { type = "Block", body = { { type = "If", cond = cases[1].cond, body = cases[1].body, elseifs = { table.unpack(cases, 2) }, elseBlock = nil } }, is_flat = true }, is_flat = true, is_dispatcher = true }
        local res = { type = "Block", body = {}, is_flat = true }
        if #hoisted > 0 then table.insert(res.body, { type = "LocalAssign", vars = hoisted, values = {} }) end
        table.insert(res.body, { type = "LocalAssign", vars = { state_var }, values = { { type = "Number", value = "1" } } })
        table.insert(res.body, dispatcher); return res
    end
    local function walk(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then if not n.is_flat then n = flatten(n) end; for i, v in ipairs(n.body) do n.body[i] = walk(v) end
        elseif n.type == "If" then n.body = walk(n.body); for _, v in ipairs(n.elseifs) do v.body = walk(v.body) end; n.elseBlock = walk(n.elseBlock)
        elseif n.type == "While" then n.body = walk(n.body)
        elseif n.type == "FunctionDef" then n.body = walk(n.body)
        end; return n
    end; return walk(ast)
end
function Obfuscator.applyMBA(ast)
    local function copy(n)
        if type(n) ~= "table" then return n end
        local r = {}; for k, v in pairs(n) do r[k] = type(v) == "table" and copy(v) or v end; return r
    end
    local function walk(n, depth)
        depth = depth or 0
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v, depth) end
        elseif n.type == "If" then n.cond = walk(n.cond, depth); n.body = walk(n.body, depth); for _, v in ipairs(n.elseifs) do v.cond = walk(v.cond, depth); v.body = walk(v.body, depth) end; n.elseBlock = walk(n.elseBlock, depth)
        elseif n.type == "While" then n.cond = walk(n.cond, depth); n.body = walk(n.body, depth)
        elseif n.type == "FunctionDef" then n.body = walk(n.body, depth)
        elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v, depth) end
        elseif n.type == "Assign" or n.type == "LocalAssign" then
            if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v, depth) end end
            if n.vars then for i, v in ipairs(n.vars) do n.vars[i] = walk(v, depth) end end
        elseif n.type == "Call" or n.type == "MemberCall" then
            if n.func then n.func = walk(n.func, depth) end
            if n.table then n.table = walk(n.table, depth) end
            for i, v in ipairs(n.args) do n.args[i] = walk(v, depth) end
        elseif n.type == "Table" then for _, f in ipairs(n.fields) do if f.key then f.key = walk(f.key, depth) end; f.value = walk(f.value, depth) end
        elseif n.type == "Index" then n.table = walk(n.table, depth); n.key = walk(n.key, depth)
        elseif n.type == "UnaryOp" then
            n.right = walk(n.right, depth)
            if depth < 3 and math.random() > 0.5 then
                if n.op == "~" then
                    return walk({ type = "BinaryOp", op = "-", left = { type = "UnaryOp", op = "-", right = copy(n.right) }, right = { type = "Number", value = "1" } }, depth + 1)
                elseif n.op == "-" then
                    return walk({ type = "BinaryOp", op = "+", left = { type = "UnaryOp", op = "~", right = copy(n.right) }, right = { type = "Number", value = "1" } }, depth + 1)
                end
            end
        elseif n.type == "BinaryOp" then
            n.left = walk(n.left, depth)
            n.right = walk(n.right, depth)
            local function is_safe(node) return node.type == "Var" or node.type == "Number" or node.type == "Boolean" end
            if depth < 3 and is_safe(n.left) and is_safe(n.right) and math.random() > 0.3 then
                local res
                if n.op == "+" then
                    local r = math.random(1, 3)
                    if r == 1 then
                        res = { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } } }
                    elseif r == 2 then
                        res = { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } }
                    else
                        res = { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) } }, right = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) } }
                    end
                elseif n.op == "-" then
                    local r = math.random(1, 2)
                    if r == 1 then
                        res = { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "&", left = { type = "UnaryOp", op = "~", right = copy(n.left) }, right = copy(n.right) } } }
                    else
                        res = { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "&", left = copy(n.left), right = { type = "UnaryOp", op = "~", right = copy(n.right) } }, right = { type = "UnaryOp", op = "-", right = { type = "BinaryOp", op = "&", left = { type = "UnaryOp", op = "~", right = copy(n.left) }, right = copy(n.right) } } }
                    end
                elseif n.op == "~" then
                    local r = math.random(1, 2)
                    if r == 1 then
                        res = { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } }
                    else
                        res = { type = "BinaryOp", op = "|", left = { type = "BinaryOp", op = "&", left = copy(n.left), right = { type = "UnaryOp", op = "~", right = copy(n.right) } }, right = { type = "BinaryOp", op = "&", left = { type = "UnaryOp", op = "~", right = copy(n.left) }, right = copy(n.right) } }
                    end
                elseif n.op == "|" then
                    res = { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } }
                elseif n.op == "&" then
                    res = { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "+", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) } }
                end
                if res then return walk(res, depth + 1) end
            end
        end
        return n
    end
    return walk(ast)
end
function Obfuscator.obfuscateIdentifiers(ast)
    local gm = {}; local gr = {}; local c = 0; local function gnn()
        c = c + 1; local name = string.format("_0x%X", c + 0xABCDEF)
        while gr[name] or gm[name] do c = c + 1; name = string.format("_0x%X", c + 0xABCDEF) end
        return name
    end
    local function walk(n, s)
        if not n or type(n) ~= "table" then return end
        if n.type == "Block" then local ns = { parent = s, locals = {} }; for i, v in ipairs(n.body) do walk(v, ns) end
        elseif n.type == "Label" then -- ignore
        elseif n.type == "LocalAssign" then
            if n.is_recursive then for i, v in ipairs(n.vars) do local nn = gnn(); s.locals[v] = nn; n.vars[i] = nn end; if n.values then for _, v in ipairs(n.values) do walk(v, s) end end
            else if n.values then for _, v in ipairs(n.values) do walk(v, s) end end; for i, v in ipairs(n.vars) do local nn = gnn(); s.locals[v] = nn; n.vars[i] = nn end end
        elseif n.type == "Var" then local cur = s; local f = false; while cur do if cur.locals[n.name] then n.name = cur.locals[n.name]; f = true; break end; cur = cur.parent end; if not f then if not gr[n.name] then local nn = gnn(); gr[n.name] = nn; gm[nn] = n.name end; n.name = gr[n.name] end
        elseif n.type == "FunctionDef" then local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args) do if v ~= "..." then local nn = gnn(); fs.locals[v] = nn; n.args[i] = nn end end; walk(n.body, fs)
        elseif n.type == "If" then walk(n.cond, s); walk(n.body, s); for _, v in ipairs(n.elseifs) do walk(v.cond, s); walk(v.body, s) end; walk(n.elseBlock, s)
        elseif n.type == "While" then walk(n.cond, s); walk(n.body, s)
        elseif n.type == "Assign" then for _, v in ipairs(n.vars) do walk(v, s) end; for _, v in ipairs(n.values) do walk(v, s) end
        elseif n.type == "Call" or n.type == "MemberCall" then if n.func then walk(n.func, s) end; if n.table then walk(n.table, s) end; for _, v in ipairs(n.args) do walk(v, s) end
        elseif n.type == "BinaryOp" then walk(n.left, s); walk(n.right, s)
        elseif n.type == "UnaryOp" then walk(n.right, s)
        elseif n.type == "Index" then walk(n.table, s); walk(n.key, s)
        elseif n.type == "Table" then for _, f in ipairs(n.fields) do if f.key then walk(f.key, s) end; walk(f.value, s) end
        elseif n.type == "Return" then for _, v in ipairs(n.values) do walk(v, s) end
        elseif n.type == "Goto" or n.type == "Label" then
        end
    end; walk(ast, { locals = {} }); return gm
end
local Virtualizer = {}
function Virtualizer.virtualize(ast, gm)
    local ops_list = { "LOADK", "GETVAR", "SETVAR", "GETTABLE", "SETTABLE", "SETTABLE_IMM", "SETTABLE_MULTI", "NEWTABLE", "CALL", "CALL_M", "RET", "RET_M", "VARARG", "VARARG_M", "JMP", "JMP_IF_FALSE", "JMP_IF_TRUE", "ADD", "SUB", "MUL", "DIV", "MOD", "POW", "EQ", "NE", "LT", "GT", "LE", "GE", "CONCAT", "NOT", "LEN", "UNM", "AND", "OR", "CLOSURE", "LOAD_VA", "PICK_RESULT", "POP", "DUP", "SWAP", "BREAK", "BOR", "BXOR", "BAND", "SHL", "SHR", "IDIV", "BNOT", "NOP" }
    local XK = math.random(1, 0xFFFFFF)
    local SX1, SX2, SX3 = math.random(1, 255), math.random(1, 255), math.random(1, 255)
    local function encrypt_k(v)
        local s
        if type(v) == "string" then s = "s" .. v
        elseif type(v) == "number" then s = "n" .. tostring(v)
        elseif type(v) == "boolean" then s = "b" .. (v and "t" or "f")
        elseif v == nil then s = "x"
        end
        local r = {}; for i=1, #s do
            local b = s:byte(i)
            b = ((b ~ SX1) + SX2) % 256
            b = b ~ SX3
            table.insert(r, string.char(b))
        end; return table.concat(r)
    end
    local function pp(block, args)
        local instructions = {}; local constants = { n = 0 }; local loop_exits = {}; local locals_map = {}
        local labels = {}; local gotos = {}
        local function addK(v) for i=1, constants.n do if constants[i] == v then return i end end; constants.n = constants.n + 1; constants[constants.n] = v; return constants.n end
        local function emit(op, arg)
            if math.random() > 0.8 then
                local jtype = math.random(1, 3)
                if jtype == 1 then table.insert(instructions, { op = "LOADK", arg = addK(math.random()) }); table.insert(instructions, { op = "POP", arg = 1 })
                elseif jtype == 2 then table.insert(instructions, { op = "JMP", arg = #instructions + 2 })
                elseif jtype == 3 then table.insert(instructions, { op = "NOP", arg = 0 }) end
            end
            table.insert(instructions, { op = op, arg = arg }); return #instructions
        end
        if args then for i, arg in ipairs(args) do if arg ~= "..." then emit("LOAD_VA", i); emit("SETVAR", addK(arg)); locals_map[arg] = true end end end
        local function walk(n, multi, is_stmt)
            if not n then return end
            if n.type == "Block" then for _, v in ipairs(n.body) do walk(v, false, true) end
            elseif n.type == "LocalAssign" or n.type == "Assign" then
                local values, vars = n.values or {}, n.vars or {}
                if n.type == "LocalAssign" then for _, v in ipairs(vars) do locals_map[v] = true end end
                local use_multi = (#vars > #values) and (values[#values] and (values[#values].type == "Call" or values[#values].type == "MemberCall" or values[#values].type == "Vararg"))
                for i = 1, #values do if i == #values and use_multi then walk(values[i], true) else walk(values[i]) end end
                if #values < #vars and not use_multi then for i = #values + 1, #vars do emit("LOADK", addK(nil)) end end
                for i = #vars, 1, -1 do local var = vars[i]; local is_multi = (i >= #values and use_multi)
                    if is_multi then emit("PICK_RESULT", i - #values + 1) end
                    if type(var) == "table" and var.type == "Index" then walk(var.table); walk(var.key); emit("SETTABLE", 0)
                    elseif type(var) == "table" and var.type == "Var" then emit("SETVAR", addK(var.name)) else emit("SETVAR", addK(var)) end
                end
                if #values > 0 and values[#values] and (values[#values].type == "Call" or values[#values].type == "MemberCall" or values[#values].type == "Vararg") and #vars >= #values then emit("POP", 1) end
            elseif n.type == "Var" then emit("GETVAR", addK(n.name))
            elseif n.type == "Number" then emit("LOADK", addK(tonumber(n.value) or n.value))
            elseif n.type == "String" or n.type == "Boolean" or n.type == "Nil" then emit("LOADK", addK(n.value))
            elseif n.type == "Vararg" then local sidx = 1; if args then for i, v in ipairs(args) do if v == "..." then sidx = i; break end end end; if multi then emit("VARARG_M", sidx) else emit("VARARG", sidx) end
            elseif n.type == "BinaryOp" then
                if n.op == "and" then
                    walk(n.left); emit("DUP", 0); local j = emit("JMP_IF_FALSE", 0); emit("POP", 1); walk(n.right); instructions[j].arg = #instructions + 1
                elseif n.op == "or" then
                    walk(n.left); emit("DUP", 0); local j = emit("JMP_IF_TRUE", 0); emit("POP", 1); walk(n.right); instructions[j].arg = #instructions + 1
                else
                    walk(n.left); walk(n.right); local ops_m = { ["+"]="ADD", ["-"]="SUB", ["*"]="MUL", ["/"]="DIV", ["%"]="MOD", ["^"]="POW", ["=="]="EQ", ["~="]="NE", ["<"]="LT", [">"]="GT", ["<="]="LE", [">="]="GE", [".."]="CONCAT", ["|"]="BOR", ["~"]="BXOR", ["&"]="BAND", ["<<"]="SHL", [">>"]="SHR", ["//"]="IDIV" }; emit(ops_m[n.op], 0)
                end
            elseif n.type == "UnaryOp" then walk(n.right); local ops_m = { ["not"]="NOT", ["#"]="LEN", ["-"]="UNM", ["~"]="BNOT" }; emit(ops_m[n.op], 0)
            elseif n.type == "Call" then walk(n.func); local use_multi = n.args[#n.args] and (n.args[#n.args].type == "Call" or n.args[#n.args].type == "MemberCall" or n.args[#n.args].type == "Vararg"); for i, v in ipairs(n.args) do if i == #n.args and use_multi then walk(v, true) else walk(v) end end; if multi then emit("CALL_M", #n.args) else emit("CALL", #n.args) end; if is_stmt then emit("POP", 1) end
            elseif n.type == "MemberCall" then walk(n.table); emit("DUP", 0); emit("LOADK", addK(n.member)); emit("GETTABLE", 0); emit("SWAP", 0); local use_multi = n.args[#n.args] and (n.args[#n.args].type == "Call" or n.args[#n.args].type == "MemberCall" or n.args[#n.args].type == "Vararg"); for i, v in ipairs(n.args) do if i == #n.args and use_multi then walk(v, true) else walk(v) end end; if multi then emit("CALL_M", #n.args + 1) else emit("CALL", #n.args + 1) end; if is_stmt then emit("POP", 1) end
            elseif n.type == "Index" then walk(n.table); walk(n.key); emit("GETTABLE", 0)
            elseif n.type == "Table" then emit("NEWTABLE", 0)
                for i, v in ipairs(n.fields) do if v.key then walk(v.key); walk(v.value); emit("SETTABLE_IMM", 0) else if i == #n.fields and (v.value.type == "Call" or v.value.type == "MemberCall" or v.value.type == "Vararg") then walk(v.value, true); emit("SETTABLE_MULTI", 0) else walk(v.value); emit("SETTABLE_IMM", i) end end end
            elseif n.type == "If" then walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); local j2 = emit("JMP", 0); instructions[j1].arg = #instructions + 1; for _, ei in ipairs(n.elseifs) do walk(ei.cond); local ej1 = emit("JMP_IF_FALSE", 0); walk(ei.body); local ej2 = emit("JMP", 0); instructions[ej1].arg = #instructions + 1; instructions[ej2].arg = j2 end; if n.elseBlock then walk(n.elseBlock) end; instructions[j2].arg = #instructions + 1
            elseif n.type == "While" then
                if n.is_dispatcher then
                    local start = #instructions + 1; walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); emit("JMP", start); instructions[j1].arg = #instructions + 1
                else
                    local start = #instructions + 1; table.insert(loop_exits, {}); walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); emit("JMP", start); instructions[j1].arg = #instructions + 1; local exits = table.remove(loop_exits); for _, ex in ipairs(exits) do instructions[ex].arg = #instructions + 1 end
                end
            elseif n.type == "Return" then if #n.values == 1 and (n.values[1].type == "Call" or n.values[1].type == "MemberCall" or n.values[1].type == "Vararg") then walk(n.values[1], true); emit("RET_M", 0) else for _, v in ipairs(n.values) do walk(v) end; emit("RET", #n.values) end
            elseif n.type == "FunctionDef" then local child_proto = pp(n.body, n.args); emit("CLOSURE", addK(child_proto))
            elseif n.type == "Break" then if #loop_exits > 0 then table.insert(loop_exits[#loop_exits], emit("JMP", 0)) else error("Break outside loop") end
            elseif n.type == "Goto" then table.insert(gotos, { name = n.label, inst = emit("JMP", 0) })
            elseif n.type == "Label" then labels[n.name] = #instructions + 1
            end
        end; walk(block, false, true); if #instructions == 0 or instructions[#instructions].op ~= "RET" then emit("RET", 0) end
        for _, g in ipairs(gotos) do if labels[g.name] then instructions[g.inst].arg = labels[g.name] else error("Label " .. g.name .. " not found") end end
        local locals_list = {}; for k in pairs(locals_map) do table.insert(locals_list, k) end; return { b = instructions, k = constants, l = locals_list }
    end
    local main_proto = pp(ast); local function sp(p)
        local op_map = {}; local id_to_op = {}; local current_id = 1
        local function get_id(op)
            if not op_map[op] then
                op_map[op] = {}; for i=1, math.random(1, 2) do table.insert(op_map[op], current_id); id_to_op[current_id] = op; current_id = current_id + 1 end
            end
            return op_map[op][math.random(1, #op_map[op])]
        end
        for _, op in ipairs(ops_list) do get_id(op) end
        while current_id < 150 do id_to_op[current_id] = "NOP"; current_id = current_id + 1 end
        local PK1, PK2 = math.random(1, 0xFFFFFF), math.random(1, 0xFFFFFF)
        local b = {}; for _, v in ipairs(p.b) do
            local op_id = get_id(v.op)
            local arg = (v.arg or 0)
            local packed = (op_id % 256) + (arg * 256)
            packed = ((packed ~ PK1) + PK2) % 0xFFFFFFFF
            packed = (packed ~ XK)
            table.insert(b, packed)
        end
        local ks = { n = p.k.n }; for i = 1, p.k.n do local v = p.k[i]
            if type(v) == "table" and v.b then ks[i] = sp(v) else ks[i] = encrypt_k(v) end
        end
        local ls = {}; for _, v in ipairs(p.l) do table.insert(ls, encrypt_k(v)) end; return { b = b, k = ks, l = ls, m = id_to_op, x1 = PK1, x2 = PK2 }
    end
    local egm = {}; for k, v in pairs(gm) do egm[k] = encrypt_k(v) end; return sp(main_proto), XK, ops_list, egm, {SX1, SX2, SX3}
end

local Wrapper = {}
function Wrapper.generate(main, xk, om, gm, sx, integrity)
    local vmap = {}
    local function gv(n) if not vmap[n] then vmap[n] = string.format("_0x%X", math.random(0x1000, 0xFFFF)) end; return vmap[n] end
    local function es(s)
        if type(s) ~= "string" then return tostring(s) end
        local res = {}; for i=1, #s do table.insert(res, string.format("\\%03d", s:byte(i))) end
        return "\"" .. table.concat(res) .. "\""
    end
    local function enc(s)
        local r = {}; local str = "s" .. s; for i=1, #str do
            local b = str:byte(i)
            b = ((b ~ sx[1]) + sx[2]) % 256
            b = b ~ sx[3]
            table.insert(r, string.char(b))
        end
        return table.concat(r)
    end
    local function sk(ks) local s = "{"; for i=1, (ks.n or #ks) do local v = ks[i]
        if type(v) == "string" then s = s .. es(v) .. ","
        elseif type(v) == "boolean" or type(v) == "number" then s = s .. tostring(v) .. ","
        elseif type(v) == "table" and v.b then s = s .. Wrapper._sp(v) .. ","
        else s = s .. "nil," end end; return s .. "}" end
    local function sm(m) local s = "{"; for k, v in pairs(m) do s = s .. "[" .. k .. "]=" .. es(v) .. "," end; return s .. "}" end
    function Wrapper._sp(p)
        local h = 0; for i=1, #p.b do h = (h + p.b[i]) % 0xFFFFFFFF end
        local r_off = math.random(1, 100); local seed = math.random(1, 1000000)
        return "{b={" .. table.concat(p.b, ",") .. "},k=" .. sk(p.k) .. ",l=" .. sk(p.l) .. ",m=" .. sm(p.m) .. ",h=" .. h .. ",r=" .. r_off .. ",s=" .. seed .. ",x1=" .. p.x1 .. ",x2=" .. p.x2 .. "}"
    end

    local function oc(v) local o = math.random(1, 1000); return "(" .. (v + o) .. " - " .. o .. ")" end
    local function mba_add(a, b)
        local r = math.random(1, 3)
        if r == 1 then return "((" .. a .. " ~ " .. b .. ") + 2 * (" .. a .. " & " .. b .. "))"
        elseif r == 2 then return "((" .. a .. " | " .. b .. ") + (" .. a .. " & " .. b .. "))"
        else return "(" .. a .. " + " .. oc(tonumber(b) or 0) .. ")" end
    end
    local function mba_sub(a, b) return "((" .. a .. " ~ " .. b .. ") - 2 * ( ~" .. a .. " & " .. b .. "))" end
    local v_stack, v_ss, v_p, v_st, v_ar = gv("_S"), gv("_SS"), gv("_P"), gv("_VM_ST"), gv("_AR")
    local v_shuf = math.random(0x100, 0xFFF)
    local function sa(idx)
        local s = v_shuf
        local r = math.random(1, 3)
        if r == 1 then
            return v_stack .. "[(" .. idx .. " + " .. s .. ") - 2 * (" .. idx .. " & " .. s .. ")]"
        elseif r == 2 then
            return v_stack .. "[2 * (" .. idx .. " | " .. s .. ") - (" .. idx .. " + " .. s .. ")]"
        else
            return v_stack .. "[(" .. idx .. " ~ " .. s .. ")]"
        end
    end
    local v_pr, v_up, v_b, v_k, v_l, v_m, v_va, v_v = gv("_PR"), gv("_UP"), gv("_B"), gv("_K"), gv("_L"), gv("_M"), gv("_VA"), gv("_V")
    local f_up_v, f_up_u = gv("up_v"), gv("up_u")
    local v_dis = gv("_DIS"); local v_clk = gv("_CLK"); local v_d = gv("_D")
    local v_env_map, v_x, v_sx, v_env, v_nil = gv("_ENV_MAP"), gv("_X"), gv("_SX"), gv("_ENV"), gv("_NIL")
    local v_exec = gv("_EXEC")
    local dis_id = math.random(100, 1000)

    local v_libs = gv("_LIBS")
    local function l(lib, f)
        if not f then return "_GFL(" .. es(enc(lib)) .. ")" end
        return "_GFL(" .. es(enc(lib)) .. ", " .. es(enc(f)) .. ")"
    end
    local function gl(f) return "_GFG(" .. es(enc(f)) .. ")" end

    local op_codes = {
        LOADK = v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = " .. v_d .. "(" .. v_k .. "[" .. v_ar .. "]); " .. v_st .. " = " .. dis_id,
        GETVAR = "local _N = " .. v_d .. "(" .. v_k .. "[" .. v_ar .. "]); local _VAL = " .. v_v .. "[_N]; if _VAL ~= nil then if _VAL == " .. v_nil .. " then _VAL = nil end elseif " .. v_env_map .. "[_N] then _VAL = " .. v_env .. "[" .. v_d .. "(" .. v_env_map .. "[_N])] elseif " .. v_up .. " then local _curr = " .. v_up .. "; while _curr do if _curr." .. f_up_v .. "[_N] ~= nil then _VAL = _curr." .. f_up_v .. "[_N]; if _VAL == " .. v_nil .. " then _VAL = nil end; break end; _curr = _curr." .. f_up_u .. " end end; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _VAL; " .. v_st .. " = " .. dis_id,
        SETVAR = "local _N = " .. v_d .. "(" .. v_k .. "[" .. v_ar .. "]); local _VAL = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if " .. v_v .. "[_N] ~= nil then " .. v_v .. "[_N] = (_VAL == nil and " .. v_nil .. " or _VAL) elseif " .. v_env_map .. "[_N] then " .. v_env .. "[" .. v_d .. "(" .. v_env_map .. "[_N])] = _VAL else local _curr, _f = " .. v_up .. ", false; while _curr do if _curr." .. f_up_v .. "[_N] ~= nil then _curr." .. f_up_v .. "[_N] = (_VAL == nil and " .. v_nil .. " or _VAL); _f = true; break end; _curr = _curr." .. f_up_u .. " end if not _f then " .. v_v .. "[_N] = (_VAL == nil and " .. v_nil .. " or _VAL) end end; " .. v_st .. " = " .. dis_id,
        GETTABLE = "local _K = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _T = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _T[_K]; " .. v_st .. " = " .. dis_id,
        SETTABLE = "local _K = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _T = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _VAL = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; _T[_K] = _VAL; " .. v_st .. " = " .. dis_id,
        SETTABLE_IMM = "local _VAL = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if " .. v_ar .. " == 0 then local _K = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _T = " .. sa(v_ss) .. "; _T[_K] = _VAL else local _T = " .. sa(v_ss) .. "; _T[" .. v_ar .. "] = _VAL end; " .. v_st .. " = " .. dis_id,
        SETTABLE_MULTI = "local _VAL = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _T = " .. sa(v_ss) .. "; if " .. gl("type") .. "(_VAL) == 'table' and _VAL._M then for _j = 1, _VAL.n do " .. l("table", "insert") .. "(_T, _VAL[_j]) end else " .. l("table", "insert") .. "(_T, _VAL) end; " .. v_st .. " = " .. dis_id,
        NEWTABLE = v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = {}; " .. v_st .. " = " .. dis_id,
        CALL = "local _AS = {n=0}; for _i = 1, " .. v_ar .. " do local _V = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if " .. gl("type") .. "(_V) == 'table' and _V._M then for _j = _V.n, 1, -1 do " .. l("table", "insert") .. "(_AS, 1, _V[_j]); _AS.n = " .. mba_add("_AS.n", "1") .. " end else " .. l("table", "insert") .. "(_AS, 1, _V); _AS.n = " .. mba_add("_AS.n", "1") .. " end end local _F = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _RE = " .. l("table", "pack") .. "(_F(" .. l("table", "unpack") .. "(_AS, 1, _AS.n))); " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _RE[1]; " .. v_st .. " = " .. dis_id,
        CALL_M = "local _AS = {n=0}; for _i = 1, " .. v_ar .. " do local _V = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if " .. gl("type") .. "(_V) == 'table' and _V._M then for _j = _V.n, 1, -1 do " .. l("table", "insert") .. "(_AS, 1, _V[_j]); _AS.n = " .. mba_add("_AS.n", "1") .. " end else " .. l("table", "insert") .. "(_AS, 1, _V); _AS.n = " .. mba_add("_AS.n", "1") .. " end end local _F = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _RE = " .. l("table", "pack") .. "(_F(" .. l("table", "unpack") .. "(_AS, 1, _AS.n))); _RE._M = true; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _RE; " .. v_st .. " = " .. dis_id,
        RET = "local _RE = {}; for _i=1, " .. v_ar .. " do _RE[" .. mba_add(mba_sub(v_ar, "_i"), "1") .. "] = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. " end " .. v_ss .. " = " .. v_pr .. ".r or 0; return " .. l("table", "unpack") .. "(_RE, 1, " .. v_ar .. ")",
        RET_M = "local _RE = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; return " .. l("table", "unpack") .. "(_RE, 1, _RE.n)",
        VARARG = "local _PK = {}; for _i=" .. v_ar .. ", " .. v_va .. ".n do " .. l("table", "insert") .. "(_PK, " .. v_va .. "[_i]) end; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _PK[1]; " .. v_st .. " = " .. dis_id,
        VARARG_M = "local _PK = {n=0}; for _i=" .. v_ar .. ", " .. v_va .. ".n do _PK.n = " .. mba_add("_PK.n", "1") .. "; _PK[_PK.n] = " .. v_va .. "[_i] end; _PK._M = true; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _PK; " .. v_st .. " = " .. dis_id,
        JMP = v_p .. " = " .. v_ar .. "; " .. v_st .. " = " .. dis_id,
        JMP_IF_FALSE = "local _V = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if not _V then " .. v_p .. " = " .. v_ar .. " end; " .. v_st .. " = " .. dis_id,
        JMP_IF_TRUE = "local _V = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; if _V then " .. v_p .. " = " .. v_ar .. " end; " .. v_st .. " = " .. dis_id,
        ADD = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L + _R; " .. v_st .. " = " .. dis_id,
        SUB = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L - _R; " .. v_st .. " = " .. dis_id,
        MUL = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L * _R; " .. v_st .. " = " .. dis_id,
        DIV = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L / _R; " .. v_st .. " = " .. dis_id,
        MOD = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L % _R; " .. v_st .. " = " .. dis_id,
        EQ = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = (_L == _R); " .. v_st .. " = " .. dis_id,
        NE = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = (_L ~= _R); " .. v_st .. " = " .. dis_id,
        LT = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L < _R; " .. v_st .. " = " .. dis_id,
        GT = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L > _R; " .. v_st .. " = " .. dis_id,
        LE = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L <= _R; " .. v_st .. " = " .. dis_id,
        GE = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L >= _R; " .. v_st .. " = " .. dis_id,
        POW = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L ^ _R; " .. v_st .. " = " .. dis_id,
        CONCAT = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L .. _R; " .. v_st .. " = " .. dis_id,
        NOT = sa(v_ss) .. " = not " .. sa(v_ss) .. "; " .. v_st .. " = " .. dis_id,
        LEN = sa(v_ss) .. " = #" .. sa(v_ss) .. "; " .. v_st .. " = " .. dis_id,
        UNM = sa(v_ss) .. " = -" .. sa(v_ss) .. "; " .. v_st .. " = " .. dis_id,
        AND = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L and _R; " .. v_st .. " = " .. dis_id,
        OR = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L or _R; " .. v_st .. " = " .. dis_id,
        CLOSURE = "local _PR = " .. v_k .. "[" .. v_ar .. "]; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = function(...) return " .. v_exec .. "(_PR, {[" .. es(f_up_v) .. "]=" .. v_v .. ", [" .. es(f_up_u) .. "]=" .. v_up .. "}, ...) end; " .. v_st .. " = " .. dis_id,
        LOAD_VA = v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = " .. v_va .. "[" .. v_ar .. "]; " .. v_st .. " = " .. dis_id,
        PICK_RESULT = "local _RES = " .. sa(v_ss) .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _RES[" .. v_ar .. "]; " .. v_st .. " = " .. dis_id,
        POP = "for _i=1, " .. v_ar .. " do " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. " end " .. v_st .. " = " .. dis_id,
        DUP = "local _V = " .. v_stack .. "[(" .. v_ss .. " - " .. v_ar .. ") ~ " .. v_shuf .. "]; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _V; " .. v_st .. " = " .. dis_id,
        SWAP = "local _A = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = " .. v_stack .. "[(" .. v_ss .. "-1) ~ " .. v_shuf .. "]; " .. v_stack .. "[(" .. v_ss .. "-1) ~ " .. v_shuf .. "] = _A; " .. v_st .. " = " .. dis_id,
        BREAK = v_st .. " = 0",
        BOR = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L | _R; " .. v_st .. " = " .. dis_id,
        BXOR = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L ~ _R; " .. v_st .. " = " .. dis_id,
        BAND = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L & _R; " .. v_st .. " = " .. dis_id,
        SHL = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L << _R; " .. v_st .. " = " .. dis_id,
        SHR = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L >> _R; " .. v_st .. " = " .. dis_id,
        IDIV = "local _R = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; local _L = " .. sa(v_ss) .. "; " .. sa(v_ss) .. " = nil; " .. v_ss .. " = " .. mba_sub(v_ss, "1") .. "; " .. v_ss .. " = " .. mba_add(v_ss, "1") .. "; " .. sa(v_ss) .. " = _L // _R; " .. v_st .. " = " .. dis_id,
        BNOT = sa(v_ss) .. " = ~" .. sa(v_ss) .. "; " .. v_st .. " = " .. dis_id,
        NOP = v_st .. " = " .. dis_id
    }

    local states = {}
    local op_to_state = {}
    local all_ops = {}
    for op, _ in pairs(op_codes) do table.insert(all_ops, op) end
    table.sort(all_ops)

    for _, op in ipairs(all_ops) do
        local sid = math.random(1000, 10000)
        while states[sid] do sid = sid + 1 end
        states[sid] = op_codes[op]
        op_to_state[op] = sid
    end

    -- Add trap states
    for i=1, 20 do
        local sid = math.random(10000, 20000)
        while states[sid] do sid = sid + 1 end
        states[sid] = v_st .. " = " .. math.random(1, 1000) .. "; if 1+1==3 then " .. v_p .. "=0 end"
    end

    local t_ids = {}
    for sid in pairs(states) do table.insert(t_ids, ((sid * 13) + 7) % 10007) end
    table.sort(t_ids)
    local mid_t_idx = math.floor(#t_ids / 2)
    local mid_t_sid = t_ids[mid_t_idx] or 0

    local dispatcher_cases = {}
    for op, sid in pairs(op_to_state) do
        -- Indirect jump to next state
        local trans = v_st .. " = ((" .. sid .. " * 13) + 7) % 10007"
        table.insert(dispatcher_cases, "_OPN == " .. es(op) .. " then " .. trans)
    end

    local vm_flattened = {}
    -- Multi-dispatcher logic
    table.insert(vm_flattened, string.format("elseif (" .. v_st .. " ~ %d) == 0 then", dis_id))
    table.insert(vm_flattened, "    if " .. v_p .. " > #" .. v_b .. " or (" .. l("os", "clock") .. "() - " .. v_clk .. " > 15.0) then " .. v_st .. " = 0 else")
    table.insert(vm_flattened, "        local _PCK = " .. v_b .. "[" .. v_p .. "]; " .. v_p .. " = " .. mba_add(v_p, "1"))
    table.insert(vm_flattened, "        _PCK = (_PCK ~ " .. v_x .. ")")
    table.insert(vm_flattened, "        _PCK = (_PCK - " .. v_pr .. ".x2) % 4294967296")
    table.insert(vm_flattened, "        _PCK = (_PCK ~ " .. v_pr .. ".x1)")
    table.insert(vm_flattened, "        local _OPI = (_PCK % 256); " .. v_ar .. " = " .. l("math", "floor") .. "(_PCK / 256); local _OPN = " .. v_m .. "[_OPI]")
    table.insert(vm_flattened, "        if " .. table.concat(dispatcher_cases, " elseif ") .. " else " .. v_st .. " = 0 end end")

    -- Two-tier dispatcher
    local d1_cases = {}
    local d2_cases = {}
    for sid, code in pairs(states) do
        local target_sid = ((sid * 13) + 7) % 10007
        -- Each opcode handler returns to dispatcher via indirect calculation
        local back_to_dis = v_st .. " = ((" .. v_st .. " - 7) * 6928) % 10007; " .. v_st .. " = " .. dis_id
        local _case
        if code:match("return") then
            _case = "elseif (" .. v_st .. " ~ " .. target_sid .. ") == 0 then " .. code
        else
            _case = "elseif (" .. v_st .. " ~ " .. target_sid .. ") == 0 then " .. code .. "; " .. back_to_dis
        end
        if target_sid < mid_t_sid then table.insert(d1_cases, _case) else table.insert(d2_cases, _case) end
    end

    local d_logic = "elseif true then if (function(s) return s < " .. mid_t_sid .. " end)(" .. v_st .. ") then \n" ..
                    " if false then \n" .. table.concat(d1_cases, "\n") .. "\n end \n" ..
                    " else if false then \n" .. table.concat(d2_cases, "\n") .. "\n end end"
    table.insert(vm_flattened, d_logic)

    local gms = "{"; for k, v in pairs(gm) do gms = gms .. "[" .. es(k) .. "]=" .. es(v) .. "," end; gms = gms .. "}"
    local sx_literal = "{" .. table.concat(sx, ",") .. "}"
    local body = "local " .. v_env_map .. " = " .. gms .. "\n"
    body = body .. "local " .. v_x .. " = " .. tostring(xk) .. "; local " .. v_sx .. " = " .. sx_literal .. "\n"
    body = body .. "local " .. v_env .. " = _G or _ENV\nlocal " .. v_nil .. " = {}\n"
    body = body .. "local _GSC = (" .. v_env .. "[" .. es("string") .. "] or _G[" .. es("string") .. "])[" .. es("char") .. "]\n"
    body = body .. "local _GSB = (" .. v_env .. "[" .. es("string") .. "] or _G[" .. es("string") .. "])[" .. es("byte") .. "]\n"
    body = body .. "local _GTC = (" .. v_env .. "[" .. es("table") .. "] or _G[" .. es("table") .. "] or {})[" .. es("concat") .. "] or (" .. v_env .. "[" .. es("table") .. "] or _G[" .. es("table") .. "])[" .. es("concat") .. "]\n"
    body = body .. "local function " .. v_d .. "(_V)\n"
    body = body .. "    if (" .. v_env .. "[" .. es("type") .. "] or _G[" .. es("type") .. "])(_V) ~= " .. es("string") .. " then return _V end\n"
    body = body .. "    local _R = {}; for _i=1, #_V do \n"
    body = body .. "        local _B = _GSB(_V, _i)\n"
    body = body .. "        _B = (_B ~ " .. v_sx .. "[3])\n"
    body = body .. "        _B = (_B - " .. v_sx .. "[2]) % 256\n"
    body = body .. "        _B = (_B ~ " .. v_sx .. "[1])\n"
    body = body .. "        _R[#_R + 1] = _GSC(_B)\n"
    body = body .. "    end\n"
    body = body .. "    local _RS = _GTC(_R)\n"
    body = body .. "    local _T = _RS:sub(1,1); local _VAL = _RS:sub(2)\n"
    body = body .. "    if _T == \"s\" then return _VAL\n"
    body = body .. "    elseif _T == \"n\" then return (" .. v_env .. "[" .. es("tonumber") .. "] or _G[" .. es("tonumber") .. "])(_VAL)\n"
    body = body .. "    elseif _T == \"b\" then return _VAL == \"t\"\n"
    body = body .. "    elseif _T == \"x\" then return nil\n"
    body = body .. "    end\n"
    body = body .. "    return nil\n"
    body = body .. "end\n"
    body = body .. "local function " .. v_exec .. "(" .. v_pr .. ", " .. v_up .. ", ...)\n"
    body = body .. "    local function _GFL(_L, _F)\n"
    body = body .. "        local _LB = " .. v_env .. "[" .. v_d .. "(_L)] or _G[" .. v_d .. "(_L)]\n"
    body = body .. "        if _F then return _LB[" .. v_d .. "(_F)] else return _LB end\n"
    body = body .. "    end\n"
    body = body .. "    local function _GFG(_N)\n"
    body = body .. "        return _G[" .. v_d .. "(_N)] or " .. v_env .. "[" .. v_d .. "(_N)]\n"
    body = body .. "    end\n"
    body = body .. "    local " .. v_clk .. " = " .. l("os", "clock") .. "(); " .. l("math", "randomseed") .. "(" .. v_pr .. ".s or " .. l("os", "time") .. "())\n"
    body = body .. "    if " .. gl("debug") .. " then if " .. l("debug", "gethook") .. "() or " .. l("debug", "getinfo") .. "(50, 'f') then " .. gl("error") .. "(\"\\065\\110\\116\\105\\045\\068\\101\\098\\117\\103\") end end\n"
    body = body .. "    if " .. (integrity and "true" or "false") .. " then\n"
    body = body .. "        local _H = 0; for _i=1, #" .. v_pr .. ".b do _H = (_H + " .. v_pr .. ".b[_i]) % 4294967295 end\n"
    body = body .. "        if _H ~= " .. v_pr .. ".h then " .. gl("error") .. "(\"\\073\\110\\116\\101\\103\\114\\105\\116\\121\\032\\067\\104\\101\\099\\107\\032\\070\\097\\105\\108\\101\\100\") end\n"
    body = body .. "    end\n"
    body = body .. "    local " .. v_stack .. ", " .. v_ss .. ", " .. v_p .. ", " .. v_st .. ", " .. v_ar .. " = {}, " .. v_pr .. ".r or 0, 1, " .. dis_id .. ", 0\n"
    body = body .. "    local " .. v_b .. ", " .. v_k .. ", " .. v_l .. ", " .. v_m .. " = " .. v_pr .. ".b, " .. v_pr .. ".k, " .. v_pr .. ".l, " .. v_pr .. ".m; local " .. v_va .. " = " .. l("table", "pack") .. "(...)\n"
    body = body .. "    local " .. v_v .. " = {}\n"
    body = body .. "    for _, _n in " .. gl("ipairs") .. "(" .. v_l .. ") do " .. v_v .. "[" .. v_d .. "(_n)] = " .. v_nil .. " end\n"
    body = body .. "    while (function() local _o = " .. (math.random(1, 100)) .. "; return _o == _o end)() and (" .. v_st .. " ~ 0) ~= 0 do\n"
    body = body .. "        if 1 == 2 then\n"
    body = body .. table.concat(vm_flattened, "\n")
    body = body .. "\n        else " .. v_st .. " = 0 end\n"
    body = body .. "    end\n"
    body = body .. "end\n"
    body = body .. "return " .. v_exec .. "(" .. Wrapper._sp(main) .. ", nil, ...)"
    return body
end
function Obfuscator.obfuscate(source, options)
    options = options or { mba = true, integrity = true, fake = true, commercial = true }
    local lex = Lexer.new(source); local tokens = lex:tokenize(); local par = Parser.new(tokens); local ast = par:parse()
    ast = Obfuscator.desugar(ast)
    if options.fake ~= false then Obfuscator.injectFakeBranches(ast) end
    ast = Obfuscator.flattenControlFlow(ast)
    if options.mba ~= false then ast = Obfuscator.applyMBA(ast) end
    local gm = Obfuscator.obfuscateIdentifiers(ast); local main, xk, om, egm, sx = Virtualizer.virtualize(ast, gm)
    return Wrapper.generate(main, xk, om, egm, sx, options.integrity ~= false)
end
return Obfuscator
