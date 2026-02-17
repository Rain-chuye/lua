math.randomseed(os.time())
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
                self:consume(); self:consume(); while self:peek():match("[%da-fA-F]") do self:consume() end
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
                    elseif esc == q then table.insert(res, q)
                    else table.insert(res, esc) end
                else table.insert(res, nc) end
            end
            self:consume(); table.insert(self.tokens, { type = "string", value = table.concat(res), line = self.line })
        elseif c == "[" and self:peek(1) == "[" then
            self:consume(); self:consume(); local s = self.pos
            while self.pos <= #self.source and self.source:sub(self.pos, self.pos + 1) ~= "]]" do self:consume() end
            table.insert(self.tokens, { type = "string", value = self.source:sub(s, self.pos - 1), line = self.line })
            self:consume(); self:consume()
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
function Parser:peek() return self.tokens[self.pos] end
function Parser:consume() local t = self:peek(); self.pos = self.pos + 1; return t end
function Parser:expect(v) local t = self:consume(); if not t or (t.value ~= v and t.type ~= v) then error("Line " .. (t and t.line or "unknown") .. ": expected " .. v .. " but got " .. (t and t.value or "nil")) end; return t end
function Parser:parse() return self:parseBlock() end
function Parser:parseBlock()
    local body = {}
    while self:peek().type ~= "eof" and not ({ ["end"]=1, ["else"]=1, ["elseif"]=1, ["until"]=1 })[self:peek().value] do table.insert(body, self:parseStatement()) end
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
function Parser:parseAnd() local node = self:parseBitOr(); while self:peek().value == "and" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitOr() } end; return node end
function Parser:parseBitOr() local node = self:parseBitXor(); while self:peek().value == "|" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitXor() } end; return node end
function Parser:parseBitXor() local node = self:parseBitAnd(); while self:peek().value == "~" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseBitAnd() } end; return node end
function Parser:parseBitAnd() local node = self:parseCompare(); while self:peek().value == "&" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseCompare() } end; return node end
function Parser:parseCompare() local node = self:parseShift(); local ops = { ["=="]=1, ["~="]=1, ["<"]=1, [">"]=1, ["<="]=1, [">="]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseShift() } end; return node end
function Parser:parseShift() local node = self:parseConcat(); while self:peek().value == "<<" or self:peek().value == ">>" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseConcat() local node = self:parseAdd(); if self:peek().value == ".." then local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseConcat() } end; return node end
function Parser:parseAdd() local node = self:parseMul(); while self:peek().value == "+" or self:peek().value == "-" do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseMul() } end; return node end
function Parser:parseMul() local node = self:parseUnary(); local ops = { ["*"]=1, ["/"]=1, ["%"]=1, ["//"]=1 }; while ops[self:peek().value] do local op = self:consume().value; node = { type = "BinaryOp", op = op, left = node, right = self:parseUnary() } end; return node end
function Parser:parseUnary() local ops = { ["not"]="not", ["#"]="#", ["-"]="-", ["~"]="~" }; if ops[self:peek().value] then local op = self:consume().value; return { type = "UnaryOp", op = op, right = self:parseUnary() } end; return self:parsePrimaryExpr() end
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
            local cond = { type = "BinaryOp", op = "<=", left = { type = "Var", name = n.var }, right = { type = "Var", name = _stop } }
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
    local function walk(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v) end
        elseif n.type == "If" then n.cond = walk(n.cond); n.body = walk(n.body); for _, v in ipairs(n.elseifs) do v.cond = walk(v.cond); v.body = walk(v.body) end; n.elseBlock = walk(n.elseBlock)
        elseif n.type == "While" then n.cond = walk(n.cond); n.body = walk(n.body)
        elseif n.type == "FunctionDef" then n.body = walk(n.body)
        elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v) end
        elseif n.type == "Assign" or n.type == "LocalAssign" then
            if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
            if n.vars then for i, v in ipairs(n.vars) do n.vars[i] = walk(v) end end
        elseif n.type == "Call" or n.type == "MemberCall" then
            if n.func then n.func = walk(n.func) end
            if n.table then n.table = walk(n.table) end
            for i, v in ipairs(n.args) do n.args[i] = walk(v) end
        elseif n.type == "Table" then for _, f in ipairs(n.fields) do if f.key then f.key = walk(f.key) end; f.value = walk(f.value) end
        elseif n.type == "Index" then n.table = walk(n.table); n.key = walk(n.key)
        elseif n.type == "UnaryOp" then
            n.right = walk(n.right)
            if n.op == "~" and math.random() > 0.5 then
                return { type = "BinaryOp", op = "-", left = { type = "UnaryOp", op = "-", right = copy(n.right) }, right = { type = "Number", value = "1" } }
            end
        elseif n.type == "BinaryOp" then
            n.left = walk(n.left)
            n.right = walk(n.right)
            local function is_safe(node) return node.type == "Var" or node.type == "Number" or node.type == "Boolean" end
            if is_safe(n.left) and is_safe(n.right) and math.random() > 0.4 then
                if n.op == "+" then
                    return { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } } }
                elseif n.op == "-" then
                    return { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "&", left = { type = "UnaryOp", op = "~", right = copy(n.left) }, right = copy(n.right) } } }
                elseif n.op == "~" then
                    return { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } }
                elseif n.op == "|" then
                    return { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "~", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "&", left = copy(n.left), right = copy(n.right) } }
                elseif n.op == "&" then
                    return { type = "BinaryOp", op = "-", left = { type = "BinaryOp", op = "+", left = copy(n.left), right = copy(n.right) }, right = { type = "BinaryOp", op = "|", left = copy(n.left), right = copy(n.right) } }
                end
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
    local ops_list = { "LOADK", "GETVAR", "SETVAR", "GETTABLE", "SETTABLE", "SETTABLE_IMM", "SETTABLE_MULTI", "NEWTABLE", "CALL", "CALL_M", "RET", "RET_M", "VARARG", "VARARG_M", "JMP", "JMP_IF_FALSE", "JMP_IF_TRUE", "ADD", "SUB", "MUL", "DIV", "MOD", "EQ", "NE", "LT", "GT", "LE", "GE", "CONCAT", "NOT", "LEN", "UNM", "AND", "OR", "CLOSURE", "LOAD_VA", "PICK_RESULT", "POP", "DUP", "SWAP", "BREAK", "BOR", "BXOR", "BAND", "SHL", "SHR", "IDIV", "BNOT", "NOP" }
    local XK = math.random(1, 0xFFFFFF); local SX = math.random(1, 255)
    local function encrypt_k(v, k)
        local s
        if type(v) == "string" then s = "s" .. v
        elseif type(v) == "number" then s = "n" .. tostring(v)
        elseif type(v) == "boolean" then s = "b" .. (v and "t" or "f")
        elseif v == nil then s = "x"
        end
        local r = {}; for i=1, #s do table.insert(r, string.char(s:byte(i) ~ k)) end; return table.concat(r)
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
                    walk(n.left); walk(n.right); local ops_m = { ["+"]="ADD", ["-"]="SUB", ["*"]="MUL", ["/"]="DIV", ["%"]="MOD", ["=="]="EQ", ["~="]="NE", ["<"]="LT", [">"]="GT", ["<="]="LE", [">="]="GE", [".."]="CONCAT", ["|"]="BOR", ["~"]="BXOR", ["&"]="BAND", ["<<"]="SHL", [">>"]="SHR", ["//"]="IDIV" }; emit(ops_m[n.op], 0)
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
        local b = {}; for _, v in ipairs(p.b) do
            local op_id = get_id(v.op); table.insert(b, op_id ~ (XK % 256)); table.insert(b, (v.arg or 0) ~ XK)
        end
        local ks = { n = p.k.n }; for i = 1, p.k.n do local v = p.k[i]
            if type(v) == "table" and v.b then ks[i] = sp(v) else ks[i] = encrypt_k(v, SX) end
        end
        local ls = {}; for _, v in ipairs(p.l) do table.insert(ls, encrypt_k(v, SX)) end; return { b = b, k = ks, l = ls, m = id_to_op }
    end; local egm = {}; for k, v in pairs(gm) do egm[k] = encrypt_k(v, SX) end; return sp(main_proto), XK, ops_list, egm, SX
end
local Wrapper = {}
function Wrapper.generate(main, xk, om, gm, sx, integrity)
    local function es(s)
        if type(s) ~= "string" then return tostring(s) end
        local res = {}; for i=1, #s do table.insert(res, "\\" .. s:byte(i)) end
        return "\"" .. table.concat(res) .. "\""
    end
    local function sk(ks) local s = "{"; for i=1, (ks.n or #ks) do local v = ks[i]
        if type(v) == "string" then s = s .. es(v) .. ","
        elseif type(v) == "boolean" or type(v) == "number" then s = s .. tostring(v) .. ","
        elseif type(v) == "table" and v.b then s = s .. Wrapper._sp(v) .. ","
        else s = s .. "nil," end end; return s .. "}" end
    local function sm(m) local s = "{"; for k, v in pairs(m) do s = s .. "[" .. k .. "]=" .. es(v) .. "," end; return s .. "}" end
    function Wrapper._sp(p)
        local h = 0; for i=1, #p.b do h = (h + p.b[i]) % 0xFFFFFFFF end
        return "{b={" .. table.concat(p.b, ",") .. "},k=" .. sk(p.k) .. ",l=" .. sk(p.l) .. ",m=" .. sm(p.m) .. ",h=" .. h .. "}"
    end
    local gms = "{"; for k, v in pairs(gm) do gms = gms .. "[" .. es(k) .. "]=" .. es(v) .. "," end; gms = gms .. "}"

    local op_codes = {
        LOADK = "_SS = _SS + 1; _S[_SS] = _D(_K[_AR]); _VM_ST = _DIS",
        GETVAR = "local _N = _D(_K[_AR]); local _VAL = _V[_N]; if _VAL ~= nil then if _VAL == _NIL then _VAL = nil end elseif _ENV_MAP[_N] then _VAL = _ENV[_D(_ENV_MAP[_N])] elseif _UP then local _curr = _UP; while _curr do if _curr.v[_N] ~= nil then _VAL = _curr.v[_N]; if _VAL == _NIL then _VAL = nil end; break end; _curr = _curr.up end end; _SS = _SS + 1; _S[_SS] = _VAL; _VM_ST = _DIS",
        SETVAR = "local _N = _D(_K[_AR]); local _VAL = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if _V[_N] ~= nil then _V[_N] = (_VAL == nil and _NIL or _VAL) elseif _ENV_MAP[_N] then _ENV[_D(_ENV_MAP[_N])] = _VAL else local _curr, _f = _UP, false; while _curr do if _curr.v[_N] ~= nil then _curr.v[_N] = (_VAL == nil and _NIL or _VAL); _f = true; break end; _curr = _curr.up end; if not _f then _V[_N] = (_VAL == nil and _NIL or _VAL) end end; _VM_ST = _DIS",
        GETTABLE = "local _K = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _T = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _T[_K]; _VM_ST = _DIS",
        SETTABLE = "local _K = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _T = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _VAL = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _T[_K] = _VAL; _VM_ST = _DIS",
        SETTABLE_IMM = "local _VAL = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if _AR == 0 then local _K = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _T = _S[_SS]; _T[_K] = _VAL else local _T = _S[_SS]; _T[_AR] = _VAL end; _VM_ST = _DIS",
        SETTABLE_MULTI = "local _VAL = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _T = _S[_SS]; if type(_VAL) == 'table' and _VAL._M then for _j = 1, _VAL.n do table.insert(_T, _VAL[_j]) end else table.insert(_T, _VAL) end; _VM_ST = _DIS",
        NEWTABLE = "_SS = _SS + 1; _S[_SS] = {}; _VM_ST = _DIS",
        CALL = "local _AS = {n=0}; for _i = 1, _AR do local _V = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if type(_V) == 'table' and _V._M then for _j = _V.n, 1, -1 do table.insert(_AS, 1, _V[_j]); _AS.n = _AS.n + 1 end else table.insert(_AS, 1, _V); _AS.n = _AS.n + 1 end end; local _F = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _RE = table.pack(_F(table.unpack(_AS, 1, _AS.n))); _SS = _SS + 1; _S[_SS] = _RE[1]; _VM_ST = _DIS",
        CALL_M = "local _AS = {n=0}; for _i = 1, _AR do local _V = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if type(_V) == 'table' and _V._M then for _j = _V.n, 1, -1 do table.insert(_AS, 1, _V[_j]); _AS.n = _AS.n + 1 end else table.insert(_AS, 1, _V); _AS.n = _AS.n + 1 end end; local _F = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _RE = table.pack(_F(table.unpack(_AS, 1, _AS.n))); _RE._M = true; _SS = _SS + 1; _S[_SS] = _RE; _VM_ST = _DIS",
        RET = "local _RE = {}; for _i=1, _AR do _RE[_AR-_i+1] = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1 end; return table.unpack(_RE, 1, _AR)",
        RET_M = "local _RE = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; return table.unpack(_RE, 1, _RE.n)",
        VARARG = "local _PK = {}; for _i=_AR, _VA.n do table.insert(_PK, _VA[_i]) end; _SS = _SS + 1; _S[_SS] = _PK[1]; _VM_ST = _DIS",
        VARARG_M = "local _PK = {n=0}; for _i=_AR, _VA.n do _PK.n = _PK.n + 1; _PK[_PK.n] = _VA[_i] end; _PK._M = true; _SS = _SS + 1; _S[_SS] = _PK; _VM_ST = _DIS",
        JMP = "_P = (_AR - 1) * 2 + 1; _VM_ST = _DIS",
        JMP_IF_FALSE = "local _V = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if not _V then _P = (_AR - 1) * 2 + 1 end; _VM_ST = _DIS",
        JMP_IF_TRUE = "local _V = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if _V then _P = (_AR - 1) * 2 + 1 end; _VM_ST = _DIS",
        ADD = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L + _R; _VM_ST = _DIS",
        SUB = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L - _R; _VM_ST = _DIS",
        MUL = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L * _R; _VM_ST = _DIS",
        DIV = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L / _R; _VM_ST = _DIS",
        MOD = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L % _R; _VM_ST = _DIS",
        EQ = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L == _R); _VM_ST = _DIS",
        NE = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L ~= _R); _VM_ST = _DIS",
        LT = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L < _R); _VM_ST = _DIS",
        GT = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L > _R); _VM_ST = _DIS",
        LE = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L <= _R); _VM_ST = _DIS",
        GE = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = (_L >= _R); _VM_ST = _DIS",
        CONCAT = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L .. _R; _VM_ST = _DIS",
        NOT = "_S[_SS] = not _S[_SS]; _VM_ST = _DIS",
        LEN = "_S[_SS] = #_S[_SS]; _VM_ST = _DIS",
        UNM = "_S[_SS] = -_S[_SS]; _VM_ST = _DIS",
        AND = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L and _R; _VM_ST = _DIS",
        OR = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L or _R; _VM_ST = _DIS",
        CLOSURE = "local _PR = _K[_AR]; _SS = _SS + 1; _S[_SS] = function(...) return _EXEC(_PR, {v=_V, up=_UP}, ...) end; _VM_ST = _DIS",
        LOAD_VA = "_SS = _SS + 1; _S[_SS] = _VA[_AR]; _VM_ST = _DIS",
        PICK_RESULT = "local _RES = _S[_SS]; _SS = _SS + 1; _S[_SS] = _RES[_AR]; _VM_ST = _DIS",
        POP = "for _i=1, _AR do _S[_SS] = nil; _SS = _SS - 1 end; _VM_ST = _DIS",
        DUP = "local _V = _S[_SS - _AR]; _SS = _SS + 1; _S[_SS] = _V; _VM_ST = _DIS",
        SWAP = "local _A = _S[_SS]; _S[_SS] = _S[_SS-1]; _S[_SS-1] = _A; _VM_ST = _DIS",
        BREAK = "_VM_ST = 0",
        BOR = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L | _R; _VM_ST = _DIS",
        BXOR = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L ~ _R; _VM_ST = _DIS",
        BAND = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L & _R; _VM_ST = _DIS",
        SHL = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L << _R; _VM_ST = _DIS",
        SHR = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L >> _R; _VM_ST = _DIS",
        IDIV = "local _R = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local _L = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; _SS = _SS + 1; _S[_SS] = _L // _R; _VM_ST = _DIS",
        BNOT = "_S[_SS] = ~_S[_SS]; _VM_ST = _DIS",
        NOP = "_VM_ST = _DIS"
    }

    local states = {}
    local dis_id = math.random(100, 1000)
    local op_to_state = {}
    local all_ops = {}
    for op, _ in pairs(op_codes) do table.insert(all_ops, op) end
    table.sort(all_ops)

    for _, op in ipairs(all_ops) do
        local sid = math.random(1000, 10000)
        while states[sid] do sid = sid + 1 end
        states[sid] = op_codes[op]:gsub("_DIS", tostring(dis_id))
        op_to_state[op] = sid
    end

    local dispatcher_cases = {}
    for op, sid in pairs(op_to_state) do
        table.insert(dispatcher_cases, string.format("_OPN == %s then _VM_ST = %d", es(op), sid))
    end

    local vm_flattened = {}
    table.insert(vm_flattened, string.format("elseif _VM_ST == %d then", dis_id))
    table.insert(vm_flattened, "if _P > #_B then _VM_ST = 0 else")
    table.insert(vm_flattened, "local _OPI = _B[_P] ~ (_X % 256); _AR = _B[_P+1] ~ _X; _P = _P + 2; local _OPN = _M[_OPI]")
    table.insert(vm_flattened, "if " .. table.concat(dispatcher_cases, " elseif ") .. " else _VM_ST = 0 end end")

    for sid, code in pairs(states) do
        table.insert(vm_flattened, string.format("elseif _VM_ST == %d then %s", sid, code))
    end

    return string.format([[
local _ENV_MAP = %s
local _X = %d; local _SX = %d
local _ENV = _G or _ENV
local _NIL = {}
local function _EXEC(_PR, _UP, ...)
    if %s then
        local _H = 0; for _i=1, #_PR.b do _H = (_H + _PR.b[_i]) %% 0xFFFFFFFF end
        if _H ~= _PR.h then error("Integrity Check Failed") end
    end
    local _S, _SS, _P, _VM_ST, _AR = {}, 0, 1, %d, 0
    local _B, _K, _L, _M = _PR.b, _PR.k, _PR.l, _PR.m; local _VA = table.pack(...)
    local _V = {}
    local function _D(_V)
        if type(_V) ~= "string" then return _V end
        local _R = {}; for _i=1, #_V do table.insert(_R, string.char(_V:byte(_i) ~ _SX)) end
        local _RS = table.concat(_R)
        local _T = _RS:sub(1,1); local _VAL = _RS:sub(2)
        if _T == "s" then return _VAL
        elseif _T == "n" then return tonumber(_VAL)
        elseif _T == "b" then return _VAL == "t"
        elseif _T == "x" then return nil
        end
        return nil
    end
    for _, _n in ipairs(_L) do _V[_D(_n)] = _NIL end
    while _VM_ST ~= 0 do
        if false then
        %s
        else _VM_ST = 0 end
    end
end
return _EXEC(%s, nil, ...)]], gms, xk, sx, integrity and "true" or "false", dis_id, table.concat(vm_flattened, "\n"), Wrapper._sp(main))
end
function Obfuscator.obfuscate(source, options)
    options = options or { mba = true, integrity = true }
    local lex = Lexer.new(source); local tokens = lex:tokenize(); local par = Parser.new(tokens); local ast = par:parse()
    ast = Obfuscator.desugar(ast)
    Obfuscator.injectFakeBranches(ast)
    ast = Obfuscator.flattenControlFlow(ast)
    if options.mba then ast = Obfuscator.applyMBA(ast) end
    local gm = Obfuscator.obfuscateIdentifiers(ast); local main, xk, om, egm, sx = Virtualizer.virtualize(ast, gm)
    return Wrapper.generate(main, xk, om, egm, sx, options.integrity)
end
return Obfuscator
