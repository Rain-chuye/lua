-- Advanced Lua 5.3.3 Obfuscator Engine - Enhanced Version
-- Author: Jules (Enhanced for Maximum Security)

local Lexer = {}
Lexer.__index = Lexer
function Lexer.new(source)
    local self = setmetatable({}, Lexer)
    self.source = source
    self.pos = 1
    self.tokens = {}
    return self
end
function Lexer:tokenize()
    local ops = { ["=="]=1, ["~="]=1, ["<="]=1, [">="]=1, ["//"]=1, ["<<"]=1, [">>"]=1, [".."]=1 }
    local single_ops = "[](){}+-*/%^#<>=~.,:;&|"
    while self.pos <= #self.source do
        local c = self.source:sub(self.pos, self.pos)
        if c:match("%s") then self.pos = self.pos + 1
        elseif self.source:sub(self.pos, self.pos+1) == "--" then
            self.pos = self.pos + 2
            if self.source:sub(self.pos, self.pos+1) == "[[" then
                local close = self.source:find("]]", self.pos)
                self.pos = close and (close + 2) or (#self.source + 1)
            else
                local nl = self.source:find("\n", self.pos)
                self.pos = nl and (nl + 1) or (#self.source + 1)
            end
        elseif c:match("%a") or c == "_" then
            local s = self.pos
            while self.pos <= #self.source and self.source:sub(self.pos, self.pos):match("[%w_]") do self.pos = self.pos + 1 end
            local val = self.source:sub(s, self.pos-1)
            table.insert(self.tokens, { type = "name", value = val })
        elseif c:match("%d") then
            local s = self.pos
            while self.pos <= #self.source and self.source:sub(self.pos, self.pos):match("[%d%.xX%a]") do self.pos = self.pos + 1 end
            table.insert(self.tokens, { type = "number", value = self.source:sub(s, self.pos-1) })
        elseif c == "\"" or c == "'" then
            local s = self.pos; self.pos = self.pos + 1
            while self.pos <= #self.source and self.source:sub(self.pos, self.pos) ~= c do
                if self.source:sub(self.pos, self.pos) == "\\" then self.pos = self.pos + 2 else self.pos = self.pos + 1 end
            end
            self.pos = self.pos + 1
            table.insert(self.tokens, { type = "string", value = self.source:sub(s+1, self.pos-2) })
        elseif c == "[" and (self.source:sub(self.pos+1, self.pos+1) == "[" or self.source:sub(self.pos+1, self.pos+1) == "=") then
            local s = self.pos; local eq = ""; self.pos = self.pos + 1
            while self.source:sub(self.pos, self.pos) == "=" do eq = eq .. "="; self.pos = self.pos + 1 end
            if self.source:sub(self.pos, self.pos) == "[" then
                self.pos = self.pos + 1
                local close = "]" .. eq .. "]"
                local e = self.source:find(close, self.pos, true)
                local val = self.source:sub(self.pos, e and (e-1) or #self.source)
                self.pos = e and (e + #close) or (#self.source + 1)
                table.insert(self.tokens, { type = "string", value = val })
            else table.insert(self.tokens, { type = "op", value = "[" }) end
        elseif ops[self.source:sub(self.pos, self.pos+1)] then
            table.insert(self.tokens, { type = "op", value = self.source:sub(self.pos, self.pos+1) })
            self.pos = self.pos + 2
        elseif single_ops:find(c, 1, true) then
            table.insert(self.tokens, { type = "op", value = c })
            self.pos = self.pos + 1
        else self.pos = self.pos + 1 end
    end
    table.insert(self.tokens, { type = "eof", value = "eof" })
    return self.tokens
end

local Parser = {}
Parser.__index = Parser
function Parser.new(tokens)
    local self = setmetatable({}, Parser)
    self.tokens = tokens
    self.pos = 1
    return self
end
function Parser:peek(n) return self.tokens[self.pos + (n or 0)] end
function Parser:consume() local t = self:peek(); self.pos = self.pos + 1; return t end
function Parser:expect(val)
    local t = self:consume()
    if t.value ~= val and t.type ~= val then error("Expected " .. val .. " got " .. (t and t.value or "nil")) end
    return t
end
function Parser:parseBlock()
    local body = {}
    while self:peek().value ~= "end" and self:peek().value ~= "elseif" and self:peek().value ~= "else" and self:peek().value ~= "until" and self:peek().type ~= "eof" do
        table.insert(body, self:parseStmt())
        if self:peek().value == ";" then self:consume() end
    end
    return { type = "Block", body = body }
end
function Parser:parseStmt()
    local tk = self:peek()
    if tk.value == "local" then
        self:consume()
        if self:peek().value == "function" then
            self:consume(); local name = self:expect("name").value; local args = self:parseArgs(); local body = self:parseBlock(); self:expect("end")
            return { type = "LocalFunction", name = name, args = args, body = body }
        else
            local vars = { self:expect("name").value }; while self:peek().value == "," do self:consume(); table.insert(vars, self:expect("name").value) end
            local values = {}; if self:peek().value == "=" then self:consume(); table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end end
            return { type = "LocalAssign", vars = vars, values = values }
        end
    elseif tk.value == "function" then
        self:consume(); local name = self:parseExpr(); local args = self:parseArgs(); local body = self:parseBlock(); self:expect("end")
        return { type = "Assign", vars = { name }, values = { { type = "FunctionDef", args = args, body = body } } }
    elseif tk.value == "if" then
        self:consume(); local cond = self:parseExpr(); self:expect("then"); local body = self:parseBlock(); local elseifs = {}
        while self:peek().value == "elseif" do self:consume(); local c = self:parseExpr(); self:expect("then"); table.insert(elseifs, { cond = c, body = self:parseBlock() }) end
        local elseBlock; if self:peek().value == "else" then self:consume(); elseBlock = self:parseBlock() end
        self:expect("end"); return { type = "If", cond = cond, body = body, elseifs = elseifs, elseBlock = elseBlock }
    elseif tk.value == "while" then
        self:consume(); local cond = self:parseExpr(); self:expect("do"); local body = self:parseBlock(); self:expect("end")
        return { type = "While", cond = cond, body = body }
    elseif tk.value == "repeat" then
        self:consume(); local body = self:parseBlock(); self:expect("until"); local cond = self:parseExpr()
        return { type = "Repeat", body = body, cond = cond }
    elseif tk.value == "for" then
        self:consume(); local name = self:expect("name").value
        if self:peek().value == "=" then
            self:consume(); local start = self:parseExpr(); self:expect(","); local stop = self:parseExpr(); local step; if self:peek().value == "," then self:consume(); step = self:parseExpr() end
            self:expect("do"); local body = self:parseBlock(); self:expect("end")
            return { type = "ForRange", var = name, start = start, stop = stop, step = step, body = body }
        else
            local vars = { name }; while self:peek().value == "," do self:consume(); table.insert(vars, self:expect("name").value) end
            self:expect("in"); local iter = { self:parseExpr() }; while self:peek().value == "," do self:consume(); table.insert(iter, self:parseExpr()) end
            self:expect("do"); local body = self:parseBlock(); self:expect("end")
            return { type = "ForIn", vars = vars, iter = iter, body = body }
        end
    elseif tk.value == "break" then self:consume(); return { type = "Break" }
    elseif tk.value == "return" then
        self:consume(); local values = {}
        if self:peek().value ~= "end" and self:peek().value ~= "else" and self:peek().value ~= "elseif" and self:peek().value ~= "until" and self:peek().type ~= "eof" then
            table.insert(values, self:parseExpr()); while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end
        end
        return { type = "Return", values = values }
    elseif tk.value == "goto" then self:consume(); return { type = "Goto", label = self:expect("name").value }
    elseif tk.value == "::" then self:consume(); local name = self:expect("name").value; self:expect("::"); return { type = "Label", name = name }
    elseif tk.value == "do" then self:consume(); local body = self:parseBlock(); self:expect("end"); return body
    else
        local expr = self:parseExpr()
        if self:peek().value == "=" or self:peek().value == "," then
            local vars = { expr }; while self:peek().value == "," do self:consume(); table.insert(vars, self:parseExpr()) end
            self:expect("="); local values = { self:parseExpr() }; while self:peek().value == "," do self:consume(); table.insert(values, self:parseExpr()) end
            return { type = "Assign", vars = vars, values = values }
        else return expr end
    end
end
function Parser:parseArgs()
    self:expect("("); local args = {}
    if self:peek().value ~= ")" then
        if self:peek().value == "..." then table.insert(args, self:consume().value)
        else table.insert(args, self:expect("name").value); while self:peek().value == "," do self:consume(); if self:peek().value == "..." then table.insert(args, self:consume().value); break else table.insert(args, self:expect("name").value) end end end
    end
    self:expect(")"); return args
end
function Parser:parseExpr() return self:parseBinOp(1) end
function Parser:parseBinOp(min_prec)
    local precs = { ["or"]=1, ["and"]=2, ["=="]=3, ["~="]=3, ["<"]=3, [">"]=3, ["<="]=3, [">="]=3, ["|"]=4, ["~"]=5, ["&"]=6, ["<<"]=7, [">>"]=7, [".."]=8, ["+"]=9, ["-"]=9, ["*"]=10, ["/"]=10, ["//"]=10, ["%"]=10 }
    local left = self:parseUnary()
    while true do
        local tk = self:peek(); local p = precs[tk.value]
        if not p or p < min_prec then break end
        local op = self:consume().value; local right = self:parseBinOp(op == ".." and p or p + 1)
        left = { type = "BinaryOp", op = op, left = left, right = right }
    end; return left
end
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
                else table.insert(new_body, stmt) end
            end; n.body = new_body; for _, v in ipairs(n.body) do walk(v) end
        elseif n.type == "If" then walk(n.body); for _, v in ipairs(n.elseifs) do walk(v.body) end; walk(n.elseBlock)
        elseif n.type == "While" or n.type == "FunctionDef" then walk(n.body)
        end
    end; walk(ast)
end
function Obfuscator.flattenControlFlow(ast)
    local function flatten(block)
        if not block or block.type ~= "Block" or #block.body < 2 or block.is_flat then return block end
        local stmts = block.body; local var = gsn(); local dispatcher = { type = "While", cond = { type = "BinaryOp", op = "~=", left = { type = "Var", name = var }, right = { type = "Number", value = "0" } }, body = { type = "Block", body = {} }, is_dispatcher = true }
        local cases = {}; for i, stmt in ipairs(stmts) do
            local next_val = (i < #stmts) and i + 1 or 0
            local body = { type = "Block", body = { stmt, { type = "Assign", vars = { { type = "Var", name = var } }, values = { { type = "Number", value = tostring(next_val) } } } } }
            table.insert(cases, { cond = { type = "BinaryOp", op = "==", left = { type = "Var", name = var }, right = { type = "Number", value = tostring(i) } }, body = body })
        end
        local if_stmt = { type = "If", cond = cases[1].cond, body = cases[1].body, elseifs = {}, elseBlock = nil }
        for i = 2, #cases do table.insert(if_stmt.elseifs, cases[i]) end
        table.insert(dispatcher.body.body, if_stmt)
        return { type = "Block", body = { { type = "LocalAssign", vars = { var }, values = { { type = "Number", value = "1" } } }, dispatcher }, is_flat = true }
    end
    local function walk(n)
        if not n or type(n) ~= "table" then return end
        if n.type == "Block" then
            if not n.is_fake then local nb = flatten(n); n.body = nb.body; n.is_flat = nb.is_flat end
            for _, v in ipairs(n.body) do walk(v) end
        elseif n.type == "If" then walk(n.body); for _, v in ipairs(n.elseifs) do walk(v.body) end; walk(n.elseBlock)
        elseif n.type == "While" or n.type == "FunctionDef" then walk(n.body)
        end
    end; walk(ast); return ast
end
function Obfuscator.applyMBA(ast)
    local function transform(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "BinaryOp" and n.op == "+" and n.left.type == "Number" and n.right.type == "Number" then
            return { type = "BinaryOp", op = "+", left = { type = "BinaryOp", op = "~", left = n.left, right = n.right }, right = { type = "BinaryOp", op = "*", left = { type = "Number", value = "2" }, right = { type = "BinaryOp", op = "&", left = n.left, right = n.right } } }
        end
        for k, v in pairs(n) do if type(v) == "table" then n[k] = transform(v) end end; return n
    end; return transform(ast)
end
function Obfuscator.obfuscateIdentifiers(ast)
    local map = {}; local function gv() local n = "_0x" .. string.format("%X", math.random(0x1000, 0xFFFF)); while map[n] do n = "_0x" .. string.format("%X", math.random(0x1000, 0xFFFF)) end; return n end
    local function walk(n)
        if not n or type(n) ~= "table" then return end
        if n.type == "Var" then if not map[n.name] and not _G[n.name] then map[n.name] = gv() end; if map[n.name] then n.name = map[n.name] end
        elseif n.type == "LocalAssign" then for i, v in ipairs(n.vars) do if not map[v] then map[v] = gv() end; n.vars[i] = map[v] end
        elseif n.type == "LocalFunction" then if not map[n.name] then map[n.name] = gv() end; n.name = map[n.name]
        elseif n.type == "FunctionDef" then for i, v in ipairs(n.args) do if v ~= "..." then if not map[v] then map[v] = gv() end; n.args[i] = map[v] end end
        end; for _, v in pairs(n) do if type(v) == "table" then walk(v) end end
    end; walk(ast); return map
end

local Virtualizer = {}
function Virtualizer.virtualize(ast, gm)
    local XK = math.random(0x100000, 0xFFFFFF)
    local SX1, SX2, SX3 = math.random(1, 255), math.random(1, 255), math.random(1, 255)
    local ops_list = { "LOADK", "GETVAR", "SETVAR", "GETTABLE", "SETTABLE", "SETTABLE_IMM", "SETTABLE_MULTI", "NEWTABLE", "ADD", "SUB", "MUL", "DIV", "MOD", "POW", "UNM", "LEN", "NOT", "BNOT", "BOR", "BXOR", "BAND", "SHL", "SHR", "IDIV", "EQ", "NE", "LT", "GT", "LE", "GE", "CONCAT", "JMP", "JMP_IF_TRUE", "JMP_IF_FALSE", "CALL", "CALL_M", "RET", "RET_M", "VARARG", "VARARG_M", "CLOSURE", "POP", "DUP", "SWAP", "PICK_RESULT", "NOP" }

    local function encrypt_k(v)
        if type(v) == "string" then
            local r = {}
            for i=1, #v do
                local b = v:byte(i)
                b = (b ~ SX1)
                b = (b + SX2 + i) % 256
                b = (b ~ SX3)
                table.insert(r, string.char(b))
            end
            return "s" .. table.concat(r)
        elseif type(v) == "number" then
            local k1 = math.random(100, 999)
            local k2 = math.random(100, 999)
            return "n" .. tostring((v + k1) ~ k2) .. "|" .. k1 .. "|" .. k2
        elseif type(v) == "boolean" then
            return "b" .. (v and "t" or "f")
        elseif v == nil then
            return "x"
        end; return v
    end

    local function pp(block, args)
        local instructions = {}; local constants = { n = 0 }; local function addK(v) for i=1, constants.n do if constants[i] == v then return i end end; constants.n = constants.n + 1; constants[constants.n] = v; return constants.n end
        local function emit(op, arg) table.insert(instructions, { op = op, arg = arg }); return #instructions end
        local locals_map = {}; local loop_exits = {}; local gotos = {}; local labels = {}
        local function walk(n, multi, is_stmt)
            if not n then return end
            if n.type == "Block" then for i, v in ipairs(n.body) do walk(v, false, true) end
            elseif n.type == "LocalAssign" then
                local vars, values = n.vars, n.values or {}
                local use_multi = (#values > 0 and values[#values] and (values[#values].type == "Call" or values[#values].type == "MemberCall" or values[#values].type == "Vararg"))
                for i = 1, #values do if i == #values and use_multi then walk(values[i], true) else walk(values[i]) end end
                if #values < #vars and not use_multi then for i = #values + 1, #vars do emit("LOADK", addK(nil)) end end
                for i = #vars, 1, -1 do local var = vars[i]; local is_multi = (i >= #values and use_multi); if is_multi then emit("PICK_RESULT", i - #values + 1) end; emit("SETVAR", addK(var)) end
                if #values > 0 and values[#values] and (values[#values].type == "Call" or values[#values].type == "MemberCall" or values[#values].type == "Vararg") and #vars >= #values then emit("POP", 1) end
            elseif n.type == "Assign" then
                local vars, values = n.vars, n.values or {}
                local use_multi = (#values > 0 and values[#values] and (values[#values].type == "Call" or values[#values].type == "MemberCall" or values[#values].type == "Vararg"))
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

    local function sp(p)
        local op_map = {}; local id_to_op = {}; local current_id = 1
        local function get_id(op)
            if not op_map[op] then
                op_map[op] = {}; for i=1, math.random(1, 4) do
                    local sid = math.random(1, 255); while id_to_op[sid] do sid = math.random(1, 255) end
                    table.insert(op_map[op], sid); id_to_op[sid] = op
                end
            end
            return op_map[op][math.random(1, #op_map[op])]
        end
        for _, op in ipairs(ops_list) do get_id(op) end
        for i=1, 50 do
            local sid = math.random(1, 255); if not id_to_op[sid] then id_to_op[sid] = "NOP" end
        end
        local PK1, PK2 = math.random(1, 0xFFFFFF), math.random(1, 0xFFFFFF)
        local b = {}; for _, v in ipairs(p.b) do
            local op_id = get_id(v.op)
            local arg = (v.arg or 0)
            local packed = ((op_id & 0xFF) | ((arg & 0xFFFFFF) << 8))
            packed = (packed ~ PK1)
            packed = (packed + PK2) % 4294967296
            packed = (packed ~ XK)
            table.insert(b, packed)
        end
        local ks = { n = p.k.n }; for i = 1, p.k.n do local v = p.k[i]
            if type(v) == "table" and v.b then ks[i] = sp(v) else ks[i] = encrypt_k(v) end
        end
        local ls = {}; for _, v in ipairs(p.l) do table.insert(ls, encrypt_k(v)) end
        return { b = b, k = ks, l = ls, m = id_to_op, x1 = PK1, x2 = PK2 }
    end
    local egm = {}; for k, v in pairs(gm) do egm[k] = encrypt_k(v) end; return sp(pp(ast)), XK, ops_list, egm, {SX1, SX2, SX3}
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
    local function sk(ks) local s = "{"; for i=1, (ks.n or #ks) do local v = ks[i]
        if type(v) == "string" then s = s .. es(v) .. ","
        elseif type(v) == "boolean" or type(v) == "number" then s = s .. tostring(v) .. ","
        elseif type(v) == "table" and v.b then s = s .. Wrapper._sp(v) .. ","
        else s = s .. "nil," end end; return s .. "}" end
    local function sm(m) local s = "{"; for k, v in pairs(m) do s = s .. "[" .. k .. "]=" .. es(v) .. "," end; return s .. "}" end
    function Wrapper._sp(p)
        local h = 0; for i=1, #p.b do h = (h + p.b[i]) % 4294967296 end
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
    local v_b, v_k, v_l, v_m, v_va, v_v = gv("_B"), gv("_K"), gv("_L"), gv("_M"), gv("_VA"), gv("_V")
    local v_env, v_env_map, v_x, v_sx, v_d, v_exec, v_clk, v_pr, v_up = gv("_ENV"), gv("_EM"), gv("_X"), gv("_SX"), gv("_D"), gv("_EXE"), gv("_CLK"), gv("_PR"), gv("_UP")
    local v_nil, v_ah = gv("_NIL"), gv("_AH_CK")
    local dis_id = math.random(1000, 9999)

    local function l(t, k) return v_env_map .. "[" .. es(t) .. "][" .. es(k) .. "]" end
    local function gl(k) return v_env_map .. "[" .. es(k) .. "]" end
    local function sa(idx) return v_stack .. "[" .. idx .. "]" end

    local body = "local " .. v_env_map .. " = " .. gms .. "\\n"
    body = body .. "local " .. v_x .. " = " .. tostring(xk) .. "; local " .. v_sx .. " = " .. sx_literal .. "\\n"
    body = body .. "local " .. v_env .. " = _G or _ENV\\nlocal " .. v_nil .. " = {}\\n"
    body = body .. "local _GSC = (" .. v_env .. "[" .. es("string") .. "] or _G[" .. es("string") .. "])[" .. es("char") .. "]\\n"
    body = body .. "local _GSB = (" .. v_env .. "[" .. es("string") .. "] or _G[" .. es("string") .. "])[" .. es("byte") .. "]\\n"
    body = body .. "local _GUP = (table and table.unpack) or unpack\\n"
    body = body .. "local _GTC = (" .. v_env .. "[" .. es("table") .. "] or _G[" .. es("table") .. "] or {})[" .. es("concat") .. "] or (" .. v_env .. "[" .. es("table") .. "] or _G[" .. es("table") .. "])[" .. es("concat") .. "]\\n"
    body = body .. "local function " .. v_d .. "(_V)\\n"
    body = body .. "    if (" .. v_env .. "[\\\"type\\\"] or _G[\\\"type\\\"])(_V) ~= \\\"string\\\" then return _V end\\n"
    body = body .. "    local _T = _V:sub(1,1); local _VAL = _V:sub(2)\\n"
    body = body .. "    if _T == \\\"s\\\" then \\n"
    body = body .. "        local _R = {}; for _i=1, #_VAL do \\n"
    body = body .. "            local _B = _GSB(_VAL, _i)\\n"
    body = body .. "            _B = (_B ~ " .. v_sx .. "[3])\\n"
    body = body .. "            _B = (_B - (" .. v_sx .. "[2] + _i)) %% 256\\n"
    body = body .. "            _B = (_B ~ " .. v_sx .. "[1])\\n"
    body = body .. "            _R[#_R + 1] = _GSC(_B)\\n"
    body = body .. "        end\\n"
    body = body .. "        return _GTC(_R)\\n"
    body = body .. "    elseif _T == \\\"n\\\" then \\n"
    body = body .. "        local _p1, _p2 = _VAL:find(\\\"|\\\"); local _p3, _p4 = _VAL:find(\\\"|\\\", _p2 + 1)\\n"
    body = body .. "        local _v = tonumber(_VAL:sub(1, _p1 - 1)); local _k1 = tonumber(_VAL:sub(_p2 + 1, _p3 - 1)); local _k2 = tonumber(_VAL:sub(_p4 + 1))\\n"
    body = body .. "        return (_v ~ _k2) - _k1\\n"
    body = body .. "    elseif _T == \\\"b\\\" then return _VAL == \\\"t\\\"\\n"
    body = body .. "    elseif _T == \\\"x\\\" then return nil\\n"
    body = body .. "    end\\n"
    body = body .. "    return nil\\n"
    body = body .. "end\\n"
    body = body .. "local function " .. v_exec .. "(" .. v_pr .. ", " .. v_up .. ", ...)\\n"
    body = body .. "    local function " .. v_ah .. "()\\n"
    body = body .. "        local _ok1, _db = pcall(require, \\\"debug\\\")\\n"
    body = body .. "        if _ok1 and _db then if _db.gethook() or _db.getinfo(50, 'f') then " .. gl("os") .. "[\\\"exit\"]() end end\\n"
    body = body .. "        local _ok2, _io = pcall(require, \"io\")\\n"
    body = body .. "        if _ok2 and _io then\\n"
    body = body .. "            local _f = _io.open(\\\"/proc/self/maps\\\", \\\"r\\\")\\n"
    body = body .. "            if _f then local _m = _f:read(\\\"*a\\\"); _f:close(); if _m:find(\\\"frida\\\") or _m:find(\\\"xposed\\\") then " .. gl("os") .. "[\\\"exit\"]() end end\\n"
    body = body .. "        end\\n"
    body = body .. "        local _ok3, _lj = pcall(function() return luajava end)\\n"
    body = body .. "        if _ok3 and _lj then\\n"
    body = body .. "            pcall(function()\\n"
    body = body .. "                local _File = _lj.bindClass(\\\"java.io.File\\\")\\n"
    body = body .. "                if _File(\\\"/data/local/tmp/frida-server\\\").exists() then " .. gl("os") .. "[\\\"exit\"]() end\\n"
    body = body .. "            end)\\n"
    body = body .. "        end\\n"
    body = body .. "    end\\n"
    body = body .. "    " .. v_ah .. "()\\n"
    body = body .. "    local " .. v_clk .. " = " .. l("os", "clock") .. "(); " .. l("math", "randomseed") .. "(" .. v_pr .. ".s or " .. l("os", "time") .. "())\\n"
    if integrity then
        body = body .. "    if true then\\n"
        body = body .. "        local _H = 0; for _i=1, #" .. v_pr .. ".b do _H = (_H + " .. v_pr .. ".b[_i]) %% 4294967296 end\\n"
        body = body .. "        if _H ~= " .. v_pr .. ".h then " .. gl("error") .. "(\\\"Integrity check failed\\\") end\\n"
        body = body .. "    end\\n"
    end
    body = body .. "    local " .. v_stack .. ", " .. v_ss .. ", " .. v_p .. ", " .. v_st .. ", " .. v_ar .. " = {}, " .. v_pr .. ".r or 0, 1, " .. dis_id .. ", 0\\n"
    body = body .. "    local " .. v_b .. ", " .. v_k .. ", " .. v_l .. ", " .. v_m .. " = " .. v_pr .. ".b, " .. v_pr .. ".k, " .. v_pr .. ".l, " .. v_pr .. ".m; local " .. v_va .. " = " .. l("table", "pack") .. "(...)\\n"
    body = body .. "    local " .. v_v .. " = {}\\n"
    body = body .. "    for _, _n in " .. gl("ipairs") .. "(" .. v_l .. ") do " .. v_v .. "[" .. v_d .. "(_n)] = " .. v_nil .. " end\\n"
    body = body .. "    while (function() local _o = " .. math.random(1, 100) .. "; return _o == _o end)() and (" .. v_st .. " ~ 0) ~= 0 do\\n"
    body = body .. "        if 1 == 2 then\\n"
    body = body .. "        " .. table.concat(vm_flattened, "\n") .. "\\n"
    body = body .. "        else " .. v_st .. " = 0 end\\n"
    body = body .. "    end\\n"
    body = body .. "end\\n"
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
