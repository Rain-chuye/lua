require "import"
import "android.widget.*"
import "android.view.*"

-- AndroLua+ Obfuscator Layout
layout = {
    LinearLayout,
    orientation = "vertical",
    layout_width = "fill",
    layout_height = "fill",
    padding = "16dp",
    backgroundColor = "#F5F5F5",
    {
        TextView,
        text = "Lua AST 混淆工具 (指令虚拟化)",
        textSize = "20sp",
        textColor = "#333333",
        layout_gravity = "center",
        padding = "10dp",
    },
    {
        TextView,
        text = "输入文件路径:",
        textColor = "#666666",
        paddingTop = "10dp",
    },
    {
        EditText,
        id = "edit_input",
        layout_width = "fill",
        hint = "/sdcard/myscript.lua",
        singleLine = true,
    },
    {
        TextView,
        text = "输出文件路径:",
        textColor = "#666666",
        paddingTop = "10dp",
    },
    {
        EditText,
        id = "edit_output",
        layout_width = "fill",
        hint = "/sdcard/myscript_obf.lua",
        singleLine = true,
    },
    {
        Button,
        id = "btn_start",
        text = "开始混淆并加密",
        layout_width = "fill",
        layout_marginTop = "20dp",
        backgroundColor = "#2196F3",
        textColor = "#FFFFFF",
    },
    {
        TextView,
        text = "日志信息:",
        textColor = "#666666",
        paddingTop = "20dp",
    },
    {
        ScrollView,
        layout_width = "fill",
        layout_height = "fill",
        layout_marginTop = "5dp",
        backgroundColor = "#EEEEEE",
        {
            TextView,
            id = "txt_log",
            text = "等待操作...",
            textSize = "12sp",
            padding = "5dp",
        }
    }
}

setContentView(loadlayout(layout))

function log(msg)
    txt_log.append("\n[" .. os.date("%H:%M:%S") .. "] " .. tostring(msg))
end

-- Obfuscator Core Modules
local package_preload = {}

package_preload["lexer"] = function()
    local Lexer = {}
    local keywords = { ["and"] = true, ["break"] = true, ["do"] = true, ["else"] = true, ["elseif"] = true, ["end"] = true, ["false"] = true, ["for"] = true, ["function"] = true, ["goto"] = true, ["if"] = true, ["in"] = true, ["local"] = true, ["nil"] = true, ["not"] = true, ["or"] = true, ["repeat"] = true, ["return"] = true, ["then"] = true, ["true"] = true, ["until"] = true, ["while"] = true }
    function Lexer.new(source) return setmetatable({ source = source, pos = 1, line = 1, tokens = {} }, { __index = Lexer }) end
    function Lexer:peek(n) return self.source:sub(self.pos + (n or 0), self.pos + (n or 0)) end
    function Lexer:consume() local c = self:peek(); if c == "\n" then self.line = self.line + 1 end; self.pos = self.pos + 1; return c end
    function Lexer:tokenize()
        while self.pos <= #self.source do
            local c = self:peek()
            if c:match("%s") then self:consume()
            elseif c == "-" and self:peek(1) == "-" then
                self:consume(); self:consume()
                if self:peek() == "[" and self:peek(1) == "[" then
                    self:consume(); self:consume()
                    while self.pos <= #self.source do
                        if self:peek() == "]" and self:peek(1) == "]" then self:consume(); self:consume(); break end
                        self:consume()
                    end
                else while self.pos <= #self.source and self:peek() ~= "\n" do self:consume() end end
            elseif c:match("[%a_]") then
                local s = self.pos; while self:peek():match("[%w_]") do self:consume() end
                local n = self.source:sub(s, self.pos - 1)
                table.insert(self.tokens, { type = keywords[n] and "keyword" or "name", value = n, line = self.line })
            elseif c:match("%d") or (c == "." and self:peek(1):match("%d")) then
                local s = self.pos
                if c == "0" and (self:peek(1):lower() == "x") then
                    self:consume(); self:consume(); while self:peek():match("[%da-fA-F.]") do self:consume() end
                else
                    while self:peek():match("[%d.]") do self:consume() end
                    if self:peek():lower() == "e" then
                        self:consume(); if self:peek() == "+" or self:peek() == "-" then self:consume() end
                        while self:peek():match("%d") do self:consume() end
                    end
                end
                table.insert(self.tokens, { type = "number", value = self.source:sub(s, self.pos - 1), line = self.line })
            elseif c == "'" or c == "\"" then
                local s = self.pos; local q = self:consume()
                while self.pos <= #self.source and self:peek() ~= q do
                    if self:peek() == "\\" then self:consume() end
                    self:consume()
                end
                self:consume(); table.insert(self.tokens, { type = "string", value = self.source:sub(s + 1, self.pos - 2), line = self.line })
            elseif c == "[" and (self:peek(1) == "[" or self:peek(1) == "=") then
                local s = self.pos; local sep = 0; self:consume()
                while self:peek() == "=" do self:consume(); sep = sep + 1 end
                if self:peek() == "[" then
                    self:consume(); local cs = self.pos
                    while self.pos <= #self.source do
                        if self:peek() == "]" then
                            local m = true; for i = 1, sep do if self:peek(i) ~= "=" then m = false break end end
                            if m and self:peek(sep + 1) == "]" then
                                local content = self.source:sub(cs, self.pos - 1); self:consume()
                                for i = 1, sep do self:consume() end; self:consume()
                                table.insert(self.tokens, { type = "string", value = content, line = self.line }); goto next
                            end
                        end
                        self:consume()
                    end
                else self.pos = s; table.insert(self.tokens, { type = "operator", value = self:consume(), line = self.line }) end
            else
                local t2, t3 = self.source:sub(self.pos, self.pos + 1), self.source:sub(self.pos, self.pos + 2)
                if t3 == "..." then table.insert(self.tokens, { type = "operator", value = "...", line = self.line }); self.pos = self.pos + 3
                elseif t2:match("[=~<>%.%/]%=") or t2 == ".." or t2 == "<<" or t2 == ">>" or t2 == "::" or t2 == "//" then
                    table.insert(self.tokens, { type = "operator", value = t2, line = self.line }); self.pos = self.pos + 2
                else table.insert(self.tokens, { type = "operator", value = self:consume(), line = self.line }) end
            end
            ::next::
        end
        table.insert(self.tokens, { type = "eof", value = "eof", line = self.line }); return self.tokens
    end
    return Lexer
end

package_preload["parser"] = function()
    local Parser = {}
    local prec = { ["or"] = 1, ["and"] = 2, ["<"] = 3, [">"] = 3, ["<="] = 3, [">="] = 3, ["~="] = 3, ["=="] = 3, ["|"] = 4, ["~"] = 5, ["&"] = 6, ["<<"] = 7, [">>"] = 7, [".."] = 8, ["+"] = 9, ["-"] = 9, ["*"] = 10, ["/"] = 10, ["//"] = 10, ["%"] = 10, ["^"] = 12 }
    function Parser.new(tokens) return setmetatable({ tokens = tokens, pos = 1 }, { __index = Parser }) end
    function Parser:peek(n) return self.tokens[self.pos + (n or 0)] end
    function Parser:consume() local t = self:peek(); self.pos = self.pos + 1; return t end
    function Parser:expect(t, v) local tk = self:peek(); if tk.type ~= t or (v and tk.value ~= v) then error("Expected "..t.." but got "..tk.type.." '"..tostring(tk.value).."' at line "..tk.line) end return self:consume() end
    function Parser:accept(t, v) local tk = self:peek(); if tk.type == t and (not v or tk.value == v) then return self:consume() end end
    function Parser:parse() return self:parseBlock() end
    function Parser:parseBlock()
        local body = {}
        while self:peek().type ~= "eof" and not ({["end"]=true,["else"]=true,["elseif"]=true,["until"]=true})[self:peek().value] do
            local s = self:parseStatement(); if s then table.insert(body, s) end
            self:accept("operator", ";")
        end
        return { type = "Block", body = body }
    end
    function Parser:parseStatement()
        local t = self:peek(); if t.type == "keyword" then
            if t.value == "local" then
                self:consume()
                if self:peek().value == "function" then
                    self:consume(); local n = self:expect("name").value; local a = self:parseArgs(); local b = self:parseBlock(); self:expect("keyword", "end")
                    return { type = "LocalFunction", name = n, args = a, body = b }
                else
                    local v = {}; repeat table.insert(v, self:expect("name").value) until not self:accept("operator", ",")
                    local vals = {}; if self:accept("operator", "=") then vals = self:parseExprList() end
                    return { type = "LocalAssign", vars = v, values = vals }
                end
            elseif t.value == "if" then
                self:consume(); local c = self:parseExpr(); self:expect("keyword", "then"); local tb = self:parseBlock(); local eifs = {}
                while self:accept("keyword", "elseif") do local ec = self:parseExpr(); self:expect("keyword", "then"); local eb = self:parseBlock(); table.insert(eifs, { cond = ec, body = eb }) end
                local elb; if self:accept("keyword", "else") then elb = self:parseBlock() end; self:expect("keyword", "end")
                return { type = "If", condition = c, thenBlock = tb, elseifs = eifs, elseBlock = elb }
            elseif t.value == "while" then
                self:consume(); local c = self:parseExpr(); self:expect("keyword", "do"); local b = self:parseBlock(); self:expect("keyword", "end")
                return { type = "While", condition = c, body = b }
            elseif t.value == "repeat" then
                self:consume(); local b = self:parseBlock(); self:expect("keyword", "until"); local c = self:parseExpr()
                return { type = "Repeat", condition = c, body = b }
            elseif t.value == "for" then
                self:consume(); local v = { self:expect("name").value }; if self:accept("operator", "=") then
                    local st = self:parseExpr(); self:expect("operator", ","); local sp = self:parseExpr(); local step; if self:accept("operator", ",") then step = self:parseExpr() end
                    self:expect("keyword", "do"); local b = self:parseBlock(); self:expect("keyword", "end")
                    return { type = "ForRange", var = v[1], start = st, stop = sp, step = step, body = b }
                else
                    while self:accept("operator", ",") do table.insert(v, self:expect("name").value) end; self:expect("keyword", "in"); local i = self:parseExprList(); self:expect("keyword", "do"); local b = self:parseBlock(); self:expect("keyword", "end")
                    return { type = "ForIn", vars = v, iter = i, body = b }
                end
            elseif t.value == "function" then
                self:consume(); local n = self:parseFuncName(); local a = self:parseArgs(); local b = self:parseBlock(); self:expect("keyword", "end")
                return { type = "Function", name = n, args = a, body = b }
            elseif t.value == "return" then
                self:consume(); local vals = {}; if not ({["end"]=true,["else"]=true,["elseif"]=true,["until"]=true})[self:peek().value] and self:peek().type ~= "eof" then vals = self:parseExprList() end
                return { type = "Return", values = vals }
            elseif t.value == "break" then self:consume(); return { type = "Break" }
            elseif t.value == "do" then self:consume(); local b = self:parseBlock(); self:expect("keyword", "end"); return { type = "Do", body = b }
            elseif t.value == "goto" then self:consume(); return { type = "Goto", name = self:expect("name").value }
            end
        elseif t.type == "operator" and t.value == "::" then self:consume(); local n = self:expect("name").value; self:expect("operator", "::"); return { type = "Label", name = n } end
        local p = self:parsePrimaryExpr()
        if self:peek().type == "operator" and (self:peek().value == "," or self:peek().value == "=") then
            local v = { p }; while self:accept("operator", ",") do table.insert(v, self:parsePrimaryExpr()) end; self:expect("operator", "="); local vals = self:parseExprList()
            return { type = "Assign", vars = v, values = vals }
        else if p.type == "Call" or p.type == "MemberCall" then return p else error("Unexpected primary expr at line "..t.line) end end
    end
    function Parser:parseFuncName() local n = self:expect("name").value; while self:accept("operator", ".") do n = n .. "." .. self:expect("name").value end; if self:accept("operator", ":") then n = n .. ":" .. self:expect("name").value end return n end
    function Parser:parseArgs() self:expect("operator", "("); local a = {}; if self:peek().value ~= ")" then repeat if self:accept("operator", "...") then table.insert(a, "..."); break end; table.insert(a, self:expect("name").value) until not self:accept("operator", ",") end; self:expect("operator", ")"); return a end
    function Parser:parseExprList() local e = { self:parseExpr() }; while self:accept("operator", ",") do table.insert(e, self:parseExpr()) end return e end
    function Parser:parseExpr() return self:parseSubExpr(0) end
    function Parser:parseSubExpr(mp)
        local l = self:parseUnaryExpr()
        while true do local op = self:peek().value; local p = prec[op]; if not p or p <= mp then break end; self:consume(); local r = self:parseSubExpr(op == "^" and p-1 or p); l = { type = "BinaryOp", op = op, left = l, right = r } end
        return l
    end
    function Parser:parseUnaryExpr()
        local op = self:peek().value; if op == "not" or op == "#" or op == "-" or op == "~" then self:consume(); return { type = "UnaryOp", op = op, operand = self:parseUnaryExpr() } end
        return self:parsePrimaryExpr()
    end
    function Parser:parsePrimaryExpr()
        local node; local t = self:peek()
        if t.type == "number" then node = { type = "Number", value = self:consume().value }
        elseif t.type == "string" then node = { type = "String", value = self:consume().value }
        elseif t.type == "keyword" then
            if t.value == "nil" then self:consume(); node = { type = "Nil" }
            elseif t.value == "true" then self:consume(); node = { type = "Boolean", value = true }
            elseif t.value == "false" then self:consume(); node = { type = "Boolean", value = false }
            elseif t.value == "function" then self:consume(); local a = self:parseArgs(); local b = self:parseBlock(); self:expect("keyword", "end"); node = { type = "FunctionDef", args = a, body = b }
            else error("Unexpected keyword "..t.value) end
        elseif t.type == "operator" then
            if t.value == "(" then self:consume(); node = self:parseExpr(); self:expect("operator", ")")
            elseif t.value == "{" then node = self:parseTable()
            elseif t.value == "..." then self:consume(); node = { type = "Vararg" }
            else error("Unexpected operator "..t.value) end
        elseif t.type == "name" then node = { type = "Var", name = self:consume().value }
        else error("Unexpected token "..t.type) end
        while true do
            local tk = self:peek(); if tk.type == "operator" then
                if tk.value == "." then self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } }
                elseif tk.value == "[" then self:consume(); local k = self:parseExpr(); self:expect("operator", "]"); node = { type = "Index", table = node, key = k }
                elseif tk.value == ":" then self:consume(); local n = self:expect("name").value; local a = self:parseCallArgs(); node = { type = "MemberCall", table = node, member = n, args = a }
                elseif tk.value == "(" or tk.value == "{" or tk.type == "string" then local a = self:parseCallArgs(); node = { type = "Call", func = node, args = a }
                else break end
            elseif tk.type == "string" or (tk.type == "operator" and tk.value == "{") then local a = self:parseCallArgs(); node = { type = "Call", func = node, args = a }
            else break end
        end
        return node
    end
    function Parser:parseTable() self:expect("operator", "{"); local f = {}; while self:peek().value ~= "}" do local fi = {}; if self:accept("operator", "[") then fi.key = self:parseExpr(); self:expect("operator", "]"); self:expect("operator", "="); fi.value = self:parseExpr() elseif self:peek().type == "name" and self:peek(1).value == "=" then fi.key = { type = "String", value = self:consume().value }; self:expect("operator", "="); fi.value = self:parseExpr() else fi.value = self:parseExpr() end; table.insert(f, fi); if not self:accept("operator", ",") and not self:accept("operator", ";") then break end end; self:expect("operator", "}"); return { type = "Table", fields = f } end
    function Parser:parseCallArgs() local t = self:peek(); if t.value == "(" then self:consume(); local a = {}; if self:peek().value ~= ")" then a = self:parseExprList() end; self:expect("operator", ")"); return a elseif t.value == "{" then return { self:parseTable() } elseif t.type == "string" then return { { type = "String", value = self:consume().value } } else error("Expected call args") end end
    return Parser
end

package_preload["obfuscator"] = function()
    local Obfuscator = {}
    local function gn(c) return string.format("_0x%X", c + 0xABCDEF) end
    function Obfuscator.desugar(ast)
        local function walk(n) if not n or type(n) ~= "table" then return n end
            if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v) end
            elseif n.type == "ForRange" then
                local step = n.step or { type = "Number", value = "1" }
                local start_stmt = { type = "LocalAssign", vars = { n.var }, values = { walk(n.start) } }
                local cond_expr = { type = "BinaryOp", op = "<=", left = { type = "Var", name = n.var }, right = walk(n.stop) }
                local update_stmt = { type = "Assign", vars = { { type = "Var", name = n.var } }, values = { { type = "BinaryOp", op = "+", left = { type = "Var", name = n.var }, right = walk(step) } } }
                return walk({ type = "Block", body = { start_stmt, { type = "While", condition = cond_expr, body = { type = "Block", body = { walk(n.body), update_stmt } } } } })
            elseif n.type == "ForIn" then
                local init_stmt = { type = "LocalAssign", vars = { "_f", "_s", "_var" }, values = n.iter }; for i, v in ipairs(init_stmt.values) do init_stmt.values[i] = walk(v) end
                local loop_vars_stmt = { type = "LocalAssign", vars = n.vars, values = { { type = "Call", func = { type = "Var", name = "_f" }, args = { { type = "Var", name = "_s" }, { type = "Var", name = "_var" } } } } }
                local break_if = { type = "If", condition = { type = "BinaryOp", op = "==", left = { type = "Var", name = n.vars[1] }, right = { type = "Nil" } }, thenBlock = { type = "Block", body = { { type = "Break" } } }, elseifs = {} }
                local update_var = { type = "Assign", vars = { { type = "Var", name = "_var" } }, values = { { type = "Var", name = n.vars[1] } } }
                return walk({ type = "Block", body = { init_stmt, { type = "While", condition = { type = "Boolean", value = true }, body = { type = "Block", body = { loop_vars_stmt, break_if, update_var, walk(n.body) } } } } })
            elseif n.type == "If" then n.condition = walk(n.condition); n.thenBlock = walk(n.thenBlock); for _, v in ipairs(n.elseifs) do v.cond = walk(v.cond); v.body = walk(v.body) end; if n.elseBlock then n.elseBlock = walk(n.elseBlock) end
            elseif n.type == "While" or n.type == "Repeat" then n.condition = walk(n.condition); n.body = walk(n.body)
            elseif n.type == "Do" then n.body = walk(n.body)
            elseif n.type == "LocalFunction" or n.type == "Function" or n.type == "FunctionDef" then n.body = walk(n.body)
            elseif n.type == "LocalAssign" or n.type == "Assign" then if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
            elseif n.type == "Call" or n.type == "MemberCall" then if n.func then n.func = walk(n.func) end; if n.table then n.table = walk(n.table) end; for i, v in ipairs(n.args) do n.args[i] = walk(v) end
            elseif n.type == "BinaryOp" then n.left = walk(n.left); n.right = walk(n.right)
            elseif n.type == "UnaryOp" then n.operand = walk(n.operand)
            elseif n.type == "Index" then n.table = walk(n.table); n.key = walk(n.key)
            elseif n.type == "Table" then for _, f in ipairs(n.fields) do if f.key then f.key = walk(f.key) end; f.value = walk(f.value) end
            elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
            return n
        end
        return walk(ast)
    end
    function Obfuscator.obfuscateIdentifiers(ast)
        local c = 0; local gm = {}; local gr = {}; local function gnn() c = c + 1; return gn(c) end
        local function walk(n, s) if not n or type(n) ~= "table" then return end
            if n.type == "Block" then local ns = { parent = s, locals = {} }; for _, v in ipairs(n.body) do walk(v, ns) end
            elseif n.type == "LocalAssign" then for i, v in ipairs(n.vars) do local nn = gnn(); s.locals[v] = nn; n.vars[i] = nn end; if n.values then for _, v in ipairs(n.values) do walk(v, s) end end
            elseif n.type == "Assign" then if n.vars then for _, v in ipairs(n.vars) do walk(v, s) end end; if n.values then for _, v in ipairs(n.values) do walk(v, s) end end
            elseif n.type == "Var" then local cur = s; local f = false; while cur do if cur.locals[n.name] then n.name = cur.locals[n.name]; f = true; break end; cur = cur.parent end; if not f then if not gr[n.name] then gr[n.name] = gnn(); gm[gr[n.name]] = n.name end; n.name = gr[n.name] end
            elseif n.type == "LocalFunction" then local nn = gnn(); s.locals[n.name] = nn; n.name = nn; local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args) do if v ~= "..." then local an = gnn(); fs.locals[v] = an; n.args[i] = an end end; walk(n.body, fs)
            elseif n.type == "Function" then if not n.name:find("[%.:]") then local cur = s; local f = false; while cur do if cur.locals[n.name] then n.name = cur.locals[n.name]; f = true; break end; cur = cur.parent end; if not f then if not gr[n.name] then gr[n.name] = gnn(); gm[gr[n.name]] = n.name end; n.name = gr[n.name] end end; local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args) do if v ~= "..." then local an = gnn(); fs.locals[v] = an; n.args[i] = an end end; walk(n.body, fs)
            elseif n.type == "FunctionDef" then local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args) do if v ~= "..." then local an = gnn(); fs.locals[v] = an; n.args[i] = an end end; walk(n.body, fs)
            elseif n.type == "If" then walk(n.condition, s); walk(n.thenBlock, s); for _, v in ipairs(n.elseifs) do walk(v.cond, s); walk(v.body, s) end; if n.elseBlock then walk(n.elseBlock, s) end
            elseif n.type == "While" or n.type == "Repeat" then walk(n.condition, s); walk(n.body, s)
            elseif n.type == "Do" then walk(n.body, s)
            elseif n.type == "Call" or n.type == "MemberCall" then walk(n.func or n.table, s); if n.args then for _, v in ipairs(n.args) do walk(v, s) end end
            elseif n.type == "BinaryOp" then walk(n.left, s); walk(n.right, s)
            elseif n.type == "UnaryOp" then walk(n.operand, s)
            elseif n.type == "Index" then walk(n.table, s); walk(n.key, s)
            elseif n.type == "Table" then for _, v in ipairs(n.fields) do if v.key then walk(v.key, s) end; walk(v.value, s) end
            elseif n.type == "Return" then for _, v in ipairs(n.values) do walk(v, s) end end
        end
        walk(ast, nil); return gm
    end
    function Obfuscator.injectFakeBranches(ast)
        local function cp() local r = math.random(1, 4)
            if r == 1 then return { type = "BinaryOp", op = "==", left = { type = "Call", func = { type = "Var", name = "type" }, args = { { type = "Var", name = "print" } } }, right = { type = "String", value = "function" } }
            elseif r == 2 then return { type = "BinaryOp", op = "~=", left = { type = "Table", fields = {} }, right = { type = "Table", fields = {} } }
            elseif r == 3 then return { type = "BinaryOp", op = "<", left = { type = "Call", func = { type = "Index", table = { type = "Var", name = "math" }, key = { type = "String", value = "abs" } }, args = { { type = "Call", func = { type = "Index", table = { type = "Var", name = "math" }, key = { type = "String", value = "sin" } }, args = { { type = "Index", table = { type = "Var", name = "math" }, key = { type = "String", value = "pi" } } } } } }, right = { type = "Number", value = "0.01" } }
            else return { type = "BinaryOp", op = "==", left = { type = "Call", func = { type = "Var", name = "tonumber" }, args = { { type = "String", value = "123" } } }, right = { type = "Number", value = "123" } } end
        end
        local function walk(n) if not n or type(n) ~= "table" then return end
            if n.type == "Block" then local nb = {}; for _, v in ipairs(n.body) do walk(v); if math.random() < 0.1 then table.insert(nb, { type = "If", condition = cp(), thenBlock = { type = "Block", body = { v } }, elseifs = {}, elseBlock = { type = "Block", body = { { type = "Call", func = { type = "Var", name = "print" }, args = { { type = "String", value = "Inaccessible branch" } } } } } }) else table.insert(nb, v) end end; n.body = nb
            elseif n.type == "If" then walk(n.thenBlock); for _, v in ipairs(n.elseifs) do walk(v.body) end; if n.elseBlock then walk(n.elseBlock) end
            elseif n.type == "While" or n.type == "Repeat" or n.type == "Do" or n.type == "Function" or n.type == "LocalFunction" or n.type == "FunctionDef" then walk(n.body) end
        end
        walk(ast)
    end
    function Obfuscator.flattenControlFlow(ast)
        local function hasBreakOrReturn(node)
            local found = false
            local function check(n)
                if not n or type(n) ~= "table" or found then return end
                if n.type == "Break" or n.type == "Return" then found = true return end
                for _, v in pairs(n) do if type(v) == "table" then check(v) end end
            end
            check(node); return found
        end
        local function walk(n) if not n or type(n) ~= "table" then return end
            if n.type == "Block" then
                if #n.body > 1 and not hasBreakOrReturn(n) then
                    local lth = {}; local ns = {}; for _, v in ipairs(n.body) do
                        if v.type == "LocalAssign" then for _, vv in ipairs(v.vars) do table.insert(lth, vv) end; if #v.values > 0 then local vs = {}; for _, vv in ipairs(v.vars) do table.insert(vs, { type = "Var", name = vv }) end; table.insert(ns, { type = "Assign", vars = vs, values = v.values }) end
                        elseif v.type == "LocalFunction" then table.insert(lth, v.name); table.insert(ns, { type = "Assign", vars = { { type = "Var", name = v.name } }, values = { { type = "FunctionDef", args = v.args, body = v.body } } })
                        else table.insert(ns, v) end
                    end
                    local states = {}; for i, v in ipairs(ns) do walk(v); table.insert(states, { id = i, stmt = v, next = (i < #ns) and (i + 1) or 0 }) end
                    local eifs = {}; for i = 2, #states do table.insert(eifs, { cond = { type = "BinaryOp", op = "==", left = { type = "Var", name = "state" }, right = { type = "Number", value = states[i].id } }, body = { type = "Block", body = { states[i].stmt, { type = "Assign", vars = { { type = "Var", name = "state" } }, values = { { type = "Number", value = states[i].next } } } } } }) end
                    local fb = { { type = "LocalAssign", vars = { "state" }, values = { { type = "Number", value = 1 } } } }; if #lth > 0 then table.insert(fb, 1, { type = "LocalAssign", vars = lth, values = {} }) end
                    local wb = { { type = "If", condition = { type = "BinaryOp", op = "==", left = { type = "Var", name = "state" }, right = { type = "Number", value = 1 } }, thenBlock = { type = "Block", body = { states[1].stmt, { type = "Assign", vars = { { type = "Var", name = "state" } }, values = { { type = "Number", value = states[1].next } } } } }, elseifs = eifs, elseBlock = nil } }
                    local ob = {}; for _, b in ipairs(fb) do table.insert(ob, b) end; table.insert(ob, { type = "While", condition = { type = "BinaryOp", op = "~=", left = { type = "Var", name = "state" }, right = { type = "Number", value = 0 } }, body = { type = "Block", body = wb } })
                    n.body = { { type = "Do", body = { type = "Block", body = ob } } }
                else for _, v in ipairs(n.body) do walk(v) end end
            elseif n.type == "If" then walk(n.thenBlock); for _, v in ipairs(n.elseifs) do walk(v.body) end; if n.elseBlock then walk(n.elseBlock) end
            elseif n.type == "While" or n.type == "Repeat" or n.type == "Do" or n.type == "Function" or n.type == "LocalFunction" or n.type == "FunctionDef" then walk(n.body) end
        end
        walk(ast)
    end
    return Obfuscator
end

package_preload["virtualizer"] = function()
    local Virtualizer = {}
    function Virtualizer.virtualize(ast)
        local function pp(node, args)
            local constants = {}; local function addK(v) for i, k in ipairs(constants) do if k == v then return i end end; table.insert(constants, v); return #constants end
            local instructions = {}; local function emit(op, arg) table.insert(instructions, { op = op, arg = arg }); return #instructions end
            local break_stack = {}; local label_map = {}; local goto_stack = {}
            if args then for i, v in ipairs(args) do if v ~= "..." then emit("LOAD_VA", i); emit("SETVAR", addK(v)) end end end
            local function walk(n, multi) if not n then return end
                if n.type == "Block" then for _, v in ipairs(n.body) do walk(v) end
                elseif n.type == "LocalAssign" or n.type == "Assign" then
                    local values, vars = n.values or {}, n.vars or {}
                    if #values == 1 and #vars > 1 and (values[1].type == "Call" or values[1].type == "MemberCall" or values[1].type == "Vararg") then
                        walk(values[1], true); for i = 1, #vars do emit("PICK_RESULT", i); local var = vars[i]
                        if type(var) == "table" and var.type == "Index" then walk(var.table); walk(var.key); emit("SETTABLE", 0)
                        elseif type(var) == "table" and var.type == "Var" then emit("SETVAR", addK(var.name)) else emit("SETVAR", addK(var)) end end; emit("POP", 1)
                    else for i = 1, #vars do
                        if i <= #values then local is_last = (i == #values)
                            if is_last and i < #vars and (values[i].type == "Call" or values[i].type == "MemberCall" or values[i].type == "Vararg") then
                                walk(values[i], true); for j = i, #vars do emit("PICK_RESULT", j - i + 1); local var = vars[j]
                                if type(var) == "table" and var.type == "Index" then walk(var.table); walk(var.key); emit("SETTABLE", 0)
                                elseif type(var) == "table" and var.type == "Var" then emit("SETVAR", addK(var.name)) else emit("SETVAR", addK(var)) end end; emit("POP", 1); break
                            else walk(values[i]) end
                        else emit("LOADK", addK(nil)) end
                        local var = vars[i]; if type(var) == "table" and var.type == "Index" then walk(var.table); walk(var.key); emit("SETTABLE", 0)
                        elseif type(var) == "table" and var.type == "Var" then emit("SETVAR", addK(var.name)) else emit("SETVAR", addK(var)) end end
                    end
                elseif n.type == "Var" then emit("GETVAR", addK(n.name))
                elseif n.type == "Number" then emit("LOADK", addK(tonumber(n.value)))
                elseif n.type == "String" or n.type == "Boolean" or n.type == "Nil" then emit("LOADK", addK(n.value))
                elseif n.type == "BinaryOp" then walk(n.left); walk(n.right); emit("BINOP", addK(n.op))
                elseif n.type == "UnaryOp" then walk(n.operand); emit("UNOP", addK(n.op))
                elseif n.type == "Call" then walk(n.func); if n.args then for i, v in ipairs(n.args) do if i == #n.args and v.type == "Vararg" then walk(v, true) else walk(v) end end end; if multi or (n.args and n.args[#n.args] and n.args[#n.args].type == "Vararg") then emit("CALL_M", n.args and #n.args or 0) else emit("CALL", n.args and #n.args or 0) end
                elseif n.type == "MemberCall" then walk(n.table); emit("LOADK", addK(n.member)); emit("GETTABLE", 0); walk(n.table); if n.args then for i, v in ipairs(n.args) do if i == #n.args and v.type == "Vararg" then walk(v, true) else walk(v) end end end; if multi or (n.args and n.args[#n.args] and n.args[#n.args].type == "Vararg") then emit("CALL_M", (n.args and #n.args or 0) + 1) else emit("CALL", (n.args and #n.args or 0) + 1) end
                elseif n.type == "If" then
                    local ej = {}; walk(n.condition); local jn = emit("JMP_IF_FALSE", 0); walk(n.thenBlock); table.insert(ej, emit("JMP", 0)); instructions[jn].arg = #instructions + 1
                    if n.elseifs then for _, v in ipairs(n.elseifs) do walk(v.cond); jn = emit("JMP_IF_FALSE", 0); walk(v.body); table.insert(ej, emit("JMP", 0)); instructions[jn].arg = #instructions + 1 end end
                    if n.elseBlock then walk(n.elseBlock) end; for _, v in ipairs(ej) do instructions[v].arg = #instructions + 1 end
                elseif n.type == "While" then
                    local si = #instructions + 1; table.insert(break_stack, {})
                    walk(n.condition); local jn = emit("JMP_IF_FALSE", 0); walk(n.body); emit("JMP", si); instructions[jn].arg = #instructions + 1
                    local bts = table.remove(break_stack); for _, b in ipairs(bts) do instructions[b].arg = #instructions + 1 end
                elseif n.type == "Repeat" then
                    local si = #instructions + 1; table.insert(break_stack, {})
                    walk(n.body); walk(n.condition); emit("JMP_IF_FALSE", si)
                    local bts = table.remove(break_stack); for _, b in ipairs(bts) do instructions[b].arg = #instructions + 1 end
                elseif n.type == "Break" then table.insert(break_stack[#break_stack], emit("JMP", 0))
                elseif n.type == "Return" then if #n.values == 1 and (n.values[1].type == "Call" or n.values[1].type == "MemberCall" or n.values[1].type == "Vararg") then walk(n.values[1], true); emit("RET_M", 0) else for _, v in ipairs(n.values) do walk(v) end; emit("RET", #n.values) end
                elseif n.type == "Do" then walk(n.body)
                elseif n.type == "Label" then label_map[n.name] = #instructions + 1
                elseif n.type == "Goto" then table.insert(goto_stack, { name = n.name, inst = emit("JMP", 0) })
                elseif n.type == "Index" then walk(n.table); walk(n.key); emit("GETTABLE", 0)
                elseif n.type == "Table" then
                    emit("NEWTABLE", 0)
                    for i, v in ipairs(n.fields) do if v.key then walk(v.key); walk(v.value); emit("SETTABLE_IMM", 0) else if (i == #n.fields) and (v.value.type == "Call" or v.value.type == "MemberCall" or v.value.type == "Vararg") then walk(v.value, true); emit("SETTABLE_MULTI", 0) else walk(v.value); emit("SETTABLE_IMM", i) end end end
                elseif n.type == "Vararg" then
                    local sidx = 1; if args then for i, v in ipairs(args) do if v == "..." then sidx = i; break end end end
                    if multi then emit("VARARG_M", sidx) else emit("VARARG", sidx) end
                elseif n.type == "FunctionDef" or n.type == "LocalFunction" or n.type == "Function" then local proto = pp(n.body, n.args); emit("CLOSURE", addK(proto)); if n.type == "LocalFunction" or n.type == "Function" then emit("SETVAR", addK(n.name)) end
                end
            end
            walk(node); emit("RET", 0)
            for _, g in ipairs(goto_stack) do if label_map[g.name] then instructions[g.inst].arg = label_map[g.name] end end
            return { instructions = instructions, constants = constants }
        end
        local mb = pp(ast); local ops = { "LOADK", "GETVAR", "SETVAR", "BINOP", "CALL", "CALL_M", "JMP", "JMP_IF_FALSE", "RET", "RET_M", "SETTABLE", "GETTABLE", "UNOP", "BREAK", "NEWTABLE", "SETTABLE_IMM", "SETTABLE_MULTI", "VARARG", "VARARG_M", "CLOSURE", "LOAD_VA", "PICK_RESULT", "POP" }
        local om = {}; local sh = {}; for i = 1, #ops do sh[i] = i end; for i = #sh, 2, -1 do local j = math.random(i); sh[i], sh[j] = sh[j], sh[i] end; for i, v in ipairs(ops) do om[v] = sh[i] end
        local XK = math.random(1, 0xFFFFFF)
        local function sp(p)
            local b = {}; for _, v in ipairs(p.instructions) do table.insert(b, (om[v.op] or 0) ~ (XK % 0x100)); local a = v.arg or 0; table.insert(b, a ~ XK) end
            local ks = {}; for _, v in ipairs(p.constants) do if type(v) == "table" and v.instructions then table.insert(ks, sp(v)) else table.insert(ks, v) end end
            return { b = b, k = ks }
        end
        return sp(mb), om, XK
    end
    return Virtualizer
end

package_preload["wrapper"] = function()
    local Wrapper = {}
    function Wrapper.generate(main_proto, gm, om, xk)
        local function sk(ks) local s = "{"; for _, v in ipairs(ks) do if type(v) == "string" then s = s .. string.format("%q", v) .. "," elseif type(v) == "boolean" or type(v) == "number" then s = s .. tostring(v) .. "," elseif type(v) == "table" and v.b then s = s .. Wrapper._sp(v) .. "," else s = s .. "nil," end end; return s .. "}" end
        function Wrapper._sp(p) return string.format("{b={%s}, k=%s}", table.concat(p.b, ","), sk(p.k)) end
        local ms = Wrapper._sp(main_proto); local gms = "{"; for k, v in pairs(gm) do gms = gms .. string.format("[%q] = %q,", k, v) end; gms = gms .. "}"
        local ocs = {}; local rm = {}; for k, v in pairs(om) do rm[v] = k end
        for i = 1, #rm do local op = rm[i]; local c = ""
            if op == "LOADK" then c = "_S[#_S+1] = _K[_AR]; _ST = 1"
            elseif op == "GETVAR" then c = [[local _N = _K[_AR]; local _VAL = _V[_N]; if _VAL == nil and _UP then local _curr = _UP; while _curr do if _curr.v[_N] ~= nil then _VAL = _curr.v[_N]; break end; _curr = _curr.up end end; if _VAL == nil then _VAL = _G[_ENV_MAP[_N] or _N] end; _S[#_S+1] = _VAL; _ST = 1]]
            elseif op == "SETVAR" then c = [[local _N = _K[_AR]; local _VAL = _S[#_S]; _S[#_S] = nil; local _found = false; if _V[_N] ~= nil then _V[_N] = _VAL; _found = true elseif _UP then local _curr = _UP; while _curr do if _curr.v[_N] ~= nil then _curr.v[_N] = _VAL; _found = true; break end; _curr = _curr.up end end; if not _found then _G[_ENV_MAP[_N] or _N] = _VAL end; _ST = 1]]
            elseif op == "BINOP" then c = [[local _R = _S[#_S]; _S[#_S] = nil; local _L = _S[#_S]; _S[#_S] = nil; local _ON = _K[_AR]; if _ON == "+" then _S[#_S+1] = _L + _R elseif _ON == "-" then _S[#_S+1] = _L - _R elseif _ON == "*" then _S[#_S+1] = _L * _R elseif _ON == "/" then _S[#_S+1] = _L / _R elseif _ON == ".." then _S[#_S+1] = _L .. _R elseif _ON == "==" then _S[#_S+1] = _L == _R elseif _ON == "~=" then _S[#_S+1] = _L ~= _R elseif _ON == "<" then _S[#_S+1] = _L < _R elseif _ON == ">" then _S[#_S+1] = _L > _R elseif _ON == "<=" then _S[#_S+1] = _L <= _R elseif _ON == ">=" then _S[#_S+1] = _L >= _R elseif _ON == "%" then _S[#_S+1] = _L % _R elseif _ON == "&" then _S[#_S+1] = _L & _R elseif _ON == "|" then _S[#_S+1] = _L | _R elseif _ON == "~" then _S[#_S+1] = _L ~ _R elseif _ON == "<<" then _S[#_S+1] = _L << _R elseif _ON == ">>" then _S[#_S+1] = _L >> _R elseif _ON == "//" then _S[#_S+1] = _L // _R end; _ST = 1]]
            elseif op == "UNOP" then c = [[local _O = _S[#_S]; _S[#_S] = nil; local _ON = _K[_AR]; if _ON == "-" then _S[#_S+1] = -_O elseif _ON == "not" then _S[#_S+1] = not _O elseif _ON == "#" then _S[#_S+1] = #_O elseif _ON == "~" then _S[#_S+1] = ~_O end; _ST = 1]]
            elseif op == "CALL" then c = [[local _AS = {}; for _i = 1, _AR do local _V = _S[#_S]; _S[#_S] = nil; if type(_V) == "table" and _V._M then for _j = _V.n, 1, -1 do table.insert(_AS, 1, _V[_j]) end else table.insert(_AS, 1, _V) end end; local _F = _S[#_S]; _S[#_S] = nil; if not _F then error("Attempt to call nil function") end; local _RE = table.pack(_F(table.unpack(_AS))); _S[#_S+1] = _RE[1]; _ST = 1]]
            elseif op == "CALL_M" then c = [[local _AS = {}; for _i = 1, _AR do local _V = _S[#_S]; _S[#_S] = nil; if type(_V) == "table" and _V._M then for _j = _V.n, 1, -1 do table.insert(_AS, 1, _V[_j]) end else table.insert(_AS, 1, _V) end end; local _F = _S[#_S]; _S[#_S] = nil; if not _F then error("Attempt to call nil function") end; local _RE = table.pack(_F(table.unpack(_AS))); _RE._M = true; _S[#_S+1] = _RE; _ST = 1]]
            elseif op == "JMP" then c = "_P = (_AR - 1) * 2 + 1; _ST = 1"
            elseif op == "JMP_IF_FALSE" then c = "local _C = _S[#_S]; _S[#_S] = nil; if not _C then _P = (_AR - 1) * 2 + 1 end; _ST = 1"
            elseif op == "RET" then c = "local _RE = {}; for _i=1, _AR do _RE[_AR-_i+1] = _S[#_S]; _S[#_S] = nil end; return table.unpack(_RE)"
            elseif op == "RET_M" then c = "local _RE = _S[#_S]; _S[#_S] = nil; return table.unpack(_RE, 1, _RE.n)"
            elseif op == "NEWTABLE" then c = "_S[#_S+1] = {}; _ST = 1"
            elseif op == "SETTABLE_IMM" then c = [[local _V = _S[#_S]; _S[#_S] = nil; if type(_V) == "table" and _V._M then local _T = _S[#_S]; for _j = 1, _V.n do _T[#_T+1] = _V[_j] end else if _AR == 0 then local _K = _S[#_S]; _S[#_S] = nil; local _T = _S[#_S]; _T[_K] = _V else local _T = _S[#_S]; _T[_AR] = _V end end; _ST = 1]]
            elseif op == "SETTABLE_MULTI" then c = [[local _V = _S[#_S]; _S[#_S] = nil; local _T = _S[#_S]; if type(_V) == "table" and _V._M then for _j = 1, _V.n do table.insert(_T, _V[_j]) end else table.insert(_T, _V) end; _ST = 1]]
            elseif op == "SETTABLE" then c = "local _K = _S[#_S]; _S[#_S] = nil; local _T = _S[#_S]; _S[#_S] = nil; local _V = _S[#_S]; _S[#_S] = nil; _T[_K] = _V; _ST = 1"
            elseif op == "GETTABLE" then c = "local _K = _S[#_S]; _S[#_S] = nil; local _T = _S[#_S]; _S[#_S] = nil; _S[#_S+1] = _T[_K]; _ST = 1"
            elseif op == "VARARG" then c = [[local _PK = {}; for _i=_AR, _VA.n do table.insert(_PK, _VA[_i]) end; _S[#_S+1] = _PK[1]; _ST = 1]]
            elseif op == "VARARG_M" then c = [[local _PK = {n=0}; for _i=_AR, _VA.n do _PK.n = _PK.n + 1; _PK[_PK.n] = _VA[_i] end; _PK._M = true; _S[#_S+1] = _PK; _ST = 1]]
            elseif op == "LOAD_VA" then c = "_S[#_S+1] = _VA[_AR]; _ST = 1"
            elseif op == "PICK_RESULT" then c = [[local _R = _S[#_S]; if not _R then error("PICK_RESULT: stack top is nil") end; _S[#_S+1] = _R[_AR]; _ST = 1]]
            elseif op == "POP" then c = "for _i=1, _AR do _S[#_S] = nil end; _ST = 1"
            elseif op == "CLOSURE" then c = "local _PR = _K[_AR]; _S[#_S+1] = function(...) return _EXEC(_PR, {v=_V, up=_UP}, ...) end; _ST = 1"
            else c = "_ST = 1" end
            table.insert(ocs, string.format([[
        elseif _ST == %d then -- %s
            %s]], i + 1, op or "NONE", c))
        end
        return string.format([[
local _ENV_MAP = %s
local _MAIN = %s
local _X = %d
local _G = _G or _ENV
local function _EXEC(_PR, _UP, ...)
    local _S, _P, _V, _ST, _OP, _AR = {}, 1, {}, 1
    local _B, _K = _PR.b, _PR.k
    local _VA = table.pack(...)
    while _ST ~= 0 do
        if _ST == 1 then
            if _P > #_B then _ST = 0
            else
                _OP = _B[_P] ~ (_X %% 0x100); _AR = _B[_P+1] ~ _X; _P = _P + 2
                _ST = _OP + 1
            end
        %s
        else _ST = 0 end
    end
end
return _EXEC(_MAIN, nil, ...)]], gms, ms, xk, table.concat(ocs, ""))
    end
    return Wrapper
end

-- Helper to require internal modules
local function require_int(name)
    return package_preload[name]()
end

local Lexer = require_int("lexer")
local Parser = require_int("parser")
local Obfuscator = require_int("obfuscator")
local Virtualizer = require_int("virtualizer")
local Wrapper = require_int("wrapper")

btn_start.onClick = function()
    local input_file = tostring(edit_input.text)
    local output_file = tostring(edit_output.text)
    if input_file == "" or output_file == "" then
        print("请输入路径")
        return
    end

    local ok, err = pcall(function()
        log("开始混淆: " .. input_file)
        local f = io.open(input_file, "r")
        if not f then error("无法打开输入文件") end
        local source = f:read("*a")
        f:close()

        math.randomseed(os.time())

        log("步骤 1: 解析源码...")
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()
        local parser = Parser.new(tokens)
        local ast = parser:parse()

        log("步骤 2: 脱糖处理...")
        ast = Obfuscator.desugar(ast)

        log("步骤 3: 控制流平展...")
        Obfuscator.flattenControlFlow(ast)

        log("步骤 4: 注入假分支...")
        Obfuscator.injectFakeBranches(ast)

        log("步骤 5: 标识符混淆...")
        local global_map = Obfuscator.obfuscateIdentifiers(ast)

        log("步骤 6: 指令虚拟化...")
        local main_proto, op_map, xor_key = Virtualizer.virtualize(ast)

        log("步骤 7: 生成输出文件...")
        local final_code = Wrapper.generate(main_proto, global_map, op_map, xor_key)

        local out = io.open(output_file, "w")
        if not out then error("无法写入输出文件") end
        out:write(final_code)
        out:close()

        log("成功! 已保存至: " .. output_file)
        print("混淆完成")
    end)

    if not ok then
        log("失败: " .. tostring(err))
        print("混淆失败")
    end
end
