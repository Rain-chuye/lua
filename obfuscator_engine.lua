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
            while self:peek():match("[%d%.eE%+%-]") do self:consume() end
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
                    else table.insert(res, esc) end
                else table.insert(res, nc) end
            end
            self:consume(); table.insert(self.tokens, { type = "string", value = table.concat(res), line = self.line })
        elseif c:match("[%p]") then
            local tri = self.source:sub(self.pos, self.pos + 2)
            local duo = self.source:sub(self.pos, self.pos + 1)
            if tri == "..." then table.insert(self.tokens, { type = "operator", value = "...", line = self.line }); self.pos = self.pos + 3
            elseif duo == "==" or duo == "~=" or duo == "<=" or duo == ">=" or duo == ".." or duo == "<<" or duo == ">>" or duo == "//" then table.insert(self.tokens, { type = "operator", value = duo, line = self.line }); self.pos = self.pos + 2
            else table.insert(self.tokens, { type = "operator", value = self:consume(), line = self.line }) end
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
    elseif t.value == "for" then
        self:consume(); local name = self:expect("name").value
        if self:peek().value == "=" then
            self:consume(); local init = self:parseExpr(); self:expect(","); local limit = self:parseExpr()
            local step = { type = "Number", value = "1" }; if self:peek().value == "," then self:consume(); step = self:parseExpr() end
            self:expect("do"); local body = self:parseBlock(); self:expect("end")
            return { type = "ForRange", var = name, init = init, limit = limit, step = step, body = body }
        else
            local names = { name }; while self:peek().value == "," do self:consume(); table.insert(names, self:expect("name").value) end
            self:expect("in"); local iter = self:parseExpr(); self:expect("do"); local body = self:parseBlock(); self:expect("end")
            return { type = "ForIn", vars = names, iter = iter, body = body }
        end
    elseif t.value == "repeat" then
        self:consume(); local body = self:parseBlock(); self:expect("until"); local cond = self:parseExpr(); return { type = "Repeat", body = body, cond = cond }
    elseif t.value == "function" then
        self:consume(); local name = self:expect("name").value; local node = { type = "Var", name = name }
        while self:peek().value == "." do self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } } end
        local args = self:parseArgs(); local body = self:parseBlock(); self:expect("end")
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
        args = { self:expect("name").value }; while self:peek().value == "," do self:consume(); if self:peek().value == "..." then table.insert(args, self:consume().value); break else table.insert(args, self:expect("name").value) end end
    end; self:expect(")"); return args
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
    elseif tk.value == "..." then self:consume(); node = { type = "Vararg" }
    elseif tk.type == "name" then node = { type = "Var", name = self:consume().value }
    else error("Line " .. (tk and tk.line or "unknown") .. ": unexpected token " .. (tk and tk.value or "nil")) end
    while true do
        local t = self:peek()
        if t.value == "." then self:consume(); node = { type = "Index", table = node, key = { type = "String", value = self:expect("name").value } }
        elseif t.value == "[" then self:consume(); node = { type = "Index", table = node, key = self:parseExpr() }; self:expect("]")
        elseif t.value == ":" then self:consume(); local n = self:expect("name").value; local a = self:parseCallArgs(); node = { type = "MemberCall", table = node, member = n, args = a }
        elseif t.value == "(" or t.type == "string" then node = { type = "Call", func = node, args = self:parseCallArgs() }
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
    else error("Expected call arguments") end
end

local Obfuscator = {}
function Obfuscator.desugar(ast)
    local function walk(n)
        if not n or type(n) ~= "table" then return n end
        if n.type == "Block" then for i, v in ipairs(n.body) do n.body[i] = walk(v) end
        elseif n.type == "If" then n.cond = walk(n.cond); n.body = walk(n.body); for _, v in ipairs(n.elseifs) do v.cond = walk(v.cond); v.body = walk(v.body) end; n.elseBlock = walk(n.elseBlock)
        elseif n.type == "While" then n.cond = walk(n.cond); n.body = walk(n.body)
        elseif n.type == "ForRange" then
            local var = n.var; local init = walk(n.init); local limit = walk(n.limit); local step = walk(n.step); local body = walk(n.body)
            table.insert(body.body, { type = "Assign", vars = { {type="Var", name=var} }, values = { {type="BinaryOp", op="+", left={type="Var", name=var}, right=step} } })
            return { type = "Block", body = { { type = "LocalAssign", vars = { var }, values = { init } }, { type = "While", cond = { type = "BinaryOp", op = "<=", left = { type = "Var", name = var }, right = limit }, body = body } } }
        elseif n.type == "LocalFunction" then return walk({ type = "LocalAssign", vars = { n.name }, values = { { type = "FunctionDef", args = n.args, body = n.body } }, is_recursive = true })
        elseif n.type == "FunctionDef" then n.body = walk(n.body)
        elseif n.type == "Call" or n.type == "MemberCall" then if n.func then n.func = walk(n.func) end; if n.table then n.table = walk(n.table) end; for i, v in ipairs(n.args) do n.args[i] = walk(v) end
        elseif n.type == "Assign" or n.type == "LocalAssign" then if n.values then for i, v in ipairs(n.values) do n.values[i] = walk(v) end end
        elseif n.type == "BinaryOp" then n.left = walk(n.left); n.right = walk(n.right)
        elseif n.type == "UnaryOp" then n.right = walk(n.right)
        elseif n.type == "Index" then n.table = walk(n.table); n.key = walk(n.key)
        elseif n.type == "Return" then for i, v in ipairs(n.values) do n.values[i] = walk(v) end
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
        elseif n.type == "FunctionDef" then local fs = { parent = s, locals = {} }; for i, v in ipairs(n.args) do if v ~= "..." then local nn = gnn(); fs.locals[v] = nn; n.args[i] = nn end end; walk(n.body, fs)
        elseif n.type == "If" then walk(n.cond, s); walk(n.body, s); for _, v in ipairs(n.elseifs) do walk(v.cond, s); walk(v.body, s) end; walk(n.elseBlock, s)
        elseif n.type == "While" then walk(n.cond, s); walk(n.body, s)
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
    local ops_list = { "LOADK", "GETVAR", "SETVAR", "GETTABLE", "SETTABLE", "CALL", "RET", "ADD", "SUB", "MUL", "DIV", "IDIV", "MOD", "POW", "BAND", "BOR", "BXOR", "SHL", "SHR", "EQ", "LT", "LE", "CONCAT", "NOT", "UNM", "BNOT", "LEN", "CLOSURE", "LOAD_VA", "LOAD_VARARG", "PICK_RESULT", "POP", "DUP", "SWAP", "JMP", "JMP_IF_FALSE" }
    local XK = math.random(1, 0xFFFFFF); local SX = math.random(1, 255)
    local function encrypt_k(v)
        local s = (type(v) == "string" and "s" or type(v) == "number" and "n" or type(v) == "boolean" and "b" or "x") .. tostring(v == nil and "" or v == true and "t" or v == false and "f" or v)
        local r = {}; for i=1, #s do table.insert(r, string.char(s:byte(i) ~ SX)) end; return table.concat(r)
    end
    local function pp(block, args)
        local insts = {}; local ks = { n = 0 }; local function addK(v) for i=1, ks.n do if ks[i] == v then return i end end; ks.n = ks.n + 1; ks[ks.n] = v; return ks.n end
        local function emit(op, arg)
            if math.random() < 0.1 then table.insert(insts, { op = "LOADK", arg = addK(math.random()) }); table.insert(insts, { op = "POP", arg = 1 }) end
            table.insert(insts, { op = op, arg = arg }); return #insts
        end
        if args then for i, a in ipairs(args) do if a ~= "..." then emit("LOAD_VA", i); emit("SETVAR", addK(a)) end end end
        local function walk(n, multi, is_stmt)
            if not n then return end
            if n.type == "Block" then for _, v in ipairs(n.body) do walk(v, false, true) end
            elseif n.type == "LocalAssign" or n.type == "Assign" then
                local vs, vals = n.vars or {}, n.values or {}
                for i = 1, #vals do walk(vals[i]) end
                if #vals < #vs then for i = #vals + 1, #vs do emit("LOADK", addK(nil)) end end
                for i = #vs, 1, -1 do local v = vs[i]
                    if type(v) == "table" and v.type == "Index" then walk(v.table); walk(v.key); emit("SETTABLE", 0)
                    elseif type(v) == "table" and v.type == "Var" then emit("SETVAR", addK(v.name)) else emit("SETVAR", addK(v)) end
                end
            elseif n.type == "Var" then emit("GETVAR", addK(n.name))
            elseif n.type == "Vararg" then emit("LOAD_VARARG", 0)
            elseif n.type == "Number" or n.type == "String" or n.type == "Boolean" or n.type == "Nil" then emit("LOADK", addK(n.value))
            elseif n.type == "Call" then walk(n.func); for _, v in ipairs(n.args) do walk(v) end; emit("CALL", #n.args); if is_stmt then emit("POP", 1) end
            elseif n.type == "MemberCall" then walk(n.table); emit("DUP", 0); emit("LOADK", addK(n.member)); emit("GETTABLE", 0); emit("SWAP", 0); for _, v in ipairs(n.args) do walk(v) end; emit("CALL", #n.args + 1); if is_stmt then emit("POP", 1) end
            elseif n.type == "Index" then walk(n.table); walk(n.key); emit("GETTABLE", 0)
            elseif n.type == "BinaryOp" then walk(n.left); walk(n.right); local m = { ["+"]="ADD", ["-"]="SUB", ["*"]="MUL", ["/"]="DIV", ["//"]="IDIV", ["%"]="MOD", ["&"]="BAND", ["|"]="BOR", ["~"]="BXOR", ["<<"]="SHL", [">>"]="SHR", ["=="]="EQ", ["<"]="LT", ["<="]="LE", [".."]="CONCAT", ["^"]="POW" }; emit(m[n.op], 0)
            elseif n.type == "UnaryOp" then walk(n.right); local m = { ["not"]="NOT", ["-"]="UNM", ["~"]="BNOT", ["#"]="LEN" }; emit(m[n.op], 0)
            elseif n.type == "FunctionDef" then emit("CLOSURE", addK(pp(n.body, n.args)))
            elseif n.type == "Return" then for _, v in ipairs(n.values) do walk(v) end; emit("RET", #n.values)
            elseif n.type == "If" then walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); local j2 = emit("JMP", 0); insts[j1].arg = #insts + 1; if n.elseBlock then walk(n.elseBlock) end; insts[j2].arg = #insts + 1
            elseif n.type == "While" then local s = #insts + 1; walk(n.cond); local j1 = emit("JMP_IF_FALSE", 0); walk(n.body); emit("JMP", s); insts[j1].arg = #insts + 1
            end
        end; walk(block, false, true); if #insts == 0 or insts[#insts].op ~= "RET" then emit("RET", 0) end
        return { b = insts, k = ks }
    end
    local main = pp(ast); local function sp(p)
        local op_to_id = {}; for i, op in ipairs(ops_list) do op_to_id[op] = i end
        local b = {}; local rk = math.random(0, 255)
        for i, v in ipairs(p.b) do
            local pck = (op_to_id[v.op] % 256) + (v.arg or 0) * 256
            table.insert(b, pck ~ ((rk + i) % 256))
        end
        local em = {}; for i, op in ipairs(ops_list) do em[i] = encrypt_k(op) end
        local k = { n = p.k.n }; for i = 1, p.k.n do local v = p.k[i]; if type(v) == "table" and v.b then k[i] = sp(v) else k[i] = encrypt_k(v) end end
        return { b = b, k = k, m = em, sk = rk }
    end
    local egm = {}; for k, v in pairs(gm) do egm[k] = encrypt_k(v) end; return sp(main), egm, SX
end

local Wrapper = {}
function Wrapper.generate(main, gm, sx)
    local vmap = {}
    local function vn(n) if not vmap[n] then vmap[n] = string.format("_0x%X", math.random(0x100000, 0x999999)) end; return vmap[n] end
    local function sk(ks)
        local s = "{"; local n = ks.n or 0; if n == 0 then for k, v in pairs(ks) do if type(k) == "number" and k > n then n = k end end end
        for i=1, n do local v = ks[i]
            if type(v) == "string" then s = s .. "\"" .. v:gsub(".", function(c) return "\\" .. string.format("%03d", c:byte()) end) .. "\","
            elseif type(v) == "table" and v.b then s = s .. Wrapper._sp(v, vn) .. ","
            else s = s .. "nil," end
        end; return s .. "}"
    end
    local function sm(t)
        local s = "{"; for k, v in pairs(t) do
            s = s .. "[\"" .. k .. "\"]=\"" .. v:gsub(".", function(c) return "\\" .. string.format("%03d", c:byte()) end) .. "\","
        end; return s .. "}"
    end
    function Wrapper._sp(p, vn_func) return "{b={" .. table.concat(p.b, ",") .. "},k=" .. sk(p.k) .. ",m=" .. sk(p.m) .. ",sk=" .. (p.sk or 0) .. "}" end
    local body = "return (function(...)\n"
    body = body .. "local "..vn("_L_ENV").." = _ENV or _G; local "..vn("_L_G").." = _G; local "..vn("_L_VAULT").." = {};\n"
    body = body .. "local "..vn("_GTY")..", "..vn("_GIP")..", "..vn("_GERR")..", "..vn("_GTON")..", "..vn("_GTOS")..", "..vn("_GPCL")..", "..vn("_GUPK")..", "..vn("_GPAI").." = type, ipairs, error, tonumber, tostring, pcall, (table and table.unpack or unpack), pairs\n"
    body = body .. "local "..vn("_L_NAMES").." = {'string','table','math','io','os','debug','coroutine','package','utf8','bit32','print','type','pairs','ipairs','next','error','tonumber','tostring','pcall','select','assert','unpack','load','loadfile','dofile'}\n"
    body = body .. "local "..vn("_L_HIDE").." = {['string']=1,['table']=1,['math']=1,['io']=1,['os']=1,['debug']=1,['coroutine']=1,['package']=1,['utf8']=1,['bit32']=1,['load']=1,['loadfile']=1,['dofile']=1}\n"
    body = body .. "for _, n in "..vn("_GIP").."("..vn("_L_NAMES")..") do "..vn("_L_VAULT").."[n] = "..vn("_L_ENV").."[n] or ("..vn("_L_G").." and "..vn("_L_G").."[n]); if "..vn("_L_HIDE").."[n] then "..vn("_L_ENV").."[n] = nil; if "..vn("_L_G").." then "..vn("_L_G").."[n] = nil end end end\n"
    body = body .. "local "..vn("_M_FL").." = ("..vn("_L_VAULT").."['math'] or math).floor; local "..vn("_S_CH")..", "..vn("_T_CO")..", "..vn("_S_SU").." = ("..vn("_L_VAULT").."['string'] or string).char, ("..vn("_L_VAULT").."['table'] or table).concat, ("..vn("_L_VAULT").."['string'] or string).sub\n"
    body = body .. "local "..vn("_D_MAP").." = {}; local "..vn("_GM_RAW").." = " .. sm(gm) .. "\n"
    body = body .. "local function "..vn("_d").."(v) if "..vn("_GTY").."(v) ~= 'string' then return v end; local r = {}; for i=1, #v do r[#r+1] = "..vn("_S_CH").."(v:byte(i) ~ "..sx..") end; local rs = "..vn("_T_CO").."(r); local t, val = "..vn("_S_SU").."(rs,1,1), "..vn("_S_SU").."(rs,2); if t == 's' then return val elseif t == 'n' then return "..vn("_GTON").."(val) elseif t == 'b' then return val == 't' else return nil end end\n"
    body = body .. "local function "..vn("_GFG").."(en_val) local dn = "..vn("_d").."(en_val); if dn == '_G' then return "..vn("_L_G").." end; if dn == '_ENV' then return "..vn("_L_ENV").." end; local on_en = "..vn("_GM_RAW").."[dn]; local on = on_en and "..vn("_d").."(on_en) or dn; local res = "..vn("_L_VAULT").."[on] or "..vn("_L_ENV").."[on]; if not res then "..vn("_GERR").."('GFG fail: ' .. "..vn("_GTOS").."(on)) end; return res end\n"
    body = body .. "local function "..vn("_EXEC").."("..vn("_PR")..", ...)\n"
    body = body .. "  local "..vn("_S")..", "..vn("_SS")..", "..vn("_P")..", "..vn("_K")..", "..vn("_V")..", "..vn("_RK").." = {}, 0, 1, "..vn("_PR")..".k, {}, "..vn("_PR")..".sk\n"
    body = body .. "  while "..vn("_P").." <= #"..vn("_PR")..".b do\n"
    body = body .. "    local _EPCK = "..vn("_PR")..".b["..vn("_P").."]; local _PCK = _EPCK ~ (("..vn("_RK").." + "..vn("_P")..") % 256); "..vn("_P").." = "..vn("_P").." + 1; local _OPI, "..vn("_ARG").." = _PCK % 256, "..vn("_M_FL").."(_PCK / 256); local _OPN = "..vn("_d").."("..vn("_PR")..".m[_OPI])\n"
    body = body .. "    if _OPN == 'LOADK' then "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = "..vn("_d").."("..vn("_K").."["..vn("_ARG").."])\n"
    body = body .. "    elseif _OPN == 'GETVAR' then local en = "..vn("_K").."["..vn("_ARG").."]; local n = "..vn("_d").."(en); local val = "..vn("_V").."[n]; if val == nil then val = "..vn("_GFG").."(en) end; "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = val\n"
    body = body .. "    elseif _OPN == 'SETVAR' then local n = "..vn("_d").."("..vn("_K").."["..vn("_ARG").."]); "..vn("_V").."[n] = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1\n"
    body = body .. "    elseif _OPN == 'CALL' then local as = {}; for i=1, "..vn("_ARG").." do as["..vn("_ARG").."-i+1] = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1 end; local f = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; if not f then "..vn("_GERR").."('CALL: nil function') end; local re = {"..vn("_GPCL").."(f, "..vn("_GUPK").."(as, 1, "..vn("_ARG").."))}; if not re[1] then "..vn("_GERR").."(re[2]) end; "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = re[2]\n"
    body = body .. "    elseif _OPN == 'RET' then local re = {}; for i=1, "..vn("_ARG").." do re["..vn("_ARG").."-i+1] = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1 end; return "..vn("_GUPK").."(re)\n"
    body = body .. "    elseif _OPN == 'GETTABLE' then local k = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local t = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = t[k]\n"
    body = body .. "    elseif _OPN == 'ADD' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; if "..vn("_GTY").."(l) == 'number' and "..vn("_GTY").."(r) == 'number' then "..vn("_S").."["..vn("_SS").."] = (l ~ r) + 2 * (l & r) else "..vn("_S").."["..vn("_SS").."] = l + r end\n"
    body = body .. "    elseif _OPN == 'SUB' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; if "..vn("_GTY").."(l) == 'number' and "..vn("_GTY").."(r) == 'number' then "..vn("_S").."["..vn("_SS").."] = (l + (~r)) + 1 else "..vn("_S").."["..vn("_SS").."] = l - r end\n"
    body = body .. "    elseif _OPN == 'MUL' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l * r\n"
    body = body .. "    elseif _OPN == 'DIV' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l / r\n"
    body = body .. "    elseif _OPN == 'POW' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l ^ r\n"
    body = body .. "    elseif _OPN == 'IDIV' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l // r\n"
    body = body .. "    elseif _OPN == 'MOD' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l % r\n"
    body = body .. "    elseif _OPN == 'BAND' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l & r\n"
    body = body .. "    elseif _OPN == 'BOR' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l | r\n"
    body = body .. "    elseif _OPN == 'BXOR' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l ~ r\n"
    body = body .. "    elseif _OPN == 'SHL' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l << r\n"
    body = body .. "    elseif _OPN == 'SHR' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l >> r\n"
    body = body .. "    elseif _OPN == 'EQ' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l == r\n"
    body = body .. "    elseif _OPN == 'LT' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l < r\n"
    body = body .. "    elseif _OPN == 'LE' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l <= r\n"
    body = body .. "    elseif _OPN == 'CONCAT' then local r = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; local l = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = l .. r\n"
    body = body .. "    elseif _OPN == 'UNM' then "..vn("_S").."["..vn("_SS").."] = -"..vn("_S").."["..vn("_SS").."]\n"
    body = body .. "    elseif _OPN == 'BNOT' then "..vn("_S").."["..vn("_SS").."] = ~"..vn("_S").."["..vn("_SS").."]\n"
    body = body .. "    elseif _OPN == 'LEN' then "..vn("_S").."["..vn("_SS").."] = #"..vn("_S").."["..vn("_SS").."]\n"
    body = body .. "    elseif _OPN == 'DUP' then "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = "..vn("_S").."["..vn("_SS").." - "..vn("_ARG").." - 1]\n"
    body = body .. "    elseif _OPN == 'SWAP' then local a = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = "..vn("_S").."["..vn("_SS").."-1]; "..vn("_S").."["..vn("_SS").."-1] = a\n"
    body = body .. "    elseif _OPN == 'JMP' then "..vn("_P").." = "..vn("_ARG").."\n"
    body = body .. "    elseif _OPN == 'JMP_IF_FALSE' then local v = "..vn("_S").."["..vn("_SS").."]; "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1; if not v then "..vn("_P").." = "..vn("_ARG").." end\n"
    body = body .. "    elseif _OPN == 'CLOSURE' then local pr = "..vn("_K").."["..vn("_ARG").."]; "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = function(...) return "..vn("_EXEC").."(pr, ...) end\n"
    body = body .. "    elseif _OPN == 'LOAD_VARARG' then local va = {...}; for i=1, #va do "..vn("_SS").." = "..vn("_SS").." + 1; "..vn("_S").."["..vn("_SS").."] = va[i] end\n"
    body = body .. "    elseif _OPN == 'POP' then for i=1, "..vn("_ARG").." do "..vn("_S").."["..vn("_SS").."] = nil; "..vn("_SS").." = "..vn("_SS").." - 1 end\n"
    body = body .. "    end\n"
    body = body .. "  end\n"
    body = body .. "end\n"
    body = body .. "return "..vn("_EXEC").."(" .. Wrapper._sp(main, vn) .. ", ...)\n"
    body = body .. "end)(...)"
    return body
end

function Obfuscator.obfuscate(source)
    local lex = Lexer.new(source); local tokens = lex:tokenize(); local ast = Parser.new(tokens):parse()
    ast = Obfuscator.desugar(ast); local gm = Obfuscator.obfuscateIdentifiers(ast); local main, egm, sx = Virtualizer.virtualize(ast, gm)
    return Wrapper.generate(main, egm, sx)
end

if _G.arg and _G.arg[1] and _G.arg[2] then
    local f = io.open(_G.arg[1], "rb"); local src = f:read("*all"); f:close()
    local ok, obf = pcall(Obfuscator.obfuscate, src)
    if ok then local out = io.open(_G.arg[2], "wb"); out:write(obf); out:close() else print(obf) end
end
return Obfuscator
