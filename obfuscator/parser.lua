local Parser = {}
Parser.__index = Parser

function Parser.new(lexer)
  local self = setmetatable({}, Parser)
  self.lexer = lexer
  self:next_token()
  return self
end

function Parser:next_token()
  self.token, self.value = self.lexer:scan()
  return self.token, self.value
end

function Parser:expect(expected)
  if self.token == expected then
    local v = self.value
    self:next_token()
    return v
  else
    error("Line " .. self.lexer.line .. ": expected " .. expected .. " but got " .. tostring(self.token))
  end
end

function Parser:test(expected)
  if self.token == expected then
    self:next_token()
    return true
  end
  return false
end

function Parser:parse()
  local chunk = { type = "Chunk", body = {} }
  while self.token ~= "TK_EOS" do
    table.insert(chunk.body, self:parse_statement())
    self:test(";")
  end
  return chunk
end

function Parser:parse_statement()
  if self.token == "TK_IF" then
    return self:parse_if()
  elseif self.token == "TK_WHILE" then
    return self:parse_while()
  elseif self.token == "TK_DO" then
    return self:parse_do()
  elseif self.token == "TK_FOR" then
    return self:parse_for()
  elseif self.token == "TK_REPEAT" then
    return self:parse_repeat()
  elseif self.token == "TK_FUNCTION" then
    return self:parse_function_def()
  elseif self.token == "TK_LOCAL" or self.token == "TK_DOLLAR" then
    return self:parse_local()
  elseif self.token == "TK_RETURN" then
    return self:parse_return()
  elseif self.token == "TK_BREAK" then
    self:next_token()
    return { type = "Break" }
  elseif self.token == "TK_CONTINUE" then
    self:next_token()
    return { type = "Continue" }
  elseif self.token == "TK_DEFER" then
    self:next_token()
    return { type = "Defer", body = self:parse_statement() }
  elseif self.token == "TK_SWITCH" then
    return self:parse_switch()
  elseif self.token == "TK_WHEN" then
    return self:parse_when()
  elseif self.token == "TK_DBCOLON" then
    return self:parse_label()
  elseif self.token == "TK_GOTO" then
    return self:parse_goto()
  else
    return self:parse_assignment_or_call()
  end
end

-- Extension: Optional then/do/in
function Parser:optional_keyword(kw)
  self:test(kw) -- Just skip if present, otherwise ignore
end

function Parser:parse_if()
  self:next_token()
  local cond = self:parse_expression()
  self:optional_keyword("TK_THEN")
  local body = {}
  local elseifs = {}
  local else_body

  while self.token ~= "TK_END" and self.token ~= "TK_ELSE" and self.token ~= "TK_ELSEIF" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end

  while self.token == "TK_ELSEIF" do
    self:next_token()
    local e_cond = self:parse_expression()
    self:optional_keyword("TK_THEN")
    local e_body = {}
    while self.token ~= "TK_END" and self.token ~= "TK_ELSE" and self.token ~= "TK_ELSEIF" do
      table.insert(e_body, self:parse_statement())
      self:test(";")
    end
    table.insert(elseifs, { condition = e_cond, body = e_body })
  end

  if self.token == "TK_ELSE" then
    self:next_token()
    else_body = {}
    while self.token ~= "TK_END" do
      table.insert(else_body, self:parse_statement())
      self:test(";")
    end
  end

  self:expect("TK_END")
  return { type = "If", condition = cond, body = body, elseifs = elseifs, else_body = else_body }
end

function Parser:parse_local()
  local is_dollar = (self.token == "TK_DOLLAR")
  self:next_token()

  local names = {}
  local tbc = {}
  repeat
    local is_tbc = self:test("TK_MUL")
    local name = self:expect("TK_NAME")
    table.insert(names, name)
    table.insert(tbc, is_tbc)
  until not self:test(",")

  local values = {}
  if self:test("=") then
    repeat
      table.insert(values, self:parse_expression())
    until not self:test(",")
  end

  return { type = "LocalAssign", names = names, values = values, tbc = tbc }
end

function Parser:parse_while()
  self:next_token()
  local cond = self:parse_expression()
  self:optional_keyword("TK_DO")
  local body = {}
  while self.token ~= "TK_END" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end
  self:expect("TK_END")
  return { type = "While", condition = cond, body = body }
end

function Parser:parse_do()
  self:next_token()
  local body = {}
  while self.token ~= "TK_END" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end
  self:expect("TK_END")
  return { type = "Do", body = body }
end

function Parser:parse_repeat()
  self:next_token()
  local body = {}
  while self.token ~= "TK_UNTIL" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end
  self:expect("TK_UNTIL")
  local cond = self:parse_expression()
  return { type = "Repeat", body = body, condition = cond }
end

function Parser:parse_for()
  self:next_token()
  local name = self:expect("TK_NAME")
  if self:test("=") then
    local init = self:parse_expression()
    self:expect(",")
    local limit = self:parse_expression()
    local step
    if self:test(",") then
      step = self:parse_expression()
    end
    self:optional_keyword("TK_DO")
    local body = {}
    while self.token ~= "TK_END" do
      table.insert(body, self:parse_statement())
      self:test(";")
    end
    self:expect("TK_END")
    return { type = "ForRange", name = name, init = init, limit = limit, step = step, body = body }
  else
    local names = { name }
    while self:test(",") do
      table.insert(names, self:expect("TK_NAME"))
    end
    self:optional_keyword("TK_IN")
    local iterators = {}
    repeat
      table.insert(iterators, self:parse_expression())
    until not self:test(",")
    self:optional_keyword("TK_DO")
    local body = {}
    while self.token ~= "TK_END" do
      table.insert(body, self:parse_statement())
      self:test(";")
    end
    self:expect("TK_END")
    return { type = "ForIn", names = names, iterators = iterators, body = body }
  end
end

function Parser:parse_switch()
  self:next_token()
  local val = self:parse_expression()
  self:optional_keyword("TK_DO")
  local cases = {}
  local default_body
  while self.token ~= "TK_END" do
    if self.token == "TK_CASE" then
      self:next_token()
      local case_val = self:parse_expression()
      self:test(":")
      local body = {}
      while self.token ~= "TK_CASE" and self.token ~= "TK_DEFAULT" and self.token ~= "TK_END" do
        table.insert(body, self:parse_statement())
        self:test(";")
      end
      table.insert(cases, { value = case_val, body = body })
    elseif self.token == "TK_DEFAULT" then
      self:next_token()
      self:test(":")
      default_body = {}
      while self.token ~= "TK_CASE" and self.token ~= "TK_END" do
        table.insert(default_body, self:parse_statement())
        self:test(";")
      end
    else
      error("Unexpected token in switch: " .. tostring(self.token))
    end
  end
  self:expect("TK_END")
  return { type = "Switch", value = val, cases = cases, default_body = default_body }
end

function Parser:parse_when()
  -- Androlua+ 'when' is often like a switch or a series of conditions
  self:next_token()
  local val
  if self.token ~= "{" then
     val = self:parse_expression()
  end
  self:expect("{")
  local cases = {}
  local else_body
  while self.token ~= "}" do
    if self.token == "TK_ELSE" then
       self:next_token()
       self:expect("->")
       else_body = self:parse_statement()
    else
       local cond = self:parse_expression()
       self:expect("->")
       local body = self:parse_statement()
       table.insert(cases, { condition = cond, body = body })
    end
    self:test(",")
  end
  self:expect("}")
  return { type = "When", value = val, cases = cases, else_body = else_body }
end

function Parser:parse_label()
  self:next_token()
  local name = self:expect("TK_NAME")
  self:expect("TK_DBCOLON")
  return { type = "Label", name = name }
end

function Parser:parse_goto()
  self:next_token()
  local name = self:expect("TK_NAME")
  return { type = "Goto", name = name }
end

function Parser:parse_assignment_or_call()
  local expr = self:parse_suffixed_expr()
  if self.token == "," or self.token == "=" then
    local targets = { expr }
    while self:test(",") do
      table.insert(targets, self:parse_suffixed_expr())
    end
    self:expect("=")
    local values = {}
    repeat
      table.insert(values, self:parse_expression())
    until not self:test(",")
    return { type = "Assign", targets = targets, values = values }
  else
    -- Must be a call
    if expr.type ~= "Call" and expr.type ~= "MethodCall" then
      error("Syntax error: statement must be an assignment or call")
    end
    return expr
  end
end

function Parser:parse_expression()
  return self:parse_sub_expr(0)
end

local PREC = {
  TK_OR = 1,
  TK_AND = 2,
  TK_LT = 3, TK_GT = 3, TK_LE = 3, TK_GE = 3, TK_NE = 3, TK_EQ = 3, TK_BNE = 3,
  ["<"] = 3, [">"] = 3, ["=="] = 3, ["~="] = 3,
  TK_BITOR = 4, ["|"] = 4,
  TK_BITXOR = 5,
  TK_BITAND = 6, ["&"] = 6,
  TK_SHL = 7, TK_SHR = 7,
  TK_CONCAT = 8, [".."] = 8,
  TK_ADD = 9, TK_SUB = 9, ["+"] = 9, ["-"] = 9,
  TK_MUL = 10, TK_DIV = 10, TK_IDIV = 10, TK_MOD = 10, ["*"] = 10, ["/"] = 10, ["%"] = 10,
  -- Unary: not, #, -, ~ (11)
  TK_POW = 12, ["^"] = 12,
}

function Parser:parse_sub_expr(min_prec)
  local left = self:parse_primary_expr()

  while true do
    local op = self.token
    local prec = PREC[op] or (op == "|" and 1 or -1) -- lambda handle
    if prec < min_prec then break end

    self:next_token()
    local right = self:parse_sub_expr(prec + (op == "TK_POW" and 0 or 1))
    left = { type = "BinaryOp", op = op, left = left, right = right }
  end

  return left
end

function Parser:parse_primary_expr()
  local t = self.token
  if t == "TK_NOT" or t == "TK_BNOT" or t == "-" or t == "#" or t == "~" then
    self:next_token()
    return { type = "UnaryOp", op = t, right = self:parse_sub_expr(11) }
  elseif t == "TK_NUMBER" then
    local val = self.value
    self:next_token()
    return { type = "Number", value = val }
  elseif t == "TK_STRING" then
    local val = self.value
    self:next_token()
    return { type = "String", value = val }
  elseif t == "TK_TRUE" or t == "TK_FALSE" or t == "TK_NIL" then
    self:next_token()
    return { type = "Literal", value = t }
  elseif t == "TK_DOTS" then
    self:next_token()
    return { type = "Vararg" }
  elseif t == "{" then
    return self:parse_table()
  elseif t == "[" then
    return self:parse_array()
  elseif t == "TK_FUNCTION" then
    return self:parse_function_expr()
  elseif t == "TK_LAMBDA" then
    return self:parse_lambda()
  elseif t == "(" then
    self:next_token()
    local e = self:parse_expression()
    self:expect(")")
    return e
  elseif t == "|" then
    return self:parse_lambda_short()
  else
    return self:parse_suffixed_expr()
  end
end

function Parser:parse_table()
  self:expect("{")
  local fields = {}
  while self.token ~= "}" do
    if self.token == "[" then
      self:next_token()
      local key = self:parse_expression()
      self:expect("]")
      self:expect("=")
      table.insert(fields, { key = key, value = self:parse_expression() })
    elseif self.token == "TK_NAME" or PREC[self.token] or Lexer.RESERVED[self.value] then
      -- Androlua+ supports keywords as keys
      local key = self.value
      self:next_token()
      if self:test("=") then
         table.insert(fields, { key = { type = "String", value = key }, value = self:parse_expression() })
      else
         -- Was just a list item (if it was a name)
         error("Expected '=' after table key")
      end
    else
      table.insert(fields, { value = self:parse_expression() })
    end
    if not (self:test(",") or self:test(";")) then break end
  end
  self:expect("}")
  return { type = "Table", fields = fields }
end

function Parser:parse_array()
  self:expect("[")
  local elements = {}
  while self.token ~= "]" do
    table.insert(elements, self:parse_expression())
    if not self:test(",") then break end
  end
  self:expect("]")
  return { type = "Array", elements = elements }
end

function Parser:parse_lambda()
  self:next_token()
  local args = {}
  if self.token ~= ":" then
    repeat
      table.insert(args, self:expect("TK_NAME"))
    until not self:test(",")
  end
  self:expect(":")
  return { type = "Function", args = args, body = { { type = "Return", values = { self:parse_expression() } } } }
end

function Parser:parse_lambda_short()
  self:expect("|")
  local args = {}
  if self.token ~= "|" then
    repeat
      table.insert(args, self:expect("TK_NAME"))
    until not self:test(",")
  end
  self:expect("|")
  return { type = "Function", args = args, body = { { type = "Return", values = { self:parse_expression() } } } }
end

function Parser:parse_suffixed_expr()
  local base = { type = "Variable", name = self:expect("TK_NAME") }
  while true do
    if self:test(".") then
      local name = self:expect("TK_NAME")
      base = { type = "Index", base = base, key = { type = "String", value = name } }
    elseif self:test("[") then
      local key = self:parse_expression()
      self:expect("]")
      base = { type = "Index", base = base, key = key }
    elseif self:test(":") then
      local name = self:expect("TK_NAME")
      local args = self:parse_args()
      base = { type = "MethodCall", base = base, method = name, args = args }
    elseif self.token == "(" or self.token == "{" or self.token == "TK_STRING" then
      local args = self:parse_args()
      base = { type = "Call", base = base, args = args }
    else
      break
    end
  end
  return base
end

function Parser:parse_args()
  if self:test("(") then
    local args = {}
    if self.token ~= ")" then
      repeat
        table.insert(args, self:parse_expression())
      until not self:test(",")
    end
    self:expect(")")
    return args
  elseif self.token == "{" then
    return { self:parse_table() }
  elseif self.token == "TK_STRING" then
    local s = self.value
    self:next_token()
    return { { type = "String", value = s } }
  else
    -- Androlua+ extension: optional () for zero args
    return {}
  end
end

function Parser:parse_function_def()
  self:next_token()
  -- simplified: handle function name and body
  local name = self:expect("TK_NAME") -- simplified
  local args = {}
  if self:test("(") then
    if self.token ~= ")" then
      repeat
        table.insert(args, self:expect("TK_NAME"))
      until not self:test(",")
    end
    self:expect(")")
  end
  local body = {}
  while self.token ~= "TK_END" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end
  self:expect("TK_END")
  return { type = "FunctionDef", name = name, args = args, body = body }
end

function Parser:parse_function_expr()
  self:next_token()
  local args = {}
  if self:test("(") then
    if self.token ~= ")" then
      repeat
        if self.token == "TK_DOTS" then
          table.insert(args, "...")
          self:next_token()
          break
        end
        table.insert(args, self:expect("TK_NAME"))
      until not self:test(",")
    end
    self:expect(")")
  end
  local body = {}
  while self.token ~= "TK_END" do
    table.insert(body, self:parse_statement())
    self:test(";")
  end
  self:expect("TK_END")
  return { type = "Function", args = args, body = body }
end

function Parser:parse_return()
  self:next_token()
  local values = {}
  if self.token ~= "TK_END" and self.token ~= "TK_EOS" and self.token ~= "TK_ELSE" and self.token ~= "TK_ELSEIF" and self.token ~= "TK_UNTIL" and self.token ~= ";" then
    repeat
      table.insert(values, self:parse_expression())
    until not self:test(",")
  end
  return { type = "Return", values = values }
end

-- TODO: implement others (while, for, repeat, switch, etc.)

return Parser
