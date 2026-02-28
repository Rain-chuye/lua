local Lexer = {}
Lexer.__index = Lexer

local TOKENS = {
  TK_AND = "and", TK_BREAK = "break", TK_DO = "do", TK_ELSE = "else", TK_ELSEIF = "elseif",
  TK_END = "end", TK_FALSE = "false", TK_FOR = "for", TK_FUNCTION = "function",
  TK_GOTO = "goto", TK_IF = "if", TK_IN = "in", TK_LOCAL = "local", TK_NIL = "nil",
  TK_NOT = "not", TK_OR = "or", TK_REPEAT = "repeat", TK_RETURN = "return",
  TK_THEN = "then", TK_TRUE = "true", TK_UNTIL = "until", TK_WHILE = "while",
  -- Androlua+ keywords
  TK_CONTINUE = "continue", TK_DEFAULT = "default", TK_DEFER = "defer",
  TK_WHEN = "when", TK_CASE = "case", TK_SWITCH = "switch", TK_LAMBDA = "lambda",
  -- Operators
  TK_CONCAT = "..", TK_DOTS = "...", TK_EQ = "==", TK_GE = ">=", TK_LE = "<=",
  TK_NE = "~=", TK_SHL = "<<", TK_SHR = ">>", TK_DBCOLON = "::", TK_IDIV = "//",
  -- Androlua+ operators
  TK_BNE = "!=", TK_BNOT = "!",
}

local RESERVED = {}
for k, v in pairs(TOKENS) do
  if k:match("^TK_[A-Z]+$") and not k:match("TK_ARRAY") and not k:match("TK_BNOT") and not k:match("TK_BNE") then
    RESERVED[v] = k
  end
end

function Lexer.new(source)
  local self = setmetatable({}, Lexer)
  self.source = source
  self.pos = 1
  self.line = 1
  self.ch = source:sub(1, 1)
  return self
end

function Lexer:next_char()
  self.pos = self.pos + 1
  self.ch = self.source:sub(self.pos, self.pos)
  if self.ch == "\n" then self.line = self.line + 1 end
  return self.ch
end

function Lexer:peek_char()
  return self.source:sub(self.pos + 1, self.pos + 1)
end

function Lexer:skip_whitespace()
  while self.ch ~= "" and (self.ch:match("%s") or self.ch == "-") do
    if self.ch == "-" then
      if self:peek_char() == "-" then
        self:next_char()
        self:next_char()
        if self.ch == "[" and self:peek_char() == "[" then
           -- Long comment
           self:next_char()
           self:next_char()
           while self.ch ~= "" do
             if self.ch == "]" and self:peek_char() == "]" then
               self:next_char()
               self:next_char()
               break
             end
             self:next_char()
           end
        else
          while self.ch ~= "" and self.ch ~= "\n" do
            self:next_char()
          end
        end
      else
        break
      end
    else
      self:next_char()
    end
  end
end

function Lexer:read_identifier()
  local start = self.pos
  while self.ch:match("[%w_]") or self.ch:byte() > 127 do
    self:next_char()
  end
  local id = self.source:sub(start, self.pos - 1)
  return RESERVED[id] or "TK_NAME", id
end

function Lexer:read_string(quote)
  local start = self.pos
  self:next_char()
  local res = ""
  while self.ch ~= "" and self.ch ~= quote do
    if self.ch == "\\" then
      self:next_char()
      -- simplified escape
      res = res .. "\\" .. self.ch
    else
      res = res .. self.ch
    end
    self:next_char()
  end
  self:next_char()
  return "TK_STRING", res
end

function Lexer:read_number()
  local start = self.pos
  while self.ch:match("[%d%.xXabcdefABCDEF]") do
    self:next_char()
  end
  return "TK_NUMBER", tonumber(self.source:sub(start, self.pos - 1)) or self.source:sub(start, self.pos - 1)
end

function Lexer:scan()
  self:skip_whitespace()
  if self.ch == "" then return "TK_EOS" end

  if self.ch:match("[%a_]") or self.ch:byte() > 127 then
    return self:read_identifier()
  end

  if self.ch:match("[%d]") then
    return self:read_number()
  end

  local ch = self.ch
  if ch == "\"" or ch == "'" then
    return self:read_string(ch)
  end

  if ch == "." then
    if self:peek_char() == "." then
      self:next_char()
      if self:peek_char() == "." then
        self:next_char()
        self:next_char()
        return "TK_DOTS", "..."
      end
      self:next_char()
      return "TK_CONCAT", ".."
    end
  end

  if ch == "=" then
    if self:peek_char() == "=" then
      self:next_char()
      self:next_char()
      return "TK_EQ", "=="
    end
  end

  if ch == "~" then
    if self:peek_char() == "=" then
      self:next_char()
      self:next_char()
      return "TK_NE", "~="
    end
  end

  if ch == "!" then
    if self:peek_char() == "=" then
      self:next_char()
      self:next_char()
      return "TK_BNE", "!="
    end
    self:next_char()
    return "TK_BNOT", "!"
  end

  if ch == "<" then
    if self:peek_char() == "=" then
      self:next_char()
      self:next_char()
      return "TK_LE", "<="
    elseif self:peek_char() == "<" then
      self:next_char()
      self:next_char()
      return "TK_SHL", "<<"
    end
  end

  if ch == ">" then
    if self:peek_char() == "=" then
      self:next_char()
      self:next_char()
      return "TK_GE", ">="
    elseif self:peek_char() == ">" then
      self:next_char()
      self:next_char()
      return "TK_SHR", ">>"
    end
  end

  if ch == "/" then
    if self:peek_char() == "/" then
      self:next_char()
      self:next_char()
      return "TK_IDIV", "//"
    end
  end

  if ch == ":" then
    if self:peek_char() == ":" then
      self:next_char()
      self:next_char()
      return "TK_DBCOLON", "::"
    end
  end

  if ch == "$" then
    self:next_char()
    return "TK_DOLLAR", "$"
  end

  if ch == "*" then
    -- Could be multiplication or TBC marker
    self:next_char()
    return "TK_MUL", "*"
  end

  self:next_char()
  return ch, ch
end

return Lexer
