local BytecodeParser = {}
BytecodeParser.__index = BytecodeParser

function BytecodeParser.new(data)
  local self = setmetatable({}, BytecodeParser)
  self.data = data
  self.pos = 1
  return self
end

function BytecodeParser:read_byte()
  local b = self.data:byte(self.pos)
  self.pos = self.pos + 1
  return b
end

function BytecodeParser:read_int()
  local b1, b2, b3, b4 = self.data:byte(self.pos, self.pos + 3)
  self.pos = self.pos + 4
  return b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
end

function BytecodeParser:read_string()
  local size = self:read_byte()
  if size == 0 then return nil end
  if size == 0xFF then
     -- size_t follows (simplified)
     size = self:read_int() -- assume 4 bytes for simplicity
  end
  local s = self.data:sub(self.pos, self.pos + size - 2)
  self.pos = self.pos + size - 1
  return s
end

function BytecodeParser:read_proto()
  local proto = {}
  self:read_string() -- source
  self:read_int() -- linedefined
  self:read_int() -- lastlinedefined
  proto.numparams = self:read_byte()
  proto.is_vararg = self:read_byte()
  proto.maxstacksize = self:read_byte()

  -- Instructions
  local n_inst = self:read_int()
  proto.instructions = {}
  for i = 1, n_inst do
    local inst = self:read_int()
    -- Map Lua 5.3 opcodes to my internal OP names (simplified)
    table.insert(proto.instructions, self:decode_lua53(inst))
  end

  -- Constants
  local n_k = self:read_int()
  proto.constants = {}
  for i = 1, n_k do
    local t = self:read_byte()
    if t == 0 then -- NIL
      table.insert(proto.constants, nil)
    elseif t == 1 then -- BOOLEAN
      table.insert(proto.constants, self:read_byte() ~= 0)
    elseif t == 3 then -- NUMBER
      -- simplified: assume 8 bytes double
      self.pos = self.pos + 8
      table.insert(proto.constants, 0) -- TODO
    elseif t == 0x13 then -- INTEGER
      -- simplified: assume 8 bytes int
      self.pos = self.pos + 8
      table.insert(proto.constants, 0) -- TODO
    elseif t == 4 or t == 0x14 then -- STRING
      table.insert(proto.constants, self:read_string())
    end
  end

  -- Upvalues
  local n_uv = self:read_int()
  proto.upvalues = {}
  for i = 1, n_uv do
    proto.upvalues[i] = { instack = self:read_byte(), idx = self:read_byte() }
  end

  -- Protos
  local n_p = self:read_int()
  proto.protos = {}
  for i = 1, n_p do
    proto.protos[i] = self:read_proto()
  end

  -- Debug info (skip)
  local n_line = self:read_int()
  self.pos = self.pos + n_line * 4
  local n_loc = self:read_int()
  for i = 1, n_loc do
    self:read_string()
    self:read_int()
    self:read_int()
  end
  local n_upv = self:read_int()
  for i = 1, n_upv do
    self:read_string()
  end

  return proto
end

local LUA53_OP = {
  [0] = "MOVE", [1] = "LOADK", [2] = "LOADKX", [3] = "LOADBOOL", [4] = "LOADNIL",
  [5] = "GETUPVAL", [6] = "GETTABUP", [7] = "GETTABLE", [8] = "SETTABUP",
  [9] = "SETUPVAL", [10] = "SETTABLE", [11] = "NEWTABLE", [12] = "SELF",
  [13] = "ADD", [14] = "SUB", [15] = "MUL", [16] = "MOD", [17] = "POW",
  [18] = "DIV", [19] = "IDIV", [20] = "BITAND", [21] = "BITOR", [22] = "BITXOR",
  [23] = "SHL", [24] = "SHR", [25] = "UNM", [26] = "BITNOT", [27] = "NOT",
  [28] = "LEN", [29] = "CONCAT", [30] = "JMP", [31] = "EQ", [32] = "LT",
  [33] = "LE", [34] = "TEST", [35] = "TESTSET", [36] = "CALL", [37] = "TAILCALL",
  [38] = "RET", [39] = "FORLOOP", [40] = "FORPREP", [41] = "TFORCALL",
  [42] = "TFORLOOP", [43] = "SETLIST", [44] = "CLOSURE", [45] = "VARARG",
}

function BytecodeParser:decode_lua53(inst)
  local op = inst & 0x3F
  local a = (inst >> 6) & 0xFF
  local c = (inst >> 14) & 0x1FF
  local b = (inst >> 23) & 0x1FF
  local bx = (inst >> 14) & 0x3FFFF
  local sbx = bx - 0x1FFFF

  local name = LUA53_OP[op] or "UNKNOWN"
  local args = { a, b, c }
  if name == "JMP" or name == "FORLOOP" or name == "FORPREP" or name == "TFORLOOP" then
    args = { a, sbx }
  elseif name == "LOADK" or name == "CLOSURE" then
    args = { a, bx }
  elseif name == "LOADKX" then
    args = { a }
  elseif name == "TEST" then
    args = { a, 0, c }
  elseif name == "TESTSET" then
    args = { a, b, c }
  elseif name == "TAILCALL" then
    name = "CALL"
  end

  return { op = name, args = args }
end

function BytecodeParser:parse()
  -- Skip header
  self.pos = 13 -- simplified header skip
  return self:read_proto()
end

return BytecodeParser
