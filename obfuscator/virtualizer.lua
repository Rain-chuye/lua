local Virtualizer = {}
Virtualizer.__index = Virtualizer

local OP_MAP = {
  MOVE = 0, LOADK = 1, LOADBOOL = 2, LOADNIL = 3, GETUPVAL = 4,
  GETGLOBAL = 5, GETTABLE = 6, SETGLOBAL = 7, SETUPVAL = 8, SETTABLE = 9,
  NEWTABLE = 10, SELF = 11, ADD = 12, SUB = 13, MUL = 14, DIV = 15,
  IDIV = 16, MOD = 17, POW = 18, UNM = 19, NOT = 20, LEN = 21,
  CONCAT = 22, JMP = 23, EQ = 24, LT = 25, LE = 26, TEST = 27,
  TESTSET = 28, CALL = 29, TAILCALL = 30, RET = 31, FORLOOP = 32,
  FORPREP = 33, TFORCALL = 34, TFORLOOP = 35, SETLIST = 36, CLOSURE = 37,
  VARARG = 38, EXTRAARG = 39,
  -- Custom/Androlua+
  NEWARRAY = 40, JMP_IF_FALSE = 41, JMP_IF_TRUE = 42, DEFER = 43, TBC = 44,
  NE = 45, GT = 46, GE = 47, BITAND = 48, BITOR = 49, BITXOR = 50,
  SHL = 51, SHR = 52, BITNOT = 53, GETTABUP = 54, SETTABUP = 55,
  LOADKX = 56, TEST = 57, TESTSET = 58, GETTABUP = 54, SETTABUP = 55,
}

function Virtualizer.new()
  local self = setmetatable({}, Virtualizer)
  self.protos = {}
  return self
end

function Virtualizer:virtualize(ast)
  self.proto_count = 0
  self.xor_key = math.random(0, 0xFFFFFFFF)
  local main_proto = self:process_function(ast.body, ast.args or {})
  return main_proto
end

function Virtualizer:generate_opcode_map()
  local indices = {}
  for i = 0, 63 do table.insert(indices, i) end
  -- shuffle
  for i = #indices, 2, -1 do
    local j = math.random(i)
    indices[i], indices[j] = indices[j], indices[i]
  end

  local physical_to_logical = {}
  local logical_to_physical = {}

  local i = 1
  for name, logical_id in pairs(OP_MAP) do
    local physical_id = indices[i]
    physical_to_logical[physical_id] = logical_id
    logical_to_physical[logical_id] = physical_id
    i = i + 1
  end
  return physical_to_logical, logical_to_physical
end

function Virtualizer:pack_proto(proto, logical_to_physical, physical_to_logical)
  if not logical_to_physical then
    physical_to_logical, logical_to_physical = self:generate_opcode_map()
  end

  local b = {}
  for i, inst in ipairs(proto.instructions) do
    local logical_op = OP_MAP[inst.op] or 0
    local physical_op = logical_to_physical[logical_op] or 0

    local a = inst.args[1] or 0
    local b_arg = inst.args[2] or 0
    local c = inst.args[3] or 0

    -- Handle signed 16-bit for B (jumps)
    if b_arg < 0 then b_arg = b_arg + 0x10000 end

    -- Packing: OP(8) | A(8) | B(16) | C(16)
    local packed = physical_op + (a * 2^8) + (b_arg * 2^16) + (c * 2^32)

    -- XOR encryption (careful with large numbers and ~ operator)
    -- Lua 5.3 ~ works on 64-bit integers. 48 bits is fine.
    local encrypted = (packed ~ self.xor_key ~ i)
    table.insert(b, encrypted)
  end

  local protos = {}
  for _, p in ipairs(proto.protos) do
    table.insert(protos, self:pack_proto(p, logical_to_physical, physical_to_logical))
  end

  return {
    b = b,
    k = proto.constants,
    p = protos,
    m = physical_to_logical,
    upvalues = proto.upvalues,
  }
end

function Virtualizer:process_function(body, params, parent_ctx)
  self.proto_count = self.proto_count + 1
  local proto = {
    id = self.proto_count,
    instructions = {},
    constants = {},
    params = params,
    locals = {},
    upvalues = {},
    protos = {},
    max_stack = 0,
  }

  local ctx = {
    proto = proto,
    scope = { locals = {} },
    labels = {},
    jmp_targets = {},
    parent = parent_ctx,
    reg_count = 0,
    free_regs = {},
  }

  for _, p in ipairs(params) do
    self:add_local(ctx, p)
  end

  for _, stmt in ipairs(body) do
    ctx.temp_reg_ptr = #ctx.proto.locals
    self:gen_stmt(ctx, stmt)
  end

  -- Add implicit return
  table.insert(proto.instructions, { op = "RET", args = { 0 } })

  return proto
end

function Virtualizer:add_local(ctx, name)
  table.insert(ctx.proto.locals, name)
  local reg = #ctx.proto.locals
  ctx.scope.locals[name] = reg
  if reg > ctx.reg_count then ctx.reg_count = reg end
  return reg
end

function Virtualizer:get_temp_reg(ctx)
  -- Temps start after all currently defined locals
  local start = #ctx.proto.locals
  if ctx.temp_reg_ptr and ctx.temp_reg_ptr > start then
    ctx.temp_reg_ptr = ctx.temp_reg_ptr + 1
  else
    ctx.temp_reg_ptr = start + 1
  end
  if ctx.temp_reg_ptr > ctx.reg_count then ctx.reg_count = ctx.temp_reg_ptr end
  return ctx.temp_reg_ptr
end

function Virtualizer:free_reg(ctx, reg)
  if reg > #ctx.proto.locals then -- Don't free actual locals
    table.insert(ctx.free_regs, reg)
  end
end

function Virtualizer:find_var(ctx, name)
  if ctx.scope.locals[name] then
    return "local", ctx.scope.locals[name]
  end

  -- Check upvalues
  for i, uv in ipairs(ctx.proto.upvalues) do
    if uv.name == name then
      return "upvalue", i - 1
    end
  end

  -- Check parent
  if ctx.parent then
    local type, idx = self:find_var(ctx.parent, name)
    if type == "local" or type == "upvalue" then
      table.insert(ctx.proto.upvalues, { name = name, type = type, index = idx })
      return "upvalue", #ctx.proto.upvalues - 1
    end
  end

  return "global", name
end

function Virtualizer:gen_stmt(ctx, stmt)
  if stmt.type == "LocalAssign" then
    for i, name in ipairs(stmt.names) do
      local reg = self:add_local(ctx, name)
      if stmt.values[i] then
        self:gen_expr(ctx, stmt.values[i], reg)
      else
        table.insert(ctx.proto.instructions, { op = "LOADNIL", args = { reg } })
      end
      if stmt.tbc and stmt.tbc[i] then
        table.insert(ctx.proto.instructions, { op = "TBC", args = { reg } })
      end
    end
  elseif stmt.type == "Assign" then
    for i, target in ipairs(stmt.targets) do
      local val_reg = self:get_temp_reg(ctx)
      self:gen_expr(ctx, stmt.values[i], val_reg)
      self:gen_assign(ctx, target, val_reg)
    end
  elseif stmt.type == "Call" or stmt.type == "MethodCall" then
    self:gen_expr(ctx, stmt, nil)
  elseif stmt.type == "Return" then
    local start_reg = self:get_temp_reg(ctx)
    for i, val in ipairs(stmt.values) do
      self:gen_expr(ctx, val, start_reg + i - 1)
    end
    table.insert(ctx.proto.instructions, { op = "RET", args = { start_reg, #stmt.values } })
  elseif stmt.type == "If" then
    local cond_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, stmt.condition, cond_reg)
    local jmp_false = self:add_inst(ctx, "JMP_IF_FALSE", { cond_reg, 0 })

    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end

    local jmp_end
    if stmt.else_body or (stmt.elseifs and #stmt.elseifs > 0) then
        jmp_end = self:add_inst(ctx, "JMP", { 0 })
    end

    self:patch_jmp(ctx, jmp_false)

    if stmt.elseifs then
      for _, elseif_stmt in ipairs(stmt.elseifs) do
        local e_cond_reg = self:get_temp_reg(ctx)
        self:gen_expr(ctx, elseif_stmt.condition, e_cond_reg)
        local e_jmp_false = self:add_inst(ctx, "JMP_IF_FALSE", { e_cond_reg, 0 })
        for _, s in ipairs(elseif_stmt.body) do self:gen_stmt(ctx, s) end
        if not jmp_end then jmp_end = self:add_inst(ctx, "JMP", { 0 }) end
        self:patch_jmp(ctx, e_jmp_false)
      end
    end

    if stmt.else_body then
       for _, s in ipairs(stmt.else_body) do self:gen_stmt(ctx, s) end
    end
    if jmp_end then self:patch_jmp(ctx, jmp_end) end
  elseif stmt.type == "While" then
    local start_pc = #ctx.proto.instructions + 1
    local cond_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, stmt.condition, cond_reg)
    local jmp_end = self:add_inst(ctx, "JMP_IF_FALSE", { cond_reg, 0 })

    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end
    self:add_inst(ctx, "JMP", { start_pc - (#ctx.proto.instructions + 1) })

    self:patch_jmp(ctx, jmp_end)
  elseif stmt.type == "Do" then
    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end
  elseif stmt.type == "Repeat" then
    local start_pc = #ctx.proto.instructions + 1
    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end
    local cond_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, stmt.condition, cond_reg)
    self:add_inst(ctx, "JMP_IF_FALSE", { cond_reg, start_pc - (#ctx.proto.instructions + 1) })
  elseif stmt.type == "FunctionDef" then
    local reg = self:get_temp_reg(ctx)
    local child_proto = self:process_function(stmt.body, stmt.args, ctx)
    table.insert(ctx.proto.protos, child_proto)
    self:add_inst(ctx, "CLOSURE", { reg, #ctx.proto.protos - 1 })
    local target = { type = "Variable", name = stmt.name }
    self:gen_assign(ctx, target, reg)
  elseif stmt.type == "Defer" then
    -- Special instruction for defer
    local child_proto = self:process_function({stmt.body}, {}, ctx)
    table.insert(ctx.proto.protos, child_proto)
    self:add_inst(ctx, "DEFER", { #ctx.proto.protos - 1 })
  elseif stmt.type == "ForRange" then
    local base = self:get_temp_reg(ctx)
    self:gen_expr(ctx, stmt.init, base)
    self:gen_expr(ctx, stmt.limit, base + 1)
    if stmt.step then
      self:gen_expr(ctx, stmt.step, base + 2)
    else
      local k = self:add_k(ctx, 1)
      self:add_inst(ctx, "LOADK", { base + 2, k })
    end
    local prep = self:add_inst(ctx, "FORPREP", { base, 0 })
    local loop_start = #ctx.proto.instructions + 1

    -- Loop body
    local loop_var_reg = self:add_local(ctx, stmt.name)
    -- TODO: handle local scope better. For now just move from internal reg.
    self:add_inst(ctx, "MOVE", { loop_var_reg, base })

    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end

    local loop_end = #ctx.proto.instructions + 1
    self:add_inst(ctx, "FORLOOP", { base, loop_start - loop_end - 1 })
    self:patch_jmp(ctx, prep, loop_end)
  elseif stmt.type == "ForIn" then
    -- Simplified ForIn
    local base = self:get_temp_reg(ctx)
    for i, iter in ipairs(stmt.iterators) do
      self:gen_expr(ctx, iter, base + i - 1)
    end
    -- base: f, base+1: s, base+2: var
    local loop_start = #ctx.proto.instructions + 1
    self:add_inst(ctx, "TFORCALL", { base, #stmt.names })
    self:add_inst(ctx, "TFORLOOP", { base + 2, 0 }) -- will patch
    local jmp_end = self:add_inst(ctx, "JMP_IF_FALSE", { base + 2, 0 })

    -- Body
    for i, name in ipairs(stmt.names) do
      local reg = self:add_local(ctx, name)
      self:add_inst(ctx, "MOVE", { reg, base + 2 + i - 1 })
    end
    for _, s in ipairs(stmt.body) do self:gen_stmt(ctx, s) end
    self:add_inst(ctx, "JMP", { loop_start - (#ctx.proto.instructions + 1) })
    self:patch_jmp(ctx, jmp_end)
  end
end

function Virtualizer:gen_expr(ctx, expr, reg)
  local is_temp = false
  if not reg then
    reg = self:get_temp_reg(ctx)
    is_temp = true
  end
  if expr.type == "Number" then
    local k = self:add_k(ctx, expr.value)
    self:add_inst(ctx, "LOADK", { reg, k })
  elseif expr.type == "String" then
    local k = self:add_k(ctx, expr.value)
    self:add_inst(ctx, "LOADK", { reg, k })
  elseif expr.type == "Variable" then
    local type, idx = self:find_var(ctx, expr.name)
    if type == "local" then
      self:add_inst(ctx, "MOVE", { reg, idx })
    elseif type == "upvalue" then
      self:add_inst(ctx, "GETUPVAL", { reg, idx })
    else
      -- Global
      local k = self:add_k(ctx, expr.name)
      self:add_inst(ctx, "GETGLOBAL", { reg, k })
    end
  elseif expr.type == "BinaryOp" then
    local left = self:get_temp_reg(ctx)
    local right = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.left, left)
    self:gen_expr(ctx, expr.right, right)
    local op_map = {
      ["+"] = "ADD", ["-"] = "SUB", ["*"] = "MUL", ["/"] = "DIV", ["//"] = "IDIV", ["%"] = "MOD",
      ["^"] = "POW", ["&"] = "BITAND", ["|"] = "BITOR", ["~"] = "BITXOR", ["<<"] = "SHL", [">>"] = "SHR",
      ["=="] = "EQ", ["!="] = "NE", ["~="] = "NE", ["<"] = "LT", [">"] = "GT",
      ["<="] = "LE", [">="] = "GE", [".."] = "CONCAT", ["and"] = "AND", ["or"] = "OR",
      ["TK_AND"] = "AND", ["TK_OR"] = "OR",
    }
    self:add_inst(ctx, op_map[expr.op] or "UNKNOWN", { reg, left, right })
  elseif expr.type == "UnaryOp" then
    local right = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.right, right)
    local op_map = {
      ["-"] = "UNM", ["not"] = "NOT", ["!"] = "NOT", ["TK_NOT"] = "NOT", ["TK_BNOT"] = "NOT", ["#"] = "LEN", ["~"] = "BITNOT",
    }
    self:add_inst(ctx, op_map[expr.op] or "UNKNOWN", { reg, right })
  elseif expr.type == "Literal" then
    if expr.value == "TK_TRUE" then
       self:add_inst(ctx, "LOADBOOL", { reg, 1, 0 })
    elseif expr.value == "TK_FALSE" then
       self:add_inst(ctx, "LOADBOOL", { reg, 0, 0 })
    elseif expr.value == "TK_NIL" then
       self:add_inst(ctx, "LOADNIL", { reg })
    end
  elseif expr.type == "Call" then
    local func_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.base, func_reg)
    -- Reserve argument registers
    local arg_regs = {}
    for i = 1, #expr.args do
      arg_regs[i] = self:get_temp_reg(ctx)
    end
    for i, arg in ipairs(expr.args) do
      self:gen_expr(ctx, arg, arg_regs[i])
    end
    self:add_inst(ctx, "CALL", { func_reg, #expr.args, 1 })
    if reg ~= func_reg then
      self:add_inst(ctx, "MOVE", { reg, func_reg })
    end
  elseif expr.type == "MethodCall" then
    local base_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.base, base_reg)
    local k = self:add_k(ctx, expr.method)
    self:add_inst(ctx, "SELF", { base_reg, k })
    -- Reserve argument registers (SELF already uses base_reg and base_reg + 1)
    local arg_regs = {}
    for i = 1, #expr.args do
      arg_regs[i] = self:get_temp_reg(ctx)
    end
    for i, arg in ipairs(expr.args) do
      self:gen_expr(ctx, arg, arg_regs[i])
    end
    self:add_inst(ctx, "CALL", { base_reg, #expr.args + 1, 1 })
    if reg ~= base_reg then
      self:add_inst(ctx, "MOVE", { reg, base_reg })
    end
  elseif expr.type == "Index" then
    local base_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.base, base_reg)
    local key_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, expr.key, key_reg)
    self:add_inst(ctx, "GETTABLE", { reg, base_reg, key_reg })
  elseif expr.type == "Table" then
    self:add_inst(ctx, "NEWTABLE", { reg, #expr.fields, 0 })
    for i, field in ipairs(expr.fields) do
      local val_reg = self:get_temp_reg(ctx)
      self:gen_expr(ctx, field.value, val_reg)
      if field.key then
        local key_reg = self:get_temp_reg(ctx)
        self:gen_expr(ctx, field.key, key_reg)
        self:add_inst(ctx, "SETTABLE", { reg, key_reg, val_reg })
      else
        self:add_inst(ctx, "SETLIST", { reg, i, val_reg })
      end
    end
  elseif expr.type == "Function" then
    local child_proto = self:process_function(expr.body, expr.args, ctx)
    table.insert(ctx.proto.protos, child_proto)
    self:add_inst(ctx, "CLOSURE", { reg, #ctx.proto.protos - 1 })
  elseif expr.type == "Array" then
    self:add_inst(ctx, "NEWARRAY", { reg, #expr.elements })
    for i, el in ipairs(expr.elements) do
      local val_reg = self:get_temp_reg(ctx)
      self:gen_expr(ctx, el, val_reg)
      self:add_inst(ctx, "SETLIST", { reg, i, val_reg })
    end
  end
  return reg
end

function Virtualizer:add_k(ctx, val)
  for i, k in ipairs(ctx.proto.constants) do
    if k == val then return i - 1 end
  end
  table.insert(ctx.proto.constants, val)
  return #ctx.proto.constants - 1
end

function Virtualizer:add_inst(ctx, op, args)
  table.insert(ctx.proto.instructions, { op = op, args = args })
  return #ctx.proto.instructions
end

function Virtualizer:patch_jmp(ctx, pc, target)
  target = target or #ctx.proto.instructions + 1
  local inst = ctx.proto.instructions[pc]
  inst.args[#inst.args] = target - pc - 1
end

function Virtualizer:get_temp_reg(ctx)
  ctx.reg_count = ctx.reg_count + 1
  return ctx.reg_count
end

function Virtualizer:gen_assign(ctx, target, val_reg)
  if target.type == "Variable" then
    local type, idx = self:find_var(ctx, target.name)
    if type == "local" then
      self:add_inst(ctx, "MOVE", { idx, val_reg })
    elseif type == "upvalue" then
      self:add_inst(ctx, "SETUPVAL", { val_reg, idx })
    else
      local k = self:add_k(ctx, target.name)
      self:add_inst(ctx, "SETGLOBAL", { k, val_reg })
    end
  elseif target.type == "Index" then
    local base_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, target.base, base_reg)
    local key_reg = self:get_temp_reg(ctx)
    self:gen_expr(ctx, target.key, key_reg)
    self:add_inst(ctx, "SETTABLE", { base_reg, key_reg, val_reg })
  end
end

return Virtualizer
