local Engine = {}
Engine.__index = Engine

function Engine.new(options)
  local self = setmetatable({}, Engine)
  self.options = options or {}
  return self
end

function Engine:encrypt_string(s)
  if #s > 1024 then return s end -- Only handle strings <= 1kb
  local key1 = math.random(1, 255)
  local key2 = math.random(1, 255)
  local key3 = math.random(1, 255)

  local res = {}
  for i = 1, #s do
    local b = s:byte(i)
    b = ((b ~ key1) + key2) % 256 ~ key3
    table.insert(res, b)
  end

  -- Return a structure that the VM can decode
  return { data = res, keys = { key1, key2, key3 } }
end

function Engine:apply_mba(n)
  if type(n) ~= "number" or math.floor(n) ~= n then return n end
  if math.random() > (self.options.int_rate or 0.5) then return n end

  -- Simple MBA: n = (n + x) - x or (n ^ x) ^ x
  local x = math.random(1, 10000)
  local op = math.random(1, 2)
  if op == 1 then
    return { type = "MBA_ADD", v = n + x, x = x }
  else
    return { type = "MBA_XOR", v = n ~ x, x = x }
  end
end

function Engine:identify_dependencies(ast)
  local deps = {}
  local function scan(node)
    if not node or type(node) ~= "table" then return end
    if node.type == "Call" then
       if node.base.type == "Variable" and (node.base.name == "require" or node.base.name == "import") then
          if node.args[1] and node.args[1].type == "String" then
             table.insert(deps, { type = node.base.name, name = node.args[1].value })
          end
       end
    end
    for k, v in pairs(node) do
      if type(v) == "table" then scan(v) end
    end
  end
  scan(ast)

  local comments = ""
  for _, dep in ipairs(deps) do
    comments = comments .. "--" .. dep.type .. " \"" .. dep.name .. "\"\n"
  end
  return comments
end

function Engine:obfuscate_proto(proto)
  if not proto then return end
  -- 1. Encrypt constants
  proto.constants = proto.constants or {}
  for i, k in ipairs(proto.constants) do
    if type(k) == "string" then
      proto.constants[i] = self:encrypt_string(k)
    elseif type(k) == "number" then
      proto.constants[i] = self:apply_mba(k)
    end
  end

  -- 2. Junk instructions / Opaque predicates
  if self.options.junk_rate and self.options.junk_rate > 0 then
    local new_insts = {}
    for i, inst in ipairs(proto.instructions) do
      if math.random() < self.options.junk_rate then
        -- Insert a junk jump
        table.insert(new_insts, { op = "JMP", args = { 1 } })
        table.insert(new_insts, { op = "EXTRAARG", args = { math.random(0, 0xFFFF) } })
      end
      table.insert(new_insts, inst)
    end
    proto.instructions = new_insts
  end

  -- 3. Recursive for child protos
  local children = proto.protos or proto.p or {}
  for _, p in ipairs(children) do
    self:obfuscate_proto(p)
  end
end

return Engine
