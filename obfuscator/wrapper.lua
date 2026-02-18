local Wrapper = {}

function Wrapper.generate(vm_template, packed_s, k1, k2)
  local body = vm_template

  -- Simple literal obfuscation
  body = body:gsub("== (%d+)", function(n)
    local val = tonumber(n)
    if val and val > 0 then
      local off = math.random(1, 100)
      return "== (" .. (val + off) .. " - " .. off .. ")"
    end
  end)

  -- Final format
  local final = body:gsub("%%s", function()
    local res = packed_s
    packed_s = tostring(k1)
    k1 = tostring(k2)
    return res
  end)

  return final
end

return Wrapper
