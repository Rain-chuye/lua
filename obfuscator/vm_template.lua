local vm_template = [==[
return (function(_PR, _K1, _K2)
  local _G = _G or getfenv()
  local _ENV = _ENV or _G
  local _UP = {}

  local function _EXEC(_PR, _ENV, _UP, ...)
    local _B = _PR.b
    local _K = _PR.k
    local _P = _PR.p
    local _M = _PR.m
    local _S = { ... }
    local _PC = 1
    local _DF = {}

    local function _DECODE(v, i)
      return (v ~ _K1 ~ i)
    end

    local function _GETK(idx)
      local kv = _K[idx + 1]
      if type(kv) == "table" then
        if kv.data then
          local s = ""
          for _, b in ipairs(kv.data) do
            s = s .. string.char(((b ~ kv.keys[3]) - kv.keys[2]) % 256 ~ kv.keys[1])
          end
          return s
        elseif kv.type == "MBA_ADD" then
          return kv.v - kv.x
        elseif kv.type == "MBA_XOR" then
          return kv.v ~ kv.x
        end
      end
      return kv
    end

    local _STATE = 0
    while _PC <= #_B or _STATE ~= 0 do
      if _STATE == 0 then
        local _INST = _DECODE(_B[_PC], _PC)
        local _R_OP = _INST & 0xFF
        local _A = (_INST >> 8) & 0xFF
        local _B_ARG = (_INST >> 16) & 0xFFFF
        local _C = (_INST >> 32) & 0xFFFF
        if _B_ARG > 0x8000 then _B_ARG = _B_ARG - 0x10000 end

        local _OP = _M[_R_OP]
        _STATE = (_OP + 1) * 7

        if _STATE == 7 then -- MOVE
          _S[_A] = _S[_B_ARG]
        elseif _STATE == 14 then -- LOADK
          _S[_A] = _GETK(_B_ARG)
        elseif _STATE == 21 then -- LOADBOOL
          _S[_A] = (_B_ARG ~= 0)
          if _C ~= 0 then _PC = _PC + 1 end
        elseif _STATE == 28 then -- LOADNIL
          _S[_A] = nil
        elseif _STATE == 35 then -- GETUPVAL
          _S[_A] = _UP[_B_ARG + 1].v
        elseif _STATE == 42 then -- GETGLOBAL
          _S[_A] = _ENV[_GETK(_B_ARG)]
        elseif _STATE == 49 then -- GETTABLE
          _S[_A] = _S[_B_ARG][_S[_C]]
        elseif _STATE == 56 then -- SETGLOBAL
          _ENV[_GETK(_A)] = _S[_B_ARG]
        elseif _STATE == 63 then -- SETUPVAL
          _UP[_B_ARG + 1].v = _S[_A]
        elseif _STATE == 70 then -- SETTABLE
          _S[_A][_S[_B_ARG]] = _S[_C]
        elseif _STATE == 77 then -- NEWTABLE
          _S[_A] = {}
        elseif _STATE == 84 then -- SELF
          _S[_A + 1] = _S[_B_ARG]
          _S[_A] = _S[_B_ARG][_GETK(_C)]
        elseif _STATE == 91 then -- ADD
          _S[_A] = _S[_B_ARG] + _S[_C]
        elseif _STATE == 98 then -- SUB
          _S[_A] = _S[_B_ARG] - _S[_C]
        elseif _STATE == 105 then -- MUL
          _S[_A] = _S[_B_ARG] * _S[_C]
        elseif _STATE == 112 then -- DIV
          _S[_A] = _S[_B_ARG] / _S[_C]
        elseif _STATE == 119 then -- IDIV
          _S[_A] = _S[_B_ARG] // _S[_C]
        elseif _STATE == 126 then -- MOD
          _S[_A] = _S[_B_ARG] % _S[_C]
        elseif _STATE == 133 then -- POW
          _S[_A] = _S[_B_ARG] ^ _S[_C]
        elseif _STATE == 140 then -- UNM
          _S[_A] = -_S[_B_ARG]
        elseif _STATE == 147 then -- NOT
          _S[_A] = not _S[_B_ARG]
        elseif _STATE == 154 then -- LEN
          _S[_A] = #_S[_B_ARG]
        elseif _STATE == 161 then -- CONCAT
          _S[_A] = _S[_B_ARG] .. _S[_C]
        elseif _STATE == 168 then -- JMP
          _PC = _PC + _B_ARG
        elseif _STATE == 175 then -- EQ
          _S[_A] = (_S[_B_ARG] == _S[_C])
        elseif _STATE == 182 then -- LT
          _S[_A] = (_S[_B_ARG] < _S[_C])
        elseif _STATE == 189 then -- LE
          _S[_A] = (_S[_B_ARG] <= _S[_C])
        elseif _STATE == 210 then -- CALL
          local args = {}
          for i = 1, _B_ARG do table.insert(args, _S[_A + i]) end
          local func = _S[_A]
          local res = { func(table.unpack(args)) }
          for i = 1, _C do _S[_A + i - 1] = res[i] end
        elseif _STATE == 224 then -- RET
          for i = #_DF, 1, -1 do _DF[i]() end
          local res = {}
          for i = 1, _B_ARG do table.insert(res, _S[_A + i - 1]) end
          return table.unpack(res)
        elseif _STATE == 231 then -- FORLOOP
          local step = _S[_A + 2]
          local idx = _S[_A] + step
          local limit = _S[_A + 1]
          if (step > 0 and idx <= limit) or (step < 0 and idx >= limit) then
            _S[_A] = idx
            _PC = _PC + _B_ARG
          end
        elseif _STATE == 238 then -- FORPREP
          _S[_A] = _S[_A] - _S[_A + 2]
          _PC = _PC + _B_ARG
        elseif _STATE == 245 then -- TFORCALL
          local res = { _S[_A](_S[_A + 1], _S[_A + 2]) }
          for i = 1, _C do _S[_A + 2 + i] = res[i] end
        elseif _STATE == 252 then -- TFORLOOP
          if _S[_A] ~= nil then
            _S[_A - 1] = _S[_A]
            _PC = _PC + _B_ARG
          end
        elseif _STATE == 259 then -- SETLIST
          local t = _S[_A]
          t[_B_ARG] = _S[_C]
        elseif _STATE == 266 then -- CLOSURE
          local child_pr = _P[_B_ARG + 1]
          local _captured = {}
          for i, uv in ipairs(child_pr.upvalues or {}) do
            if uv.type == "local" then
              _captured[i] = { v = _S[uv.index] }
            else
              _captured[i] = _UP[uv.index + 1]
            end
          end
          _S[_A] = function(...)
             return _EXEC(child_pr, _ENV, _captured, ...)
          end
        elseif _STATE == 287 then -- NEWARRAY
          _S[_A] = {}
        elseif _STATE == 294 then -- JMP_IF_FALSE
          if not _S[_A] then _PC = _PC + _B_ARG end
        elseif _STATE == 301 then -- JMP_IF_TRUE
          if _S[_A] then _PC = _PC + _B_ARG end
        elseif _STATE == 308 then -- DEFER
          local child_pr = _P[_A + 1]
          table.insert(_DF, function() _EXEC(child_pr, _ENV, _UP) end)
        elseif _STATE == 315 then -- TBC
          local val = _S[_A]
        elseif _STATE == 322 then -- NE
          _S[_A] = (_S[_B_ARG] ~= _S[_C])
        elseif _STATE == 329 then -- GT
          _S[_A] = (_S[_B_ARG] > _S[_C])
        elseif _STATE == 336 then -- GE
          _S[_A] = (_S[_B_ARG] >= _S[_C])
        elseif _STATE == 343 then -- BITAND
          _S[_A] = _S[_B_ARG] & _S[_C]
        elseif _STATE == 350 then -- BITOR
          _S[_A] = _S[_B_ARG] | _S[_C]
        elseif _STATE == 357 then -- BITXOR
          _S[_A] = _S[_B_ARG] ~ _S[_C]
        elseif _STATE == 364 then -- SHL
          _S[_A] = _S[_B_ARG] << _S[_C]
        elseif _STATE == 371 then -- SHR
          _S[_A] = _S[_B_ARG] >> _S[_C]
        elseif _STATE == 378 then -- BITNOT
          _S[_A] = ~_S[_B_ARG]
        end
        _STATE = 0
        _PC = _PC + 1
      end
    end
  end

  return _EXEC(_PR, _ENV, _UP)
end)(%s, %s, %s)
]==]

return vm_template
