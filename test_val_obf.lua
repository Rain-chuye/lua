return (function(...)
local _G_ENV = _G or _ENV; local _VAULT = {}; local _GTY, _GIP, _GERR, _GTON, _GTOS = type, ipairs, error, tonumber, tostring
local _L_NAMES = {'string','table','math','io','os','debug','coroutine','package','utf8','assert','pcall','print','select','type','tostring'}
for _, n in _GIP(_L_NAMES) do _VAULT[n] = _G_ENV[n]; if n ~= 'print' and n ~= '_G' then _G_ENV[n] = nil end end
local _M_FL, _T_UP = _VAULT['math'].floor, _VAULT['table'].unpack
local function _EXEC(_PR, _UP, ...)
  local _S, _SS, _P, _K, _M, _V = {}, 0, 1, _PR.k, _PR.m, {}
  local _D_MAP = {}
  local _S_CH, _T_CO = _VAULT['string'].char, _VAULT['table'].concat
  local function _d(v) if _GTY(v) ~= 'string' then return v end; local r = {}; for i=1, #v do r[#r+1] = _S_CH(((v:byte(i) ~ 30) - 20) % 256 ~ 10) end; local rs = _T_CO(r); local t, val = rs:sub(1,1), rs:sub(2); if t == 's' then return val elseif t == 'n' then return _GTON(val) elseif t == 'b' then return val == 't' else return nil end end
  local function _GFG(n_val) local en = _D_MAP[n_val]; local dn = en and _d(en) or _d(n_val); return _VAULT[dn] or _G_ENV[dn] end
  while _P <= #_PR.b do
    local _PCK = _PR.b[_P]; _P = _P + 1; local _OPI, _ARG = _PCK % 256, _M_FL(_PCK / 256); local _OPN = _d(_M[_OPI])
    if _OPN == 'LOADK' then _SS = _SS + 1; _S[_SS] = _d(_K[_ARG])
    elseif _OPN == 'GETVAR' then local n = _d(_K[_ARG]); local val = _V[n]; if val == nil then val = _GFG(n) end; _SS = _SS + 1; _S[_SS] = val
    elseif _OPN == 'SETVAR' then local n = _d(_K[_ARG]); _V[n] = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1
    elseif _OPN == 'CALL' then local as = {}; for i=1, _ARG do as[_ARG-i+1] = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1 end; local f = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; if not f then _GERR('CALL: function is nil') end; local re = {_VAULT['pcall'](f, _T_UP(as))}; if not re[1] then _GERR(re[2]) end; _SS = _SS + 1; _S[_SS] = re[2]
    elseif _OPN == 'RET' then local re = {}; for i=1, _ARG do re[_ARG-i+1] = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1 end; return _T_UP(re)
    elseif _OPN == 'GETTABLE' then local k = _S[_SS]; _S[_SS] = nil; _SS = _SS - 1; local t = _S[_SS]; _S[_SS] = t[k]
    elseif _OPN == 'DUP' then _SS = _SS + 1; _S[_SS] = _S[_SS - _ARG - 1]
    elseif _OPN == 'SWAP' then local a = _S[_SS]; _S[_SS] = _S[_SS-1]; _S[_SS-1] = a
    end
  end
end
return _EXEC({b={257,513,13,265},k={"\147\081\082\083","\147\076\077\078",},m={"\076\079\065\068\075","\071\069\084\086\065\082","\083\069\084\086\065\082","\071\069\084\084\065\066\076\069","\083\069\084\084\065\066\076\069","\083\069\084\084\065\066\076\069\095\073\077\077","\078\069\087\084\065\066\076\069","\067\065\076\076","\082\069\084","\086\065\082\065\082\071","\074\077\080","\074\077\080\095\073\070\095\070\065\076\083\069","\065\068\068","\083\085\066","\077\085\076","\068\073\086","\069\081","\076\084","\067\079\078\067\065\084","\078\079\084","\067\076\079\083\085\082\069","\076\079\065\068\095\086\065","\080\073\067\075\095\082\069\083\085\076\084","\080\079\080","\068\085\080","\083\087\065\080","\070\085\083\069\095\079\066\074\095\077",}}, nil, ...)
end)(...)