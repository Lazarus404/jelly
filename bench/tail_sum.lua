-- Tail-recursive sum (Lua has proper tail calls)
local function sum(n, acc)
  if n == 0 then return acc end
  return sum(n - 1, acc + n)
end

local n = 50000
local r = sum(n, 0)
assert(r == 1250025000)
print("ok")
