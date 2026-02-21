local x = 1.0
local y = 1.0
local i = 1000000

while i > 0 do
  x = x * 1.000000001
  y = y / 1.000000001
  i = i - 1
end

local p = x * y
local d = (p > 1.0) and (p - 1.0) or (1.0 - p)
assert(d < 1e-6)
io.write("ok")

