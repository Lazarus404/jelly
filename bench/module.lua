local function boot(i)
  return { x = i, y = i + 1, z = i + 2 }
end

local n = 100000
local cache = {}
local sum = 0

for i = 0, n - 1 do
  cache = boot(i)
  sum = sum + cache.y
end

assert(sum > 0)
io.write("ok")

