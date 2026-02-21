local function nsieve(m)
  local a = {}
  for i = 1, m do
    a[i] = false
  end
  local count = 0
  for i = 2, m - 1 do
    if not a[i] then
      count = count + 1
      local j = i + i
      while j < m do
        a[j] = true
        j = j + i
      end
    end
  end
  return count
end

local n = 2
local c0 = nsieve(10000 * (2 ^ n))
local c1 = nsieve(10000 * (2 ^ (n - 1)))
local c2 = nsieve(10000 * (2 ^ (n - 2)))

assert(c0 == 4203)
assert(c1 == 2262)
assert(c2 == 1229)
io.write("ok")

