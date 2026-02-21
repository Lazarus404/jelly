local function pow2(k)
  local r = 1
  for _ = 1, k do
    r = r + r
  end
  return r
end

local function fill_vals(i, d, idx, vals)
  vals[idx] = i
  if d == 0 then
    return
  end
  local i2 = i + i
  fill_vals(i2 - 1, d - 1, idx + idx + 1, vals)
  fill_vals(i2, d - 1, idx + idx + 2, vals)
end

local function check(idx, d, vals)
  local v = vals[idx]
  if d == 0 then
    return v
  end
  return v + check(idx + idx + 1, d - 1, vals) - check(idx + idx + 2, d - 1, vals)
end

local function tree_check(i, d)
  local n_nodes = pow2(d + 1) - 1
  local vals = {}
  fill_vals(i, d, 0, vals)
  return check(0, d, vals)
end

local arg = 10
local min_depth = 4
local max_depth = (min_depth + 2 < arg) and arg or (min_depth + 2)
local stretch_depth = max_depth + 1

assert(tree_check(0, stretch_depth) == -1)
assert(tree_check(0, max_depth) == -1)

for d = min_depth, max_depth, 2 do
  local niter = pow2(max_depth - d + min_depth)
  local c = 0
  for i = 1, niter do
    c = c + tree_check(i, d) + tree_check(-i, d)
  end
  assert(c == -(niter + niter))
end

io.write("ok")

