local function ack(x, y)
  if x == 0 then
    return y + 1
  end
  if y == 0 then
    return ack(x - 1, 1)
  end
  return ack(x - 1, ack(x, y - 1))
end

local function fib(n)
  if n < 2 then
    return 1
  end
  return fib(n - 2) + fib(n - 1)
end

local function fib_fp(n)
  if n < 2.0 then
    return 1.0
  end
  return fib_fp(n - 2.0) + fib_fp(n - 1.0)
end

local function tak(x, y, z)
  if y >= x then
    return z
  end
  return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
end

local n_in = 4
local n = n_in - 1

assert(ack(3, n + 1) == 125)
assert(fib_fp(n + 28.0) == 2178309.0)
assert(tak(3 * n, 2 * n, n) == 6)
assert(fib(3) == 3)

io.write("ok")

