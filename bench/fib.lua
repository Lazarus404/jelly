local function fib(n)
  if n <= 1 then
    return 1
  end
  return fib(n - 1) + fib(n - 2)
end

local r = fib(30)
assert(r == 1346269)
io.write("ok")

