local function eq_i32(a, b)
  assert(a == b)
end

local function eq_f64(a, b)
  assert(a == b)
end

local small = 32767
local med = 2147418112
local less = -2147483648
local spe1 = -1073741824

eq_i32(small * small, 1073676289)
eq_i32(med + med, -131072)
eq_i32(less + 1, -2147483647)
eq_i32(spe1 / 2, -536870912)

eq_f64(1.0 - 1.25, -0.25)
eq_f64(1.25 - 1.0, 0.25)
eq_f64(med + 1.1, 2147418113.1)
eq_f64(1.1 + med, 2147418113.1)

io.write("ok")

