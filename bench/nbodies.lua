local pi = 3.141592653589793
local solar_mass = 4.0 * pi * pi
local days_per_year = 365.24

local function pair(dt, b, b2)
  local dx = b.x - b2.x
  local dy = b.y - b2.y
  local dz = b.z - b2.z
  local dist = math.sqrt(dx * dx + dy * dy + dz * dz)
  local mag = dt / (dist * dist * dist)
  local bm = b.mass * mag
  local b2m = b2.mass * mag
  b.vx = b.vx - dx * b2m
  b.vy = b.vy - dy * b2m
  b.vz = b.vz - dz * b2m
  b2.vx = b2.vx + dx * bm
  b2.vy = b2.vy + dy * bm
  b2.vz = b2.vz + dz * bm
end

local function advance(dt, sun, jup, sat, ura, nep)
  pair(dt, sun, jup)
  pair(dt, sun, sat)
  pair(dt, sun, ura)
  pair(dt, sun, nep)
  pair(dt, jup, sat)
  pair(dt, jup, ura)
  pair(dt, jup, nep)
  pair(dt, sat, ura)
  pair(dt, sat, nep)
  pair(dt, ura, nep)

  for _, b in ipairs({ sun, jup, sat, ura, nep }) do
    b.x = b.x + dt * b.vx
    b.y = b.y + dt * b.vy
    b.z = b.z + dt * b.vz
  end
end

local function energy(sun, jup, sat, ura, nep)
  local e = 0.0
  local bodies = { sun, jup, sat, ura, nep }
  for i = 1, #bodies do
    local b = bodies[i]
    e = e + 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz)
    for j = i + 1, #bodies do
      local b2 = bodies[j]
      local dx = b.x - b2.x
      local dy = b.y - b2.y
      local dz = b.z - b2.z
      local dist = math.sqrt(dx * dx + dy * dy + dz * dz)
      e = e - (b.mass * b2.mass) / dist
    end
  end
  return e
end

local function offset_momentum(sun, jup, sat, ura, nep)
  local px = jup.vx * jup.mass + sat.vx * sat.mass + ura.vx * ura.mass + nep.vx * nep.mass
  local py = jup.vy * jup.mass + sat.vy * sat.mass + ura.vy * ura.mass + nep.vy * nep.mass
  local pz = jup.vz * jup.mass + sat.vz * sat.mass + ura.vz * ura.mass + nep.vz * nep.mass
  sun.vx = -px / solar_mass
  sun.vy = -py / solar_mass
  sun.vz = -pz / solar_mass
end

local function round9(e)
  local scaled = e * 1000000000.0
  local adj = (scaled < 0.0) and (scaled - 0.5) or (scaled + 0.5)
  -- Portable trunc after adjustment (Lua 5.1+)
  return (adj < 0.0) and math.ceil(adj) or math.floor(adj)
end

local sun = { x = 0.0, y = 0.0, z = 0.0, vx = 0.0, vy = 0.0, vz = 0.0, mass = solar_mass }
local jup = {
  x = 4.84143144246472090,
  y = -1.16032004402742839,
  z = -0.103622044471123109,
  vx = 0.00166007664274403694 * days_per_year,
  vy = 0.00769901118419740425 * days_per_year,
  vz = -0.0000690460016972063023 * days_per_year,
  mass = 0.000954791938424326609 * solar_mass,
}
local sat = {
  x = 8.34336671824457987,
  y = 4.12479856412430479,
  z = -0.403523417114321381,
  vx = -0.00276742510726862411 * days_per_year,
  vy = 0.00499852801234917238 * days_per_year,
  vz = 0.0000230417297573763929 * days_per_year,
  mass = 0.000285885980666130812 * solar_mass,
}
local ura = {
  x = 12.8943695621391310,
  y = -15.1111514016986312,
  z = -0.223307578892655734,
  vx = 0.00296460137564761618 * days_per_year,
  vy = 0.00237847173959480950 * days_per_year,
  vz = -0.0000296589568540237556 * days_per_year,
  mass = 0.0000436624404335156298 * solar_mass,
}
local nep = {
  x = 15.3796971148509165,
  y = -25.9193146099879641,
  z = 0.179258772950371181,
  vx = 0.00268067772490389322 * days_per_year,
  vy = 0.00162824170038242295 * days_per_year,
  vz = -0.0000951592254519715870 * days_per_year,
  mass = 0.0000515138902046611451 * solar_mass,
}

offset_momentum(sun, jup, sat, ura, nep)
local e0 = round9(energy(sun, jup, sat, ura, nep))
assert(e0 == -169075164)

for _ = 1, 1000 do
  advance(0.01, sun, jup, sat, ura, nep)
end

local e1 = round9(energy(sun, jup, sat, ura, nep))
assert(e1 == -169087605)
io.write("ok")

