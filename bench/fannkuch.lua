local function fannkuch(n)
  local perm = {}
  local perm1 = {}
  local count = {}
  local maxflips = 0
  local m = n - 1

  for i = 0, n - 1 do
    perm1[i] = i
  end

  local r = n
  while true do
    while r ~= 1 do
      count[r - 1] = r
      r = r - 1
    end

    if perm1[0] ~= 0 and perm1[m] ~= m then
      for i = 0, n - 1 do
        perm[i] = perm1[i]
      end

      local flips = 0
      local k = perm[0]
      while k ~= 0 do
        local i = 1
        local j = k - 1
        while i < j do
          perm[i], perm[j] = perm[j], perm[i]
          i = i + 1
          j = j - 1
        end
        flips = flips + 1
        local j2 = perm[k]
        perm[k] = k
        k = j2
      end

      if flips > maxflips then
        maxflips = flips
      end
    end

    while true do
      if r == n then
        return maxflips
      end
      local perm0 = perm1[0]
      for i = 0, r - 1 do
        perm1[i] = perm1[i + 1]
      end
      perm1[r] = perm0
      count[r] = (count[r] or 0) - 1
      if count[r] > 0 then
        break
      end
      r = r + 1
    end
  end
end

local r = fannkuch(7)
assert(r == 16)
io.write("ok")

