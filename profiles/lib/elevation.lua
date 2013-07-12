local string = string
local table = table
local ipairs = ipairs
local math = math

module "Elevation"

local function reverse_dz(pl)
   local n=table.maxn(pl)
   if n<2 then
      return pl
   else
      local r={}
      r[1]={0,pl[n][2]}
      for i=2,n do
         r[i]={r[i-1][1]+(pl[n-i+1+1][1]-pl[n-i+1+0][1]), pl[n-i+1][2]}
      end
      -- compensate for float precision
      r[n][1]=pl[n][1]
      return r
   end
end

local function map(func, array)
      local new_array = {}
      for i,v in ipairs(array) do
         new_array[i] = func(v)
      end
      return new_array
end

-- simple speed (scaling) function depending on gradient
-- input: gradient
-- output: speed
local function gradient_speed(g)
   if g>0 then
      return math.max(3/15,1-100/15*g)
   else
      return math.min(50/15,1-50/15*g)
   end
end

local function last(t)
   return t[table.maxn(t)]
end

-- input: 2d polyline
-- output: distance/elevation deltas
local function dz(pl)
   local r={}
   for i=1,table.maxn(pl)-1 do
      r[i]={pl[i+1][1]-pl[i][1],pl[i+1][2]-pl[i][2]}
   end
   return r
end

-- input: 2d pl (d,z)
-- output: average speed (scale) and length
local function avg_speed_and_length_2d(pl2d, gs)
   local l=0+last(pl2d)[1]
   local dzs=dz(pl2d)
   local speeds=map(gs or gradient_speed, map(function(x) return x[2]/x[1] end, dzs))
   local time=0
   for i=1,table.maxn(speeds) do
      time=time+dzs[i][1]/speeds[i]
   end
   return l/time,l
end

-- todo: could be simplified if we stay with simple gradient speed
function speed_scales(elevation_profile, gradient_speedf)
   local speed_scale_fwd = avg_speed_and_length_2d(elevation_profile, gradient_speedf)
   local speed_scale_bwd = avg_speed_and_length_2d(reverse_dz(elevation_profile), gradient_speedf)
   -- print('fwd='..speed_scale_fwd..' bwd='..speed_scale_bwd)
   return speed_scale_fwd, speed_scale_bwd
end

function speed_scale(elevation_profile, gradient_speedf, forwardp)
   if forwardp then
      return avg_speed_and_length_2d(elevation_profile, gradient_speedf)
   else
      return avg_speed_and_length_2d(reverse_dz(elevation_profile), gradient_speedf)
   end
end

function parse_profile(s)
   if not s or s == '' then
      return false
   else
      -- print(s)
      local r={}
      for i in string.gmatch(s, "[^ ]+") do
         local p={}
         for j in string.gmatch(i, "[^%,]+") do
            table.insert(p,j)
         end
         table.insert(r,{p[4],p[3]})
      end
      return r
   end
end
