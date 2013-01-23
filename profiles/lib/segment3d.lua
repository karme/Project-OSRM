-- segment function using elevation to adjust weight

-- Begin of globals

-- elpro modules must be in lua path
-- you might want to use something like:
-- LUA_PATH='./profiles/?.lua;;' ./osrm-extract ...
-- if it isn't
if false then
   require("lib/elpro-http")
   upsample_pl4d = get_upsample_pl4d("127.0.0.1",80)
else
   require("lib/elpro-tcp")
   upsample_pl4d = get_upsample_pl4d("127.0.0.1",2223)
end

-- calculate segment weight
-- notes:
-- could be simplified if we stay with the simple gradient based speed function
function segment_function(lat1, lon1, lat2, lon2, speed, maxspeed)
   local function map(func, array)
      local new_array = {}
      for i,v in ipairs(array) do
	 new_array[i] = func(v)
      end
      return new_array
   end

   local function last(t)
      return t[table.maxn(t)]
   end

   -- input: 4d polyline
   -- output: distance/elevation deltas
   local function dz(pl)
      local r={}
      for i=1,table.maxn(pl)-1 do
	 r[i]={pl[i+1][4]-pl[i][4],pl[i+1][3]-pl[i][3]}
      end
      return r
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

   -- input: 4d pl
   -- output: average speed (scale) and length
   local function avg_speed_and_length_4d(pl4d)
      local l=0+last(pl4d)[4]
      local dzs=dz(pl4d)
      local speeds=map(gradient_speed, map(function(x) return x[2]/x[1] end, dzs))
      local time=0
      for i=1,table.maxn(speeds) do
	 time=time+dzs[i][1]/speeds[i]
      end
      return l/time,l
   end

   -- input: 4d pl (distance in last dimension)
   -- output: reversed 4d polyline
   local function reverse_pl4d(pl)
      -- todo: there must be something like this?!
      local function copy(a)
	 local n=table.maxn(a)
	 local r={}
	 for i=1,n do
	    r[i]=a[i]
	 end
	 return r
      end

      local n=table.maxn(pl)
      if n<2 then
	 return pl
      else
	 local r={}
	 r[1]=copy(pl[n])
	 r[1][4]=0
	 for i=2,n do
	    r[i]=copy(pl[n-i+1])
	    r[i][4]=r[i-1][4]+(pl[n-i+1+1][4]-pl[n-i+1+0][4])
	 end
	 -- compensate for float precision
	 r[n][4]=pl[n][4]
	 return r
      end
   end


   local plfwd=upsample_pl4d({{lon1/1e5,lat1/1e5},{lon2/1e5,lat2/1e5}},50)
   local plbwd=reverse_pl4d(plfwd)
   local asfwd,lengthfwd=avg_speed_and_length_4d(plfwd)
   local asbwd,lengthbwd=avg_speed_and_length_4d(plbwd)
   assert(lengthfwd==lengthbwd)
   local length=lengthfwd
   local weightfwd
   local weightbwd
   if maxspeed>0 then
      weightfwd=length*10/(math.min(speed*asfwd,maxspeed)/3.6)
      weightbwd=length*10/(math.min(speed*asbwd,maxspeed)/3.6)
   else
      weightfwd=length*10/((speed*asfwd)/3.6)
      weightbwd=length*10/((speed*asbwd)/3.6)
   end
   -- todo:
   -- if forward and backward weights are nearly identical we might want
   -- to use the same weight for both directions to create less directed edges
   -- (suggested by prozessor13)
   -- note:
   -- weights are converted to ints in the native code and only if those differ
   -- directed edges are used (=> imho good enough for now)
   -- if nearly_same(weightfwd,weightbwd) then
   --    weightfwd=avg(weightfwd,weightbwd)
   --    weightbwd=weightfwd
   -- end
   return {weightfwd,weightbwd,length}
end
