#!/usr/bin/lua
-- example of a elevation based speed function using elevation-profile demo service
require("curl")
require("json")
c = curl.easy_init() 

function map(func, array)
  local new_array = {}
  for i,v in ipairs(array) do
    new_array[i] = func(v)
  end
  return new_array
end

function params(pl,dist)
  return 'path='..table.concat(map(function(x) return table.concat(x,",") end, pl),"|")..'&upsample='..dist..'&format=sjs'
end

-- input: 2d polyline
-- output: upsampled 4d polyline with elevation and wgs84-distance from startpoint as 3rd and 4th dimension added
function upsample_pl4d(pl,dist)
   local header = {}
   local body = {}
   -- todo: maybe use http post
   print('http://karme.de/elevation-profile/demo?'..params(pl,dist))
   c:setopt(curl.OPT_URL,'http://karme.de/elevation-profile/demo?'..params(pl,dist))
   c:setopt(curl.OPT_HEADERFUNCTION,function(s,len) table.insert(header,s) return len,nil end)
   c:setopt(curl.OPT_WRITEFUNCTION,function(s,len) table.insert(body,s) return len,nil end)
   c:perform()
   local r=json.decode(table.concat(body))
   assert(r['status']=='OK')
   return r['results']
end

-- input: 4d polyline
-- output: distance/elevation deltas
function dz(pl)
   local r={}
   for i=1,table.maxn(pl)-1 do
      r[i]={pl[i+1][4]-pl[i][4],pl[i+1][3]-pl[i][3]}
   end
   return r
end

-- input: delta
-- output: gradient
function gradient(x)
   return x[2]/x[1]
end

-- simple speed function depending on gradient
-- input: gradient
-- output: speed
function speed(g)
   if g>0 then
      return math.max(3,15-100*g)
   else
      return math.min(50,15-50*g)
   end
end

function last(t)
   return t[table.maxn(t)]
end

-- input: pl
-- output: average speed
function avg_speed(pl)
   local pl4d=upsample_pl4d(pl,50)
   local l=last(pl4d)[4]
   --print(table.concat(map(table.concat,pl4d)))
   local dzs=dz(pl4d)
   --print(table.concat(map(table.concat,dzs)))
   local speeds=map(speed, map(function(x) return x[2]/x[1] end, dzs))
   --print(table.concat(speeds," "))
   local time=0
   for i=1,table.maxn(speeds) do
      time=time+dzs[i][1]/speeds[i]
   end
   return l/time
end

-- todo
function reverse(a)
   local r={}
   local n=table.maxn(a)
   for i=1,n do
      r[n-i+1]=a[i]
   end
   return r
end

function avg_speed_fwdbwd(pl)
   return {avg_speed(pl),avg_speed(reverse(pl))}
end

function test(from,to)
   print(table.concat(avg_speed_fwdbwd({from,to})," "))
end

test({48.5,9},{48.5,9.1})
test({0,0},{0,0.1})
test({48.52748,9.15578},{48.52408,9.14444})
