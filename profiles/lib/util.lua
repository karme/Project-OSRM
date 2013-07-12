-- misc utils

function scale_way_speeds(way, fwd, bwd)
   local cspeed_fwd=way.forward.speed
   local cspeed_bwd=way.backward.speed
   if cspeed_fwd > 0 then
      way.forward.speed = cspeed_fwd * fwd
   end
   if cspeed_bwd > 0 then
      way.backward.speed = cspeed_bwd * bwd
   end
end

-- argh: no bitwise operators (bit32 in lua 5.2 only)
function set_lowest_bit(i,b)
   assert((b==0) or (b==1))
   -- no bitshift and no integer division in lua 5.1 :(
   return (math.floor(i/2)*2)+b
end

-- helpers to call profile from waysplit

function fakeway(id,tags)
   local fakeway = {id = id,
                    tags = {Find = function(dummy,x) return tags[x] or "" ; end},
                    forward = {speed = 0, mode = 0, cost = 0, realspeed = 0},
                    backward = {speed = 0, mode = 0, cost = 0, realspeed = 0}}
   setmetatable(fakeway, {__newindex = function(t,k,v) rawset(rawget(t,'forward'),k,v); rawset(rawget(t,'backward'),k,v); end})
   return fakeway
end

function way_info_dir(way,forwardp)
   local x = forwardp and 'forward' or 'backward'
   -- todo: cost
   return {speed = way[x].realspeed or 0, mode=way[x].mode, cost=way[x].speed}
end

function way_info(way)
   local x = forwardp and 'forward' or 'backward'
   way_function(way)
   return way_info_dir(way,true), way_info_dir(way,false)
end


if not parseMaxspeed then
   parseMaxspeed = function(x)
      -- todo:
      -- boost::algorithm::to_lower(input);
      -- int n = stringToInt(input);
      -- if (input.find("mph") != std::string::npos || input.find("mp/h") != std::string::npos) {
      --     n = (n*1609)/1000;
      -- }
      -- return n;
      return tonumber(x) or 0
   end
end

if not durationIsValid then
   -- todo:
   durationIsValid = function(x)
      return false
   end
end

if not parseDuration then
   -- todo:
   parseDuration = function(x)
      return 1;
   end
end

function Set(t)
   local s = {}
   for _,v in pairs(t) do s[v] = true end
   return s
end

function memberp(t, e)
   return t[e] and true or false
end

function way_is_part_of_route(way, forwardp, route_type_set)
   assert(type(route_type_set) == 'table');
   local i=0
   local rel_type
   while true do
      -- note: assumes all denormalized relations have a non-empty type tag
      -- at the moment the denormalization preprocessing filters for route types
      rel_type=way.tags:Find("rel["..i.."][type]")
      if rel_type == '' then break end
      if rel_type=='route' and memberp(route_type_set, way.tags:Find("rel["..i.."][route]")) then
         local role=way.tags:Find("rel["..i.."]:role")
         if role == '' or ((fowardp and role=='forward')) or ((not forwardp) and role=='backward') then
            return true
         end
      end
      i=i+1
   end
   return false
end

function way_is_part_of_cycle_route(way, forwardp)
   return way_is_part_of_route(way, forwardp, Set({"bicycle"}))
end

function way_is_mtbway(way, forwardp)
   -- todo:
   -- mtb:scale is downhill only?!
   -- there is also mtb:scale:uphill
   -- and http://wiki.openstreetmap.org/wiki/Key:incline
   -- we will have to consider direction here, too
   assert(way.tags:Find("mtb:scale"))
   return (way.tags:Find("mtb:scale") ~= "") or way_is_part_of_route(way,forwardp,Set({"mtb"}))
end

function way_is_cycleway(way, forwardp)
   local cycleway = way.tags:Find("cycleway")
   local cycleway_left = way.tags:Find("cycleway:left")
   local cycleway_right = way.tags:Find("cycleway:right")
   return (cycleway and cycleway_tags[cycleway]) or (forwardp and cycleway_right and cycleway_tags[cycleway_right]) or ((not forwardp) and cycleway_left and cycleway_tags[cycleway_left]) or way_is_part_of_cycle_route(way,forwardp)
end

-- simple speed (scaling) function depending on gradient
-- input: gradient
-- output: speed
function foot_gradient_speed(g)
   if g>0 then
      return math.max(1/4.2,1-3*g)
   else
      return math.max(1/4.2,1+1.4*g)
   end
end
