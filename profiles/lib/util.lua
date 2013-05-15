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
