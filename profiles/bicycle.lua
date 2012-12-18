-- Begin of globals

if false then
   require("profiles/elpro-http")
   upsample_pl4d = get_upsample_pl4d("127.0.0.1",80)
else
   require("profiles/elpro-tcp")
   upsample_pl4d = get_upsample_pl4d("127.0.0.1",10000)
end

barrier_whitelist = { [""] = true, ["bollard"] = true, ["entrance"] = true, ["cattle_grid"] = true, ["border_control"] = true, ["toll_booth"] = true, ["sally_port"] = true, ["gate"] = true}
access_tag_whitelist = { ["yes"] = true, ["permissive"] = true, ["designated"] = true	}
access_tag_blacklist = { ["no"] = true, ["private"] = true, ["agricultural"] = true, ["forestery"] = true }
access_tag_restricted = { ["destination"] = true, ["delivery"] = true }
access_tags_hierachy = { "bicycle", "vehicle", "access" }
cycleway_tags = {["track"]=true,["lane"]=true,["opposite"]=true,["opposite_lane"]=true,["opposite_track"]=true,["share_busway"]=true,["sharrow"]=true,["shared"]=true }
service_tag_restricted = { ["parking_aisle"] = true }

default_speed = 16

main_speeds = { 
	["cycleway"] = 18,
	["primary"] = 17,
	["primary_link"] = 17,
	["secondary"] = 18,
	["secondary_link"] = 18,
	["tertiary"] = 18,
	["tertiary_link"] = 18,
	["residential"] = 18,
	["unclassified"] = 16,
	["living_street"] = 16,
	["road"] = 16,
	["service"] = 16,
	["track"] = 13,
	["path"] = 13,
	["footway"] = 12,
	["pedestrian"] = 12,
	["pier"] = 12,
	["steps"] = 2
}

pedestrian_speeds = { 
	["footway"] = 5,
	["pedestrian"] = 5,
	["pier"] = 5,
	["steps"] = 2
}

railway_speeds = { 
	["train"] = 10,
	["railway"] = 10,
	["subway"] = 10,
	["light_rail"] = 10,
	["monorail"] = 10,
	["tram"] = 10
}

platform_speeds = { 
	["platform"] = 5
}

amenity_speeds = { 
	["parking"] = 10,
	["parking_entrance"] = 10
}

route_speeds = { 
	["ferry"] = 5
}

take_minimum_of_speeds 	= true
obey_oneway 			= true
obey_bollards 			= false
use_restrictions 		= true
ignore_areas 			= true -- future feature
traffic_signal_penalty 	= 2
u_turn_penalty 			= 20

-- End of globals

--find first tag in access hierachy which is set
function find_access_tag(source)
	for i,v in ipairs(access_tags_hierachy) do 
		local tag = source.tags:Find(v)
		if tag ~= '' then --and tag ~= "" then
			return tag
		end
	end
	return nil
end

function node_function (node)
	local barrier = node.tags:Find ("barrier")
	local access = find_access_tag(node)
	local traffic_signal = node.tags:Find("highway")
	
	-- flag node if it carries a traffic light	
	if traffic_signal == "traffic_signals" then
		node.traffic_light = true
	end
	
	-- parse access and barrier tags
	if access  and access ~= "" then
		if access_tag_blacklist[access] then
			node.bollard = true
		else
			node.bollard = false
		end
	elseif barrier and barrier ~= "" then
		if barrier_whitelist[barrier] then
			node.bollard = false
		else
			node.bollard = true
		end
	end
	
	return 1
end

function way_function (way, numberOfNodesInWay)
	-- A way must have two nodes or more
	if(numberOfNodesInWay < 2) then
		return 0;
	end
	
	-- First, get the properties of each way that we come across
	local highway = way.tags:Find("highway")
	local name = way.tags:Find("name")
	local ref = way.tags:Find("ref")
	local junction = way.tags:Find("junction")
	local route = way.tags:Find("route")
	local railway = way.tags:Find("railway")
	local maxspeed = parseMaxspeed(way.tags:Find ( "maxspeed") )
	local man_made = way.tags:Find("man_made")
	local barrier = way.tags:Find("barrier")
	local oneway = way.tags:Find("oneway")
	local onewayClass = way.tags:Find("oneway:bicycle")
	local cycleway = way.tags:Find("cycleway")
	local cycleway_left = way.tags:Find("cycleway:left")
	local cycleway_right = way.tags:Find("cycleway:right")
	local duration	= way.tags:Find("duration")
	local service	= way.tags:Find("service")
	local area = way.tags:Find("area")
	local amenity = way.tags:Find("amenity")
	local access = find_access_tag(way)
	
	-- only route on things with highway tag set (not buildings, boundaries, etc)
    if (not highway or highway == '') and 
		(not route or route == '') and 
		(not railway or railway=='') and 
		(not amenity or amenity=='') then
		return 0
    end
		
 	-- access
    if access_tag_blacklist[access] then
		return 0
    end

	-- name	
	if "" ~= ref then
		way.name = ref
	elseif "" ~= name then
		way.name = name
	else
		way.name = highway		-- if no name exists, use way type
	end
	
    if route_speeds[route] then
		-- ferries
		way.direction = Way.bidirectional
		way.ignore_in_grid = true
		if durationIsValid(duration) then
			way.speed = math.max( parseDuration(duration) / math.max(1, numberOfNodesInWay-1) )
		 	way.is_duration_set = true
		else
		 	way.speed = route_speeds[route]
		end
	elseif railway and platform_speeds[railway] then
		-- railway platforms
		way.speed = platform_speeds[railway]
    elseif railway and railway_speeds[railway] then
	 	-- railways
		if access and access_tag_whitelist[access] then
			way.speed = railway_speeds[railway]		
			way.direction = Way.bidirectional
		end
	elseif pedestrian_speeds[highway] and main_speeds[highway] then
		-- pedestrian areas
		if access_tag_whitelist[access] then
			way.speed = main_speeds[highway]		-- biking 
		else
			way.speed = pedestrian_speeds[highway]	-- pushing bikes
		end
	elseif amenity and amenity_speeds[amenity] then
		-- parking areas
		way.speed = amenity_speeds[amenity]
	else
		-- regular ways
		if main_speeds[highway] then 
	      	way.speed = main_speeds[highway]
	    elseif main_speeds[man_made] then 
			way.speed = main_speeds[man_made]
		elseif access_tag_whitelist[access] then
			way.speed = default_speed
		end
	end
	
	-- maxspeed
	if take_minimum_of_speeds then
		if maxspeed and maxspeed>0 then
			way.speed = math.min(way.speed, maxspeed)
		end
	end
	
	-- direction
	way.direction = Way.bidirectional
	local impliedOneway = false
	if junction == "roundabout" or highway == "motorway_link" or highway == "motorway" then
		way.direction = Way.oneway
		impliedOneway = true
	end
	
	if onewayClass == "yes" or onewayClass == "1" or onewayClass == "true" then
		way.direction = Way.oneway
	elseif onewayClass == "no" or onewayClass == "0" or onewayClass == "false" then
		way.direction = Way.bidirectional
	elseif onewayClass == "-1" then
		way.direction = Way.opposite
	elseif oneway == "no" or oneway == "0" or oneway == "false" then
		way.direction = Way.bidirectional
	elseif cycleway and string.find(cycleway, "opposite") == 1 then
		if impliedOneway then
			way.direction = Way.opposite
		else
			way.direction = Way.bidirectional
		end
	elseif cycleway_left and cycleway_tags[cycleway_left] and cycleway_right and cycleway_tags[cycleway_right] then
		way.direction = Way.bidirectional
	elseif cycleway_left and cycleway_tags[cycleway_left] then
		if impliedOneway then
			way.direction = Way.opposite
		else
			way.direction = Way.bidirectional
		end
	elseif cycleway_right and cycleway_tags[cycleway_right] then
		if impliedOneway then
			way.direction = Way.oneway
		else
			way.direction = Way.bidirectional
		end
	elseif oneway == "-1" then
		way.direction = Way.opposite
	elseif oneway == "yes" or oneway == "1" or oneway == "true" then
		way.direction = Way.oneway
	end
	
	-- cycleways
	if cycleway and cycleway_tags[cycleway] then
		way.speed = main_speeds["cycleway"]
	elseif cycleway_left and cycleway_tags[cycleway_left] then
		way.speed = main_speeds["cycleway"]
	elseif cycleway_right and cycleway_tags[cycleway_right] then
		way.speed = main_speeds["cycleway"]
	end

	way.type = 1
	return 1
end

function map(func, array)
  local new_array = {}
  for i,v in ipairs(array) do
    new_array[i] = func(v)
  end
  return new_array
end

function last(t)
   return t[table.maxn(t)]
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

-- input: 4d pl
-- output: average speed
function avg_speed_and_length_4d(pl4d)
   local l=0+last(pl4d)[4]
   local dzs=dz(pl4d)
   local speeds=map(speed, map(function(x) return x[2]/x[1] end, dzs))
   local time=0
   for i=1,table.maxn(speeds) do
      time=time+dzs[i][1]/speeds[i]
   end
   return l/time,l
end

-- input: 4d pl (distance in last dimension)
-- output: reversed 4d polyline
function reverse_pl4d(pl)
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

-- calculate segment weight
-- todo: way maxspeed must be respected downhill, too
-- notes:
-- could be simplified if we stay with the simple gradient based speed function
function segment_function(lat1, lon1, lat2, lon2, speed)
   local plfwd=upsample_pl4d({{lat1/1e5,lon1/1e5},{lat2/1e5,lon2/1e5}},50)
   local plbwd=reverse_pl4d(plfwd)
   local asfwd,lengthfwd=avg_speed_and_length_4d(plfwd)
   local asbwd,lengthbwd=avg_speed_and_length_4d(plbwd)
   assert(lengthfwd==lengthbwd)
   local length=lengthfwd
   return {length*10/((speed*asfwd/15)/3.6), length*10/((speed*asbwd/15)/3.6), length}
end
