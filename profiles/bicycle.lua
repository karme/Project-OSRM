require("lib/access")
require("lib/maxspeed")
require("lib/elevation")

-- Begin of globals
barrier_whitelist = { [""] = true, ["cycle_barrier"] = true, ["bollard"] = true, ["entrance"] = true, ["cattle_grid"] = true, ["border_control"] = true, ["toll_booth"] = true, ["sally_port"] = true, ["gate"] = true, ["no"] = true}
access_tag_whitelist = { ["yes"] = true, ["permissive"] = true, ["designated"] = true	}
access_tag_blacklist = { ["no"] = true, ["private"] = true, ["agricultural"] = true, ["forestery"] = true }
access_tag_restricted = { ["destination"] = true, ["delivery"] = true }
access_tags_hierachy = { "bicycle", "vehicle", "access" }
cycleway_tags = {["track"]=true,["lane"]=true,["opposite"]=true,["opposite_lane"]=true,["opposite_track"]=true,["share_busway"]=true,["sharrow"]=true,["shared"]=true }
service_tag_restricted = { ["parking_aisle"] = true }
restriction_exception_tags = { "bicycle", "vehicle", "access" }

default_speed = 15

walking_speed = 6

bicycle_speeds = { 
	["cycleway"] = default_speed,
	["primary"] = default_speed,
	["primary_link"] = default_speed,
	["secondary"] = default_speed,
	["secondary_link"] = default_speed,
	["tertiary"] = default_speed,
	["tertiary_link"] = default_speed,
	["residential"] = default_speed,
	["unclassified"] = default_speed,
	["living_street"] = default_speed,
	["road"] = default_speed,
	["service"] = default_speed,
	["track"] = 12,
	["path"] = 12
	--["footway"] = 12,
	--["pedestrian"] = 12,
}

pedestrian_speeds = { 
	["footway"] = walking_speed,
	["pedestrian"] = walking_speed,
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
	["platform"] = walking_speed
}

amenity_speeds = { 
	["parking"] = 10,
	["parking_entrance"] = 10
}

man_made_speeds = { 
	["pier"] = walking_speed
}

route_speeds = { 
	["ferry"] = 5
}

surface_speeds = { 
	["cobblestone:flattened"] = 10,
	["paving_stones"] = 10,
	["compacted"] = 10,
	["cobblestone"] = 6,
	["unpaved"] = 6,
	["fine_gravel"] = 6,
	["gravel"] = 6,
	["fine_gravel"] = 6,
	["pebbelstone"] = 6,
	["ground"] = 6,
	["dirt"] = 6,
	["earth"] = 6,
	["grass"] = 6,
	["mud"] = 3,
	["sand"] = 3	
}

take_minimum_of_speeds 	= true
obey_oneway 			= true
obey_bollards 			= false
use_restrictions 		= true
ignore_areas 			= true -- future feature
traffic_signal_penalty 	= 5
u_turn_penalty 			= 20
use_turn_restrictions   = false
turn_penalty 			= 60
turn_bias               = 1.4
-- End of globals

--modes
mode_normal = 1
mode_pushing = 2
mode_ferry = 3
mode_train = 4

    
function get_exceptions(vector)
	for i,v in ipairs(restriction_exception_tags) do 
		vector:Add(v)
	end
end

function node_function (node)
	local barrier = node.tags:Find ("barrier")
	local access = Access.find_access_tag(node, access_tags_hierachy)
	local traffic_signal = node.tags:Find("highway")
	
	-- flag node if it carries a traffic light	
	if traffic_signal == "traffic_signals" then
		node.traffic_light = true
	end
	
	-- parse access and barrier tags
	if access and access ~= "" then
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
	
	return true
end

local function print_way(way)
    local r=''
    for k,v in pairs({'speed','backward_speed','direction','duration','name','ignore_in_grid','is_access_restricted','roundabout'}) do
       r=r..v..'='
       if type(way[v])=='boolean' then
          r=r..(way[v] and 'true' or 'false')
       elseif v=='direction' then
          r=r..((way[v]==Way.bidirectional and 'bidirectional') or (way[v]==Way.oneway and 'oneway') or (way[v]==Way.opposite and 'oneway reverse') or 'bug!')
       else
          r=r..way[v]
       end
       r=r..' '
    end
    print(r)
end

local function way_is_part_of_cycle_route(way, forwardp)
   local i=0
   local rel_type
   while true do
      -- note: assumes all denormalized relations have a non-empty type tag
      -- at the moment the denormalization preprocessing filters for route types
      rel_type=way.tags:Find("rel["..i.."][type]")
      if rel_type == '' then break end
      if rel_type=='route' and way.tags:Find("rel["..i.."][route]")=='bicycle' then
         local role=way.tags:Find("rel["..i.."]:role")
         if role == '' or ((fowardp and role=='forward')) or ((not forwardp) and role=='backward') then
            return true
         end
      end
      i=i+1
   end
   return false
end

local function way_is_cycleway(way, forwardp)
   local cycleway = way.tags:Find("cycleway")
   local cycleway_left = way.tags:Find("cycleway:left")
   local cycleway_right = way.tags:Find("cycleway:right")
   return (cycleway and cycleway_tags[cycleway]) or (forwardp and cycleway_right and cycleway_tags[cycleway_right]) or ((not forwardp) and cycleway_left and cycleway_tags[cycleway_left]) or way_is_part_of_cycle_route(way,forwardp)
end

local function scale_way_speeds(way, fwd, bwd)
   local cspeed_fwd=way.forward.speed
   local cspeed_bwd=way.backward.speed
   if cspeed_fwd > 0 then
      way.forward.speed = cspeed_fwd * fwd
   end
   if cspeed_bwd > 0 then
      way.backward.speed = cspeed_bwd * bwd
   end
end

function way_function (way)
	-- initial routability check, filters out buildings, boundaries, etc
	local highway = way.tags:Find("highway")
	local route = way.tags:Find("route")
	local man_made = way.tags:Find("man_made")
	local railway = way.tags:Find("railway")
	local amenity = way.tags:Find("amenity")
	local public_transport = way.tags:Find("public_transport")
    if (not highway or highway == '') and 
		(not route or route == '') and 
		(not railway or railway=='') and 
		(not amenity or amenity=='') and
		(not man_made or man_made=='') and
    	(not public_transport or public_transport=='')
    	then
    	return false
    end
    
    -- don't route on ways or railways that are still under construction
    if highway=='construction' or railway=='construction' then
        return false
    end
    
	-- access
 	local access = Access.find_access_tag(way, access_tags_hierachy)
    if access_tag_blacklist[access] then
		return false
    end


	-- other tags
	local name = way.tags:Find("name")
	local ref = way.tags:Find("ref")
	local junction = way.tags:Find("junction")
	local maxspeed = parseMaxspeed(way.tags:Find ( "maxspeed") )
	local maxspeed_forward = parseMaxspeed(way.tags:Find( "maxspeed:forward"))
	local maxspeed_backward = parseMaxspeed(way.tags:Find( "maxspeed:backward"))
	local barrier = way.tags:Find("barrier")
	local oneway = way.tags:Find("oneway")
	local onewayClass = way.tags:Find("oneway:bicycle")
	local cycleway = way.tags:Find("cycleway")
	local cycleway_left = way.tags:Find("cycleway:left")
	local cycleway_right = way.tags:Find("cycleway:right")
	local duration	= way.tags:Find("duration")
	local service	= way.tags:Find("service")
	local area = way.tags:Find("area")
	local foot = way.tags:Find("foot")
	local surface = way.tags:Find("surface")
	local foot_forward = way.tags:Find("foot:forward")
	local foot_backward = way.tags:Find("foot:backward")

	-- name	
	if "" ~= ref and "" ~= name then
		way.name = name .. ' / ' .. ref
    elseif "" ~= ref then
    	way.name = ref
	elseif "" ~= name then
		way.name = name
	else
		way.name = "{highway:"..highway.."}"	-- if no name exists, use way type
		                                        -- this encoding scheme is excepted to be a temporary solution
	end
		
	way.mode = mode_normal
	
	-- speed
    if route_speeds[route] then
		-- ferries (doesn't cover routes tagged using relations)
    	way.mode = mode_ferry
		way.ignore_in_grid = true
		if durationIsValid(duration) then
			way.duration = math.max( 1, parseDuration(duration) )
		else
		 	way.speed = route_speeds[route]
		end
	elseif platform_speeds[railway] then
		-- railway platforms (old tagging scheme)
		way.speed = platform_speeds[railway]
	elseif platform_speeds[public_transport] then
		-- public_transport platforms (new tagging platform)
		way.speed = platform_speeds[public_transport]
    elseif railway_speeds[railway] then
	 	-- railways
		if access and access_tag_whitelist[access] then
        	way.mode = mode_train
			way.speed = railway_speeds[railway]		
		end
	elseif amenity_speeds[amenity] then
		-- parking areas
		way.speed = amenity_speeds[amenity]
	elseif bicycle_speeds[highway] then
		-- regular ways
      	way.speed = bicycle_speeds[highway]
	elseif access_tag_whitelist[access] then
	    -- unknown way, but valid access tag
		way.speed = default_speed
	else
	    -- biking not allowed, maybe we can push our bike?
	    -- essentially requires pedestrian profiling, for example foot=no mean we can't push a bike
        if foot ~= 'no' then
	        if pedestrian_speeds[highway] then
	            -- pedestrian-only ways and areas
        		way.speed = pedestrian_speeds[highway]
            	way.mode = mode_pushing
        	elseif man_made and man_made_speeds[man_made] then
            	-- man made structures
            	way.speed = man_made_speeds[man_made]
            	way.mode = mode_pushing
            elseif foot == 'yes' then
                way.speed = walking_speed
            	way.mode = mode_pushing
            elseif foot_forward == 'yes' then
                way.forward.speed = walking_speed
            	way.forward.mode = mode_pushing
            	way.backward.mode = 0
            elseif foot_backward == 'yes' then
                way.backward.speed = walking_speed
            	way.backward.mode = mode_pushing
            	way.forward.mode = 0
            end
        end
    end
		
	-- direction
	local impliedOneway = false
	if junction == "roundabout" or highway == "motorway_link" or highway == "motorway" then
		impliedOneway = true
	end
	
	if onewayClass == "yes" or onewayClass == "1" or onewayClass == "true" then
    	way.backward.mode = 0
	elseif onewayClass == "no" or onewayClass == "0" or onewayClass == "false" then
	    -- prevent implied oneway
	elseif onewayClass == "-1" then
    	way.forward.mode = 0
	elseif oneway == "no" or oneway == "0" or oneway == "false" then
	    -- prevent implied oneway
	elseif string.find(cycleway, "opposite") == 1 then
		if impliedOneway then
        	way.forward.mode = 0
        	way.backward.mode = mode_normal
		end
	elseif cycleway_tags[cycleway_left] and cycleway_tags[cycleway_right] then
	    -- prevent implied
	elseif cycleway_tags[cycleway_left] then
		if impliedOneway then
        	way.forward.mode = 0
        	way.backward.mode = mode_normal
		end
	elseif cycleway_tags[cycleway_right] then
		if impliedOneway then
        	way.forward.mode = mode_normal
        	way.backward.mode = 0
		end
	elseif oneway == "-1" then
		way.forward.mode = 0
	elseif oneway == "yes" or oneway == "1" or oneway == "true" or impliedOneway then
	    way.backward.mode = 0
    end
	
    
	-- pushing bikes
	if bicycle_speeds[highway] or pedestrian_speeds[highway] then
	    if foot ~= "no" then
	        if junction ~= "roundabout" then
            	if way.backward.mode == 0 then
            	    way.backward.speed = walking_speed
                	way.backward.mode = mode_pushing
                elseif way.forward.mode == 0 then
                    way.forward.speed = walking_speed
                	way.forward.mode = mode_pushing
            	end
            end
        end
    end
	
	-- cycleway speed
    if way_is_cycleway(way, true) then
		way.forward.speed = bicycle_speeds["cycleway"]
    end
    if way_is_cycleway(way, false) then
		way.backward.speed = bicycle_speeds["cycleway"]
	end

    -- surfaces
    if surface_speeds[surface] then
        way.forward.speed = math.min(way.forward.speed, surface_speeds[surface])
        way.backward.speed  = math.min(way.backward.speed, surface_speeds[surface])
    end

    -- elevation
    local elevation_profile = Elevation.parse_profile(way.tags:Find("profile"))
    if elevation_profile then
       local speed_scale_fwd, speed_scale_bwd = Elevation.speed_scales(elevation_profile)
       scale_way_speeds(way, speed_scale_fwd, speed_scale_bwd)
    end

	-- maxspeed
    MaxSpeed.limit( way, maxspeed, maxspeed_forward, maxspeed_backward )

    -- prefer cycle ways
    -- note: we adjust the cost function here / not the time/speed function
    if way_is_cycleway(way) then
       scale_way_speeds(way, 2, 2)
    end
    	
	return true
end

function turn_function (angle)
    -- compute turn penalty as angle^2, with a left/right bias
    k = turn_penalty/(90.0*90.0)
	if angle>=0 then
	    return angle*angle*k/turn_bias
	else
	    return angle*angle*k*turn_bias
    end
end
