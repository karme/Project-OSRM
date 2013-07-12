-- Foot profile
-- todo: maybe better call it hiking profile?!
require("lib/access")
require("lib/elevation")
require("lib/util")

-- Begin of globals

bollards_whitelist = { [""] = true, ["cattle_grid"] = true, ["border_control"] = true, ["toll_booth"] = true, ["sally_port"] = true, ["gate"] = true}
access_tag_whitelist = { ["yes"] = true, ["foot"] = true, ["permissive"] = true, ["designated"] = true  }
access_tag_blacklist = { ["no"] = true, ["private"] = true }
access_tag_restricted = { ["destination"] = true, ["delivery"] = true }
access_tags_hierachy = { "foot", "access" }
service_tag_restricted = { ["parking_aisle"] = true }
ignore_in_grid = { ["ferry"] = true }
restriction_exception_tags = { "foot" }

default_speed = 4.2

speed_profile = { 
    ["primary"] = default_speed,
    ["primary_link"] = default_speed,
    ["secondary"] = default_speed,
    ["secondary_link"] = default_speed,
    ["tertiary"] = default_speed,
    ["tertiary_link"] = default_speed,
    ["unclassified"] = default_speed,
    ["residential"] = default_speed,
    ["road"] = default_speed,
    ["living_street"] = default_speed,
    ["service"] = default_speed,
    ["track"] = default_speed,
    ["path"] = default_speed,
    ["steps"] = default_speed,
    ["ferry"] = default_speed,
    ["pedestrian"] = default_speed,
    ["footway"] = default_speed,
    ["pier"] = default_speed,
    ["default"] = default_speed
}

waytype_penalties = {
   ["R"] = 10,
   ["A"] = 5,
   ["S"] = 3,
   ["W"] = 2,
   ["P"] = 2,
   ["G"] = 10
}

take_minimum_of_speeds 	= true
obey_oneway 			= true
obey_bollards 			= false
use_restrictions 		= false
ignore_areas 			= false -- future feature
traffic_signal_penalty 	= 2
u_turn_penalty 			= 2
use_turn_restrictions   = false
-- End of globals

function get_exceptions(vector)
    for i,v in ipairs(restriction_exception_tags) do 
        vector:Add(v)
    end
end

function node_function (node)
    local barrier = node.tags:Find ("barrier")
    local access = node.tags:Find ("access")
    local traffic_signal = node.tags:Find("highway")

    --flag node if it carries a traffic light

    if traffic_signal == "traffic_signals" then
        node.traffic_light = true;
    end

    if obey_bollards then
        --flag node as unpassable if it black listed as unpassable
        if access_tag_blacklist[barrier] then
            node.bollard = true;
        end

        --reverse the previous flag if there is an access tag specifying entrance
        if node.bollard and not bollards_whitelist[barrier] and not access_tag_whitelist[barrier] then
            node.bollard = false;
        end
    end
    return true
end

local function way_is_part_of_foot_route(way, forwardp)
   return way_is_part_of_route(way, forwardp, Set({"foot","hiking"}))
end

local function way_is_footway(way, forwardp)
   -- see also:
   -- http://wiki.openstreetmap.org/wiki/Key:sac_scale
   return way_is_part_of_foot_route(way, forwardp) or (way.tags:Find("sac_scale") ~= '')
end

function foot_way_penalty(way, elevation_profile, forwardp)
   if way_is_footway(way, forwardp) then
      return 1
   end
   return waytype_penalties[way.tags:Find("alpstein:waytype")] or 2
end

function way_function (way)

    -- First, get the properties of each way that we come across
    local highway = way.tags:Find("highway")
    local name = way.tags:Find("name")
    local ref = way.tags:Find("ref")
    local junction = way.tags:Find("junction")
    local route = way.tags:Find("route")
    local maxspeed = parseMaxspeed(way.tags:Find ( "maxspeed") )
    local man_made = way.tags:Find("man_made")
    local barrier = way.tags:Find("barrier")
    local oneway = way.tags:Find("oneway")
    local onewayClass = way.tags:Find("oneway:foot")
    local duration  = way.tags:Find("duration")
    local service  = way.tags:Find("service")
    local area = way.tags:Find("area")
    local access = way.tags:Find("access")

    -- Second parse the way according to these properties

    if ignore_areas and "yes"==area then
        return false
    end

	-- access
 	local access = Access.find_access_tag(way, access_tags_hierachy)
    if access_tag_blacklist[access] then
		return false
    end

    way.name = ''..way.id
    way.forward.mode = 1*2
    way.backward.mode = 1*2

    if "roundabout" == junction then
        way.roundabout = true;
    end

    -- Handling ferries and piers
    if false then
        -- (speed_profile[route] and speed_profile[route]>0) or (speed_profile[man_made] and speed_profile[man_made]>0) then
        way.forward.mode = 2*2
        way.backward.mode = 2*2
        if durationIsValid(duration) then
            way.duration = math.max( 1, parseDuration(duration) )
        else
            way.forward.speed = speed_profile[highway]
            way.backward.speed = speed_profile[highway]
        end
    else
        if speed_profile[route] then
            highway = route
        elseif speed_profile[man_made] then
            highway = man_made
        end
        if speed_profile[highway] then 
            way.forward.mode = 1*2
            way.backward.mode = 1*2
            way.forward.speed = speed_profile[highway]
            way.backward.speed = speed_profile[highway]
        -- ugly specialcase for cycleway
        elseif highway == "cycleway" and access_tag_whitelist[access] then
            way.forward.mode = 1*2
            way.backward.mode = 1*2
            way.forward.speed = default_speed
            way.backward.speed = default_speed
        end
        -- ignore oneway, but respect oneway:foot
        if onewayClass=="yes" or onewayClass=="1" or onewayClass=="true" then
            way.backward.mode = 0
        elseif onewayClass=="-1" then
            way.forward.mode = 0
        end
        
        -- restricted areas
        if access_tag_restricted[access] then
            way.is_access_restricted = true
        end
        if service_tag_restricted[service] then
            way.is_access_restricted = true
        end
    end

    -- elevation
    local elevation_profile = Elevation.parse_profile(way.tags:Find("geometry"))
    if elevation_profile then
       local speed_scale_fwd, speed_scale_bwd = Elevation.speed_scales(elevation_profile, foot_gradient_speed)
       scale_way_speeds(way, speed_scale_fwd, speed_scale_bwd)
    end

    -- todo: maxspeed?

    -- prefer walk ways
    -- todo: we want to adjust the cost function here / not the time/speed function
    -- for now use a hack (note: this only works with the fake way!)
    way.forward.realspeed = way.forward.speed
    way.backward.realspeed = way.backward.speed

    -- make cost only depend on length
    if way.forward.speed > 0 then way.forward.speed = default_speed end
    if way.backward.speed > 0 then way.backward.speed = default_speed end

    scale_way_speeds(way,
                     1/foot_way_penalty(way, elevation_profile, true),
                     1/foot_way_penalty(way, elevation_profile, false))

    -- adjust mode for direction
    if way.forward.mode > 0 then
       way.forward.mode = set_lowest_bit(way.forward.mode, 1)
    end
    if way.backward.mode > 0 then
       way.backward.mode = set_lowest_bit(way.backward.mode, 0)
       assert (way.backward.mode > 0)
    end
    return true
end
