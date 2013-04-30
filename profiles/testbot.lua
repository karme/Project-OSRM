-- Testbot profile

-- Moves at fixed, well-known speeds, practical for testing speed and travel times:

-- Primary road:	36km/h = 36000m/3600s = 100m/10s
-- Secondary road:	18km/h = 18000m/3600s = 100m/20s
-- Tertiary road:	12km/h = 12000m/3600s = 100m/30s

-- modes:
-- 1: normal
-- 2: route
-- 3: river downstream
-- 4: river upstream
-- 5: steps down
-- 6: steps up

speed_profile = { 
	["primary"] = 36,
	["secondary"] = 18,
	["tertiary"] = 12,
	["steps"] = 6,
	["default"] = 24
}

-- these settings are read directly by osrm

take_minimum_of_speeds 	= true
obey_oneway 			= true
obey_bollards 			= true
use_restrictions 		= true
ignore_areas 			= true	-- future feature
traffic_signal_penalty 	= 7		-- seconds
u_turn_penalty 			= 20

modes = { "bot", "ferry", "downstream", "upstream" }

function get_modes(vector)
	for i,v in ipairs(modes) do 
		vector:Add(v)
	end
end

function limit_speed(speed, limits)
    -- don't use ipairs(), since it stops at the first nil value
    for i=1, #limits do
        limit = limits[i]
        if limit ~= nil and limit > 0 then
            if limit < speed then
                return limit        -- stop at first speedlimit that's smaller than speed
            end
        end
    end
    return speed
end

function node_function (node)
	local traffic_signal = node.tags:Find("highway")

	if traffic_signal == "traffic_signals" then
		node.traffic_light = true;
		-- TODO: a way to set the penalty value
	end
	return 1
end

function way_function (way)
	local highway = way.tags:Find("highway")
	local name = way.tags:Find("name")
	local oneway = way.tags:Find("oneway")
	local route = way.tags:Find("route")
	local duration = way.tags:Find("duration")
    local maxspeed = tonumber(way.tags:Find ( "maxspeed"))
    local maxspeed_forward = tonumber(way.tags:Find( "maxspeed:forward"))
    local maxspeed_backward = tonumber(way.tags:Find( "maxspeed:backward"))
	
	way.name = ''..way.id

	if oneway == "yes" or oneway == "1" or oneway == "true" then
       -- print("found oneway id: "..way.id)
       way.forward.speed = 1.0
       way.backward.speed = 0.0
	elseif oneway == "-1" then
       way.forward.speed = 0.0
       way.backward.speed = 1.0
    else
       way.forward.speed = 1.0
       way.backward.speed = 1.0
    end

    if way.forward.speed > 0 then
       way.forward.mode = 1
    else
       way.forward.mode = 0
    end
    if way.backward.speed > 0 then
       way.backward.mode = 2
    else
       way.backward.mode = 0
    end
	return 1
end
