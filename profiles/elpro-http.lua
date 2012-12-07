require("curl")
require("json")

function get_upsample_pl4d(host, port)
   local clib = curl.easy_init()
   local url = 'http://'..host..':'..port..'/cgi-bin/elpro.fcgi?'
   function params(pl,dist)
      return 'path='..table.concat(map(function(x) return table.concat(x,",") end, pl),"|")..'&upsample='..dist..'&format=sjs'
   end

   -- input: 2d polyline
   -- output: upsampled 4d polyline with elevation and wgs84-distance from startpoint as 3rd and 4th dimension added
   return function(pl,dist)
      local header = {}
      local body = {}
      -- todo: maybe use http post
      -- print(url..params(pl,dist))
      clib:setopt(curl.OPT_URL,url..params(pl,dist))
      clib:setopt(curl.OPT_HEADERFUNCTION,function(s,len) table.insert(header,s) return len,nil end)
      clib:setopt(curl.OPT_WRITEFUNCTION,function(s,len) table.insert(body,s) return len,nil end)
      clib:perform()
      local r=json.decode(table.concat(body))
      assert(r and (r['status']=='OK')) -- nil is false in lua
      return r['results']
   end
end
