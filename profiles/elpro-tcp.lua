#!/usr/bin/lua
function get_upsample_pl4d(host, port)
   local socket = require("socket")
   local client = assert(socket.tcp())
   assert(client:connect(socket.dns.toip(host), port))

   function readline()
      local str
      local err
      local part
      str, err, part = client:receive('*l')
      return str
   end

   function remote_eval(s)
      assert(client:send(s))
      return string.sub(readline(),string.len("gosh> ")+1)
   end

   function map(func, array)
      local new_array = {}
      for i,v in ipairs(array) do
	 new_array[i] = func(v)
      end
      return new_array
   end

   function parse_result(s)
      local r={}
      for i in string.gmatch(s, "%([^%)]+%)") do
	 local p={}
	 -- todo: crap
	 for j in string.gmatch(i, "[^%(%) ]+") do
	    table.insert(p,j)
	 end
	 table.insert(r,p)
      end
      return r
   end


   -- io.write("search for prompt")
   assert(client:send("'ready\n"))
   local line;
   while 1 do
      line=readline()
      if (string.find(line,"ready")) then break end
      -- io.write(".")
   end
   -- print("ok")

   -- todo
   function reverse(a)
      local r={}
      local n=table.maxn(a)
      for i=1,n do
	 r[n-i+1]=a[i]
      end
      return r
   end

   return function(pl,dist)
      return parse_result(remote_eval("(upsample-polyline->4d 'wgs84 '("..table.concat(map(function(x) return "("..table.concat(reverse(x)," ")..")" end, pl)," ")..") "..dist..")"))
   end
end
