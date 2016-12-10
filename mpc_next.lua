#!/usr/bin/luajit

if    arg[1] ~= "next"
  and arg[1] ~= "prev"
  and arg[1] ~= "toggle" then
    return 
end

local f = io.popen("mpc --format ''")
if not f then return end
local t = f:read("*a") or ""
f:close()

local playing
if t:find("playing") then
  playing = true
else
  playing = false
end

if arg[1] == "next" then
  os.execute("mpc -q next")
  if not playing then
    os.execute("mpc -q pause")
  end
elseif arg[1] == "prev" then
  local _, _, sec = t:find("%:(%d+)%/")
  if not sec then return end

  sec = tonumber(sec)

  if playing and sec > 6 then
      os.execute("mpc -q seek 0")
  else
      os.execute("mpc -q prev")
  end

  if not playing then
      os.execute("mpc -q pause")
  end
elseif arg[1] == "toggle" then
  if playing then
      os.execute("mpc -q pause")
  else
      os.execute("mpc -q play")
  end
end
