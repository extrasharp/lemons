#!/bin/luajit

local filepath = "/sys/class/backlight/amdgpu_bl0/brightness"
local min = 3
local max = 80

-- min and max must be 1 - 255

local function get_brightness()
  local f = io.open(filepath)
  if not f then return nil end
  local ret = tonumber(f:read("*l") or "")
  f:close()
  return ret
end

local function set_brightness(to)
  local f = io.open(filepath, "w")
  if not f then return end
  if to < min then to = min end
  if to > max then to = max end
  f:write(tostring(to))
  f:close()
  return to
end

if #arg == 0 then
  io.write(get_brightness(), "\n")
end

local num = tonumber(arg[1])
if not num then return end

local brightness
if string.find(arg[1], "[+-]") then
  brightness = (get_brightness() or 50) + num
else
  brightness = num
end

brightness = set_brightness(brightness)
io.write(brightness, "\n")
