#!/bin/luajit

local ffi = require "ffi"
ffi.cdef [[
int poll(void *, int, int);
]]

local function sleep(ms)
  ffi.C.poll(nil, 0, ms)
end

local string = require "string"
local fmt = string.format

local function usage()
  print(fmt("usage: %s [seconds]", arg[0]))
  print(fmt("       %s [minutes]m", arg[0]))
  print(fmt("       %s [hours]h", arg[0]))
end

local function parse_time_str(str)
  local _, _, time, mult = string.find(arg[1], "(%d*)(%a*)")
  return time, mult
end

if #arg ~= 1 then
  usage()
  return
end

local time, mult = parse_time_str(arg[1])

if not time then
  usage()
  return
end

if mult == "" then
  mult = "s"
end

local times = {
  s = 1 ,
  m = 60 ,
  h = 3600 ,
}

if not times[mult] then
  usage()
  return
end

local secs = tonumber(time) * times[mult]
local ms = secs * 1000
local jump = 500

io.write(fmt("%d seconds\n", secs))

for i = 0, ms, jump do
  sleep(jump)
  -- io.write(i, "\n")
  io.write(string.rep(" ", 15), "\r")
  io.write(fmt("%.1f", i / 1000))
  io.flush()
end

io.write("\n")
