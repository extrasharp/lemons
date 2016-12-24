#!/bin/luajit

local string = require "string"
local fmt = string.format

last = 0

local function eval(str)
  str = str:gsub(",", "")
  local f, err = load(fmt("return (%s)", str))
  if f then
    local ans = f()

    if type(ans) == "number" then
      last = ans
    else
      ans = nil
    end

    io.write(fmt(ans and "%s\n" or "...\n", ans))
  else
    -- io.write(fmt("%s\n", err))
    io.write("error\n")
  end
end

if #arg > 0 then
  eval(table.concat(arg, " "))
  return
end

while true do
  local read
  while not read or read == "" do
    io.write("? ")
    read = io.read()
  end

  if read == "q" then break end

  eval(read)
end

io.write("bye\n")
