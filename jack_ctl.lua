#! /bin/luajit

-- TODO allow to connect ports w/ having to specify l and r

local string = require "string"
local fmt = string.format

local aliases = {
  sysi1l = "system:capture_1" ,
  sysi1r = "system:capture_2" ,
  syso1l = "system:playback_1" ,
  syso1r = "system:playback_2" ,
  mpdo1l = "Music Player Daemon:left" ,
  mpdo1r = "Music Player Daemon:right" ,
}

for i=1, 8 do
  aliases[fmt("rnsi%dl", i)] = fmt("renoise:input_%02d_left", i)
  aliases[fmt("rnsi%dr", i)] = fmt("renoise:input_%02d_right", i)
end

for i=1, 2 do
  aliases[fmt("rnso%dl", i)] = fmt("renoise:output_%02d_left", i)
  aliases[fmt("rnso%dr", i)] = fmt("renoise:output_%02d_right", i)
end

aliases["mpvo1l"] = "mpv:out_0"
aliases["mpvo1r"] = "mpv:out_1"

for i=1, 2 do
  aliases[fmt("mpvo%dl", i + 1)] = fmt("mpv-%02d:out_0", i)
  aliases[fmt("mpvo%dr", i + 1)] = fmt("mpv-%02d:out_1", i)
end

local commands
commands = {
  list = "jack_lsp" ,
  start = "jack_control start" ,
  stop = "jack_control stop" ,

  connect = function (t)
    local one = aliases[t[1]] or t[1]
    local two = aliases[t[2]] or t[2]
    cmd = fmt("jack_connect %s %s", one, two)
    io.write(cmd, "\n")
    os.execute(cmd)
  end ,
  disconnect = function (t)
    local one = aliases[t[1]] or t[1]
    local two = aliases[t[2]] or t[2]
    local cmd = fmt("jack_disconnect %s %s", one, two)
    io.write(cmd, "\n")
    os.execute(cmd)
  end ,

  rns_resample = function ()
    commands.connect({ "rnso1l", "rnsi2l" })
    commands.connect({ "rnso1r", "rnsi2r" })
  end ,

  hello = function ()
    io.write "hey\n"
  end ,
  aliases = function ()
    for k, v in pairs(aliases) do
      io.write(fmt("%7s > %s\n", k, v))
    end
  end ,
  bye = function ()
    io.write(" ~ see u ~ \n")
    os.exit()
  end ,
}

local function alias_cmd(main, t)
  for i=1, #t do
    commands[t[i]] = commands[main]
  end
end

local cmd_aliases = {
  bye = { "quit", "q", "exit" } ,
  hello = { "hey", "hi" } ,
  connect = { "conn", "c" } ,
  disconnect = { "disconn", "dc" } ,
  list = { "l" } ,
}

for k, v in pairs(cmd_aliases) do
  alias_cmd(k, v)
end

local function eval(str)
  local todo = commands[str:match("(%S+)%s*")]
  if type(todo) == "string" then
    io.write(todo, "\n")
    os.execute(todo)
  elseif type(todo) == "function" then
    local args = {}
    local i = 0
    for it in str:gmatch("(%S+)%s*") do
      args[i] = it
      i = i + 1
    end
    todo(args)
  else
    io.write(fmt("invalid command: %s\n", str))
  end
end

if arg[1] then
  eval(table.concat(arg, " "))
  return
end

while true do
  io.write("♡ ")
  eval(io.read())
end
