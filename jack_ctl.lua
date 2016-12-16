#! /bin/luajit

-- TODO allow to connect ports w/ having to specify l and r

local string = require "string"
local fmt = string.format

local aliases = {
  syso1l = "system:capture_1" ,
  syso1r = "system:capture_2" ,
  sysi1l = "system:playback_1" ,
  sysi1r = "system:playback_2" ,
  mpdo1l = "'Music Player Daemon:left'" ,
  mpdo1r = "'Music Player Daemon:right'" ,
}

for i=1, 8 do
  aliases[fmt("rnsi%dl", i)] = fmt("renoise:input_%02d_left", i)
  aliases[fmt("rnsi%dr", i)] = fmt("renoise:input_%02d_right", i)
end

for i=1, 2 do
  aliases[fmt("rnso%dl", i)] = fmt("renoise:output_%02d_left", i)
  aliases[fmt("rnso%dr", i)] = fmt("renoise:output_%02d_right", i)
end

for i=0,2 do
  aliases[fmt("ffxo%dl", i + 1)] = fmt("Firefox:AudioStream_%d_out_0", i)
  aliases[fmt("ffxo%dr", i + 1)] = fmt("Firefox:AudioStream_%d_out_1", i)
end

aliases["mpvo1l"] = "mpv:out_0"
aliases["mpvo1r"] = "mpv:out_1"
aliases["csdo1l"] = "csound6:output1"
aliases["csdo1r"] = "csound6:output2"

for i=1, 2 do
  aliases[fmt("mpvo%dl", i + 1)] = fmt("mpv-%02d:out_0", i)
  aliases[fmt("mpvo%dr", i + 1)] = fmt("mpv-%02d:out_1", i)
  aliases[fmt("csdo%dl", i + 1)] = fmt("csound6-%02d:output1", i)
  aliases[fmt("csdo%dr", i + 1)] = fmt("csound6-%02d:output2", i)
end

local function do_cmd(cmd)
    -- io.write(cmd, "\n")
    os.execute(cmd)
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
    do_cmd(cmd)
  end ,
  disconnect = function (t)
    local one = aliases[t[1]] or t[1]
    local two = aliases[t[2]] or t[2]
    local cmd = fmt("jack_disconnect %s %s", one, two)
    do_cmd(cmd)
  end ,

  renoise_resample = function ()
    commands.connect({ "rnso1l", "rnsi2l" })
    commands.connect({ "rnso1r", "rnsi2r" })
  end ,
  
  aliases = function ()
    local sorted = {}
    for k in pairs(aliases) do
      table.insert(sorted, k)
    end
    table.sort(sorted)
    for i=1, #sorted do
      io.write(fmt("%s > %s\n", sorted[i], aliases[sorted[i]]))
    end
  end ,

  hello = function (t)
    local greetings = {
      "meow", "yo", "sup?"
    }
    math.randomseed(os.time())
    math.random() math.random() math.random()

    io.write(fmt("%s\n", t[0] ~= "meow" and greetings[math.random(1, 3)] or "meow"))
  end ,

  bye = function ()
    io.write("see u\n")
    os.exit()
  end ,
}

local function alias_cmd(main, t)
  for i=1, #t do
    commands[t[i]] = commands[main]
  end
end

local cmd_aliases = {
  bye = { "cya", "q" } ,
  hello = { "hey", "hi", "sup?", "meow" } ,
  connect = { "conn", "c" } ,
  disconnect = { "disconn", "dc" } ,
  list = { "l" } ,
  renoise_resample = { "rns" } ,
  aliases = { "a" } ,
}

for k, v in pairs(cmd_aliases) do
  alias_cmd(k, v)
end

local function eval(str)
  if str == "" then return end
  local todo = commands[str:match("(%S+)%s*")]
  if type(todo) == "string" then
    do_cmd(todo)
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
  io.write("~ ")
  eval(io.read())
end
