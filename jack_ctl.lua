#! /bin/luajit

local string = require "string"
local fmt = string.format

function table.join(t1, t2)
  local out = {}
  for k, v in pairs(t1) do
    out[k] = v
  end
  for k, v in pairs(t2) do
    out[k] = v
  end
  return out
end

-- aliases for jack ports

local aliases = {}

local function gen_alias(base, lfmt, rfmt, start, to)
  for i=start, to do
    aliases[fmt("%s%dl", base, i)] = fmt(lfmt, i)
    aliases[fmt("%s%dr", base, i)] = fmt(rfmt, i)
  end
end

local function gen_generic(base, lfmt, rfmt, ct)
  aliases[fmt("%s1l", base)] = lfmt
  aliases[fmt("%s1r", base)] = rfmt
  local _, _, l1, l2 = lfmt:find("(.+):(.+)")
  local _, _, r1, r2 = rfmt:find("(.+):(.+)")
  for i=2, ct do
    aliases[fmt("%s%dl", base, i)] = fmt("%s-%02d:%s", l1, i - 1, l2)
    aliases[fmt("%s%dr", base, i)] = fmt("%s-%02d:%s", r1, i - 1, r2)
  end
end

local function gen_aliases()
  aliases = table.join(aliases, {
    syso1l = "system:capture_1" ,
    syso1r = "system:capture_2" ,
    sysi1l = "system:playback_1" ,
    sysi1r = "system:playback_2" ,
    mpdo1l = "'Music Player Daemon:left'" ,
    mpdo1r = "'Music Player Daemon:right'" ,
    pulo1l = "'PulseAudio JACK Sink:front-left'" ,
    pulo1r = "'PulseAudio JACK Sink:front-right'" ,
    puli1l = "'PulseAudio JACK Source:front-left'" ,
    puli1r = "'PulseAudio JACK Source:front-right'" ,
  })

  gen_alias("rnsi"
    , "renoise:input_%02d_left"
    , "renoise:input_%02d_right"
    , 1, 8)
  gen_alias("rnso"
    , "renoise:output_%02d_left"
    , "renoise:output_%02d_right"
    , 1, 2)
  gen_alias("ffxo"
    , "Firefox:AudioStream_%d_out_0"
    , "Firefox:AudioStream_%d_out_1"
    , 0, 2)
  gen_generic("mpvo"
    , "mpv:out_0"
    , "mpv:out_1"
    , 3)
  gen_generic("csdo"
    , "csound6:output1"
    , "csound6:output2"
    , 3)
end

local num_aliases = {}

local function gen_numbered_aliases()
  local status = os.execute("jack_control status &> /dev/null")
  if status ~= 0 then return end

  local f = io.popen("jack_lsp")
  if not f then return end
  local clients = {}
  local ct = -1
  local chan_ct = -1
  while true do
    local line = f:read()
    if not line then break end

    local _, _, client, channel = line:find("(.+):(.+)")
    if clients[client] then
      chan_ct = chan_ct + 1
      if chan_ct >= 10 then
        chan_ct = 0
        ct = ct + 1
      end
    else
      clients[client] = true
      ct = ct + 1
      chan_ct = 0
    end

    num_aliases[fmt("%d%d", ct, chan_ct)] = line
  end
  f:close()
end

gen_aliases()
gen_numbered_aliases()

-- commands

local function do_cmd(cmd, do_print)
    if do_print then
      io.write(fmt("%s\n", cmd))
    end
    os.execute(cmd)
end

local function get_name(str)
  return num_aliases[str] or aliases[str] or str or "'(-_-'"
end

local function get_next_channel(str)
  if not str then return nil end
  if #str == 2 then
    local num = tonumber(str)
    if not num then return nil end
    return get_name(fmt("%02d", num + 1))
  elseif #str == 6 then
    -- error if str[5] ~= l
    return get_name(fmt("%sr", str:sub(1,5)))
  end
  return nil
end

local commands
commands = {
  list = "jack_lsp" ,
  start = "jack_control start" ,
  stop = "jack_control stop" ,

  connect = function (t)
    local one, two
    if t[1] == "both" or t[1] == "b" then
      commands.connect({ t[2], t[3] })
      one, two = get_next_channel(t[2]), get_next_channel(t[3])
    else
      one, two = get_name(t[1]), get_name(t[2])
    end

    local cmd = fmt("jack_connect %s %s", one, two)
    do_cmd(cmd)
  end ,
  disconnect = function (t)
    local one, two
    if t[1] == "both" or t[1] == "b" then
      commands.disconnect({ t[2], t[3] })
      one, two = get_next_channel(t[2]), get_next_channel(t[3])
    else
      one, two = get_name(t[1]), get_name(t[2])
    end

    local cmd = fmt("jack_disconnect %s %s", one, two)
    do_cmd(cmd)
  end ,

  renoise_resample = function ()
    commands.connect({ "both", "rnso1l", "rnsi2l" })
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

  numbered_aliases = function ()
    local sorted = {}
    for k in pairs(num_aliases) do
      table.insert(sorted, k)
    end
    table.sort(sorted)
    for i=1, #sorted do
      io.write(fmt("%s > %s\n", sorted[i], num_aliases[sorted[i]]))
    end
  end ,

  hello = function (t)
    local greetings = {
      "meow", "yo", "sup?", "heya"
    }
    math.randomseed(os.time())
    math.random() math.random() math.random()
    io.write(fmt("%s\n"
      , t[0] ~= "meow" and greetings[math.random(1, #greetings)] or "meow"))
  end ,

  bye = function ()
    io.write("~ see u ~\n")
    os.exit()
  end ,
}

local function alias_cmd(main, t)
  for i=1, #t do
    commands[t[i]] = commands[main]
  end
end

local cmd_aliases = {
  bye              = { "cya", "q" } ,
  hello            = { "hey", "hi", "sup?", "meow" } ,
  connect          = { "conn", "c" } ,
  disconnect       = { "disconn", "dc" } ,
  renoise_resample = { "rns" } ,
  list             = { "l" } ,
  aliases          = { "a" } ,
  numbered_aliases = { "na", "num" } ,
}

for k, v in pairs(cmd_aliases) do
  alias_cmd(k, v)
end

-- main

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
    io.write(fmt("what\n", str))
  end
end

if #arg > 0 then
  eval(table.concat(arg, " "))
  return
end

while true do
  io.write("~ ")
  eval(io.read())
end
