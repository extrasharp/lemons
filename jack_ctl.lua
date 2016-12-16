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

local function gen_aliases(base, lfmt, rfmt, start, to)
  for i=start, to do
    aliases[fmt("%s%dl", base, i)] = fmt(lfmt, i)
    aliases[fmt("%s%dr", base, i)] = fmt(rfmt, i)
  end
end

local function gen_generics(base, lfmt, rfmt, ct)
  aliases[fmt("%s1l", base)] = lfmt
  aliases[fmt("%s1r", base)] = rfmt
  local _, _, l1, l2 = lfmt:find("(.+):(.+)")
  local _, _, r1, r2 = rfmt:find("(.+):(.+)")
  for i=2, ct do
    aliases[fmt("%s%dl", base, i)] = fmt("%s-%02d:%s", l1, i - 1, l2)
    aliases[fmt("%s%dr", base, i)] = fmt("%s-%02d:%s", r1, i - 1, r2)
  end
end

gen_aliases("rnsi"
  , "renoise:input_%02d_left"
  , "renoise:input_%02d_right"
  , 1, 8)
gen_aliases("rnso"
  , "renoise:output_%02d_left"
  , "renoise:output_%02d_right"
  , 1, 2)
gen_aliases("ffxo"
  , "Firefox:AudioStream_%d_out_0"
  , "Firefox:AudioStream_%d_out_1"
  , 0, 2)

gen_generics("mpvo"
  , "mpv:out_0"
  , "mpv:out_1"
  , 3)
gen_generics("csdo"
  , "csound6:output1"
  , "csound6:output2"
  , 3)

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
