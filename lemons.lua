#!/usr/bin/luajit

local debug_mode = arg[1] == "debug"

local string = require "string"
local fmt = string.format

-- low level

local ffi = require "ffi"
ffi.cdef [[
typedef int mqd_t;
typedef int ssize_t;
mqd_t mq_open(const char *, int);
ssize_t mq_receive(mqd_t, char *, size_t, unsigned int *);

struct msg {
  int do_quit;
  int current;
  int windows[6];
  int urgent[6];
};

struct pollfds {
  int fd;
  short int events;
  short int revents;
};

int poll(struct pollfds *, unsigned long, int);
]]

local rt = ffi.load "rt"
local bit = require "bit"

local POLL = {
    IN = 0x001
  , PRI = 0x002
  , OUT = 0x004
  , RDNORM = 0x040
  , RDBAND = 0x080
  , WRNORM = 0x100
  , WRBAND = 0x200
  , MSG = 0x400
  , REMOVE = 0x1000
  , RDHUP = 0x2000
  , ERR = 0x008
  , HUP = 0x010
  , NVAL = 0x020
}

local O = {
    RDONLY = 00
  , WRONLY = 01
  , RDWR = 02
}

local function file_to_str(filepath, read)
  local f = io.open(filepath, "r")
  if not f then return "" end
  local t = f:read(read or "*a") or ""
  f:close()
  return t
end

local function cmd_to_str(cmd, read)
  local f = io.popen(cmd, "r")
  if not f then return "" end
  local t = f:read(read or "*a") or ""
  f:close()
  return t
end

local function clone(t)
  local ret = {}
  for k, v in pairs(t) do
      ret[k] = type(v) == "table" and clone(v) or v
  end
  return ret
end

-- base obj

local obj = {
  __call = function (self, ...)
    if self.new then
      return self.new(...)
    end
  end
}

setmetatable(obj, obj)
obj.__index = obj

function obj.new()
  local ret = {}
  ret.__index = ret
  return setmetatable(ret, obj)
end

-- animations

local anim = obj()

function anim.new(t)
  local ret = clone(t)
  ret.cur = 1
  return setmetatable(ret, anim)
end

function anim:peek()
  return self[self.cur]
end

function anim:next()
  self.cur = self.cur + 1
  if self.cur > #self then
    self.cur = 1
  end
  return self:peek()
end

function anim:prev()
  self.cur = self.cur - 1
  if self.cur < 1 then
    self.cur = #self
  end
  return self:peek()
end

-- desktops

local desktop_info = obj()

function desktop_info.new(t)
  local names = clone(t)
  return setmetatable({
      names = names ,
      ct = #names   ,
      current = 1   ,
      urgent = {}   ,
      windows = {}  ,
    }, desktop_info)
end

function desktop_info:set(raw)
  for i = 0, self.ct - 1 do
    self.urgent[i + 1] = raw.urgent[i]
    self.windows[i + 1] = raw.windows[i]
  end
  self.current = raw.current + 1
end

function desktop_info:to_str()
  local ret = ""
  for i = 1, self.ct do
    local indc = nil
    if i == self.current then
      indc = ":"
    elseif self.urgent[i] and self.urgent[i] > 0 then
      indc = "!"
    elseif self.windows[i] and self.windows[i] > 0 then
      indc = "."
    end
    ret = indc and fmt("%s%s%s ", ret, indc, self.names[i]) or ret
  end
  return ret:sub(1, #ret - 1)
end

-- buffer

local buffer = obj()
buffer.__call = function (self, ...)
  return self:add(...)
end

function buffer.new()
  return setmetatable({ "" }, buffer)
end

function buffer:clear()
  self[1] = ""
end

function buffer:add(text)
  self[1] = fmt("%s%s", self[1], text or "")
  return self
end

function buffer:to_str()
  return self[1]
end

local pos = {
  l = "%{l}" ,
  r = "%{r}" ,
  c = "%{c}" ,
}

local colors = {
  bg = "#191919" ,
  fg = "#AAAAAA" ,
  reset = "-"    ,
}

local function colorize(fgc, bgc)
  return fmt("%s%s"
    , fgc and fmt("%%{F%s}", fgc) or ""
    , bgc and fmt("%%{B%s}", bgc) or "")
end

-- info get

local sw = false

local function batt()
  local capa = file_to_str("/sys/class/power_supply/BAT0/capacity")
  local stat = file_to_str("/sys/class/power_supply/BAT0/status"):sub(1, 1)
  local capa_int = tonumber(capa) or 0
  local status_strs = { D = ".", C = ":", F = ":", U = "!" }
  if stat == "D" then
    if capa_int < 9 then
      os.execute("notify-send 'battery very low'")
    elseif sw and capa_int < 15 then
      os.execute("notify-send 'battery low'")
    end
  end
  sw = not sw
  return fmt("%d%s", capa_int, status_strs[stat] or "?")
end

local paused = anim {
  "   (__" ,
  "   (__" ,
  "  .(__" ,
  "  z(__" ,
  " Z (__" ,
  "z  (__" ,
}

local sep = "║"

local mpc_find = {
  "artist", "title", "file", "album"
}
local mpc_cmd = "mpc -f '"
for i = 1, #mpc_find do
  mpc_cmd = fmt("%s%%%s%%#%s", mpc_cmd, mpc_find[i], sep)
end
mpc_cmd = fmt("%s'", mpc_cmd)

local last_info
local tags = {}

local spin = {"-", "/", "\\"}
local sp1 = anim(spin)
local sp2 = anim(spin)

local oh = {"o ",  " o"}
local oh1 = anim(oh)
local oh2 = anim(oh)

local function mpd()
  local f = io.popen(mpc_cmd)
  if not f then return end
  local info = f:read("*l") or ""
  local status = f:read("*l") or ""
  f:close()

  if last_info ~= info then
    local idx = 1
    for str in info:gmatch(fmt("([^%s]*)%s", sep, sep)) do
      if str == "" then str = nil end
      tags[mpc_find[idx]] = str
      idx = idx + 1
    end

    _, _, tags.file = tags.file:find("([^/]+%.%w+)")
  end

  last_info = info

  local playing = status:find("playing")
  return playing and
    fmt("%s %s %s %s %s"
      , tags.artist or ""
      , tags.artist and oh1:next() or sp1:next()
      , tags.title  or tags.file
      , tags.album  and oh2:next() or sp2:prev()
      , tags.album  or "")
    or paused:next()
end

local function time()
  local time = cmd_to_str("date +'%I%M%S'")
  local hr  = time:sub(1, 2)
  local min = time:sub(3, 4)
  local sec = tonumber(time:sub(5, 6)) or 0
  sec = sec - (sec % 3)
  return fmt("%s:%s.%02d", hr, min, sec)
end

-- setup

local mqd = -1
while mqd < 0 do
  mqd = rt.mq_open("/monsterwm", O.RDONLY)
  ffi.C.poll(nil, 0, 100)
end

local pfds = ffi.new("struct pollfds[1]")
pfds[0].fd = mqd
pfds[0].events = POLL.IN

local raw_info = ffi.new("struct msg[1]");

local bar = debug_mode and io.stdout
or
  io.popen(fmt(
  '~/dotfiles/_wm/bar/lemonbar -f "tewi:pixelsize=10"'..
  ' -f "Kochi Gothic:pixelsize=10:antialias=false"'..
  ' -B "%s" -F "%s" -g x12', colors.bg, colors.fg), "w")

local buf = buffer()

local info = desktop_info {
  "lov♡" , "kis♡" , "ya ♡" ,
  "2 ♡♡" , "♡jx " , "(vv)" ,
}

local wink = anim {
  " (oo" ,
  " (oo" ,
  "★(-o" ,
}

-- loop

while true do
  local res = ffi.C.poll(pfds[0], 1, 1000);
  if res > 0 then
    if (bit.band(pfds[0].revents, POLL.IN) ~= 0) then
      rt.mq_receive(mqd, ffi.cast("char *", raw_info), ffi.sizeof("struct msg"), nil);
      if raw_info[0].do_quit == 1 then
        break
      else
        info:set(raw_info[0])
      end
    end
  end

  buf:clear()

  buf:add(pos.l)
  buf:add(colorize(colors.bg, colors.fg))
  buf:add(info:to_str())
  buf:add(colorize(colors.fg, colors.bg))
  buf:add("")

  buf:add(pos.c)
  buf:add(mpd())

  buf:add(pos.r)
  buf:add("")
  buf:add(colorize(colors.bg, colors.fg))
  buf:add(wink:next()) " ♡ " (time()) " ♡ " (batt())
  buf:add(colorize(colors.reset, colors.reset))

  bar:write(buf:to_str(), "\n")
  bar:flush()
end

-- see you

bar:close()
io.write("goodbye\n")
