#!/usr/bin/luajit

-- TODO debug mode (dont spawn bar) (print to stdout)

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

function file_to_str(filepath)
  local f = io.open(filepath, "r")
  if not f then return "" end
  local t = f:read("*a") or ""
  f:close()
  return t
end

function cmd_to_str(cmd)
  local f = io.popen(cmd, "r")
  if not f then return "" end
  local t = f:read("*a") or ""
  f:close()
  return t
end

-- buffer

local buffer = {}

function buffer:new(b)
  b = b or { "" }
  setmetatable(b, self)
  self.__index = self
  return b
end

function buffer:clear()
  self[1] = ""
end

function buffer:add(text)
  self[1] = fmt("%s%s", self[1], text or "")
end

function buffer:to_str()
  return self[1]
end

function buffer:set_pos(pos)
  local t = { l = "%{l}", r = "%{r}", c = "%{c}" }
  self:add(t[pos] or "")
end

function buffer:set_colors(fgc, bgc)
  self:add(fmt("%s%s"
    , fgc and fmt("%%{F%s}", fgc) or ""
    , bgc and fmt("%%{B%s}", bgc) or ""))
end

local sw = false

function buffer:put_batt()
  local capa = file_to_str("/sys/class/power_supply/BAT0/capacity")
  local stat = file_to_str("/sys/class/power_supply/BAT0/status"):sub(1,1)
  local capa_int = tonumber(capa) or 0
  local status_strs = { D = "b", C = "c", F = "f", U = "x" }
  if capa_int < 9 and stat == "D" then
    os.execute("notify-send 'battery very low'")
  elseif sw and capa_int < 15 and stat == "D" then
    os.execute("notify-send 'battery low'")
  end
  sw = not sw
  self:add(status_strs[stat] or "?")
  self:add(fmt("%3d", capa_int))
end

function buffer:put_mpd()
  local f = io.popen("mpc -f '%artist% >> %title% >> %album%'")
  if not f then do return end end
  local mpd = f:read("*l") or ""
  local playing = f:read("*l") or ""
  self:add(playing:find("playing") and mpd or ".paused.")
  f:close()
end

function buffer:put_time()
  local time = cmd_to_str("date +'%I%M%S'")
  local hr  = time:sub(1, 2)
  local min = time:sub(3, 4)
  local sec = tonumber(time:sub(5, 6)) or 0
  sec = sec - (sec % 3)
  local time = fmt("♡ %s, %s, %02d ♡", hr, min, sec)
  self:add(time)
end

function buffer:put_anim(anim)
  if not anim.cur or anim.cur > #anim then
    anim.cur = 1
  end
  self:add(anim[anim.cur])
  anim.cur = anim.cur + 1
end

-- more stuff

local desktop_names = {
    "love♡", "kiss♡", "yay ♡"
  , "22 ♡♡", "♡9 10", "(vv^)"
}

function process_desktop_info(msg)
  local ret = ""
  for i = 0, 5 do
    local indc = " "
    if i == msg.current then
      indc = "#"
    elseif msg.urgent[i] > 0 then
      indc = "*"
    elseif msg.windows[i] > 0 then
      indc = "."
    end
    ret = fmt(i < 5 and "%s%s%s%s " or "%s%s%s%s", ret, indc, desktop_names[i + 1], indc)
  end
  return ret
end

local colors = {
    bg = "#222222"
  , fg = "#AAAAAA"
  , reset = "-"
}

local anim1 = {
    " (oo"
  , " (oo"
  , "★(-o"
}

-- main

function main()
  local pfds = ffi.new("struct pollfds[1]")
  local mqd = -1
  while mqd < 0 do
    mqd = rt.mq_open("/monsterwm", O.RDONLY)
    ffi.C.poll(nil, 0, 500)
  end

  pfds[0].fd = mqd
  pfds[0].events = POLL.IN

  local raw_info = ffi.new("struct msg[1]");

  local bar = io.popen(  
  '~/documents/programming/c/windowmgrs/bar/lemonbar -f "tewi:pixelsize=10"'..
  ' -f "Kochi Gothic:pixelsize=10:antialias=false"'..
  ' -B "'..colors.bg..'" -F "'..colors.fg..'" -g x12', "w")

  local buf = buffer:new()
  local desktop_info = ""

  while true do
    local res = ffi.C.poll(pfds[0], 1, 1000);
    if res > 0 then
      if (bit.band(pfds[0].revents, POLL.IN) ~= 0) then
        rt.mq_receive(mqd, ffi.cast("char *", raw_info), ffi.sizeof("struct msg"), nil);
        if raw_info[0].do_quit == 1 then
          io.write "goodbye\n"
          break
        else
          desktop_info = process_desktop_info(raw_info[0])
        end
      end
    end

    buf:clear()

    buf:set_pos("l")
    buf:set_colors(colors.bg, colors.fg)
    buf:add(desktop_info)
    buf:set_colors(colors.fg, colors.bg)
    buf:add("")

    buf:set_pos("c")
    buf:put_mpd()

    buf:set_pos("r")
    buf:add("")
    buf:set_colors(colors.bg, colors.fg)
    buf:put_anim(anim1)
    buf:add(" ")
    buf:put_time()
    buf:add(" ")
    buf:put_batt()
    buf:set_colors(colors.reset, colors.reset)

    bar:write(buf:to_str(), "\n")
    bar:flush()
  end

  bar:close()
end

main()
