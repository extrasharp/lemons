local string = require "string"
local fmt = string.format

local ffi = require "ffi"
ffi.cdef [[
struct pollfds {
  int fd;
  short int events;
  short int revents;
};
int poll(struct pollfds *fds, unsigned long nfds, int timeout);
]]
local bit = require "bit"
local ev = {
    POLLIN = 0x001
  , POLLPRI = 0x002
  , POLLOUT = 0x004
  , POLLRDNORM = 0x040
  , POLLRDBAND = 0x080
  , POLLWRNORM = 0x100
  , POLLWRBAND = 0x200
  , POLLMSG = 0x400
  , POLLREMOVE = 0x1000
  , POLLRDHUP = 0x2000
  , POLLERR = 0x008
  , POLLHUP = 0x010
  , POLLNVAL = 0x020
}

function file_to_str(filepath)
  local f = io.open(filepath, "r")
  local t = f:read("*a")
  f:close()
  return t
end

function cmd_to_str(cmd)
  local f = io.popen(cmd, "r")
  local t = f:read("*a")
  f:close()
  return t
end

local buffer = ""

function buffer_clear()
  buffer = ""
end

function buffer_add(text)
  buffer = fmt("%s%s", buffer, text)
end

function buffer_set_pos(pos)
  local t = { l = "%{l}", r = "%{r}", c = "%{c}" }
  buffer_add(t[pos])
end

local sw = false

function put_batt()
  local capa = file_to_str("/sys/class/power_supply/BAT0/capacity")
  local stat = file_to_str("/sys/class/power_supply/BAT0/status"):sub(1,1)
  local capa_int = tonumber(capa)
  local status_strs = { D = "b", C = "c", F = "f", U = "x" }
  if capa_int < 9 and stat == "D" then
    os.execute("notify-send 'battery very low'")
  elseif sw and capa_int < 15 and stat == "D" then
    os.execute("notify-send 'battery low'")
  end
  sw = not sw
  buffer_add(status_strs[stat] or "?")
  buffer_add(fmt("%3d", capa_int))
end

function put_mpd()
  local f = io.popen("mpc -f '%artist% >> %title% >> %album%'")
  local mpd = f:read("*l")
  local playing = f:read("*l")
  buffer_add(playing:find("playing") and mpd or "[paused]")
end

function put_time()
  local time = cmd_to_str("date +'%I %M %S'")
  local hr  = time:sub(1, 2)
  local min = time:sub(4, 5)
  local sec = tonumber(time:sub(7, 8))
  sec = sec - (sec % 3)
  local time = fmt("♡ %s, %s, %02d ♡", hr, min, sec)
  buffer_add(time)
end

function main()
  local pfds = ffi.new("struct pollfds[1]")
  pfds[0].fd = 0
  pfds[0].events = ev.POLLIN

  local desktop_info = ""

  while true do
    local res = ffi.C.poll(pfds[0], 1, 1000);
    -- res of 0 means timedout
    if res > 0 then
      if (bit.band(pfds[0].revents, ev.POLLIN) ~= 0) then
        desktop_info = io.read("*l") or desktop_info
      elseif (bit.band(pfds[0].revents, ev.POLLHUP) ~= 0) then
        io.write("goodbye\n")
        break
      end
    end

    buffer_clear()
    buffer_set_pos("l")
    buffer_add(desktop_info)
    buffer_set_pos("c")
    put_mpd()
    buffer_set_pos("r")
    put_time()
    buffer_add(" ")
    put_batt()
    io.write(buffer.."\n")
    io.flush()
  end
end

main()
