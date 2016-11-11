#!/usr/bin/luajit

if    arg[1] ~= "next"
  and arg[1] ~= "prev" then
    do return end
end

local save_loc = "/home/mel/.last_desktop"

local files = {
    "~/images/inspiration/bugs/aphids.jpg"
  , "~/images/inspiration/textures/pink_slime.jpg"
  , "~/desktop/sort\\ later/aaaa.png"
  , "~/desktop/sort\\ later/twisty.jpg"
}

local f = io.open(save_loc, "r")
if not f then do return end end
local desktop = f:read("*a")
f:close()

local at = 1

for i = 1, #files do
  if files[i] == desktop then
    at = i
  end
end

if arg[1] == "next" then
  at = at + 1
  if at > #files then
    at = 1
  end
elseif arg[1] == "prev" then
  at = at - 1
  if at <= 0 then
    at = #files
  end
end

desktop = files[at]

os.execute("feh --bg-fill "..desktop)

local f = io.open(save_loc, "w")
f:write(desktop)
f:close()
