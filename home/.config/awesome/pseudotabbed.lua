---------------------------------------------------------------------------
--- A "pseudo-tabbed" layouts module for awful
--
-- @author dzwdz
-- @author Donald Ephraim Curtis &lt;dcurtis@cs.uiowa.edu&gt;
-- @author Julien Danjou &lt;julien@danjou.info&gt;
-- @copyright 2025 dzwdz
-- @copyright 2009 Donald Ephraim Curtis
-- @copyright 2008 Julien Danjou
---------------------------------------------------------------------------

-- Grab environment we need
local tag = require("awful.tag")
local client = require("awful.client")
local ipairs = ipairs
local math = math
local capi =
{
	mouse = mouse,
	screen = screen,
	mousegrabber = mousegrabber
}

local tile = {}

--- The tile layout layoutbox icon.
-- @beautiful beautiful.layout_tile
-- @param surface
-- @see gears.surface

--- The tile top layout layoutbox icon.
-- @beautiful beautiful.layout_tiletop
-- @param surface
-- @see gears.surface

--- The tile bottom layout layoutbox icon.
-- @beautiful beautiful.layout_tilebottom
-- @param surface
-- @see gears.surface

--- The tile left layout layoutbox icon.
-- @beautiful beautiful.layout_tileleft
-- @param surface
-- @see gears.surface

local function apply_size_hints(c, width, height, useless_gap)
	local bw = c.border_width
	width, height = width - 2 * bw - useless_gap, height - 2 * bw - useless_gap
	width, height = c:apply_size_hints(math.max(1, width), math.max(1, height))
	return width + 2 * bw + useless_gap, height + 2 * bw + useless_gap
end

local function tile_group(gs, cls, wa, orientation, fact, group, useless_gap)
	-- get our orientation right
	local height = "height"
	local width = "width"
	local x = "x"
	local y = "y"
	if orientation == "top" or orientation == "bottom" then
		height = "width"
		width = "height"
		x = "y"
		y = "x"
	end

	-- make this more generic (not just width)
	local available = wa[width] - (group.coord - wa[x])

	-- find our total values
	local total_fact = 0
	local min_fact = 1
	local size = group.size
	for c = group.first,group.last do
		-- determine the width/height based on the size_hint
		local i = c - group.first +1
		local size_hints = cls[c].size_hints
		local size_hint = size_hints["min_"..width] or size_hints["base_"..width] or 0
		size = math.max(size_hint, size)

		-- calculate the height
		if not fact[i] then
			fact[i] = min_fact
		else
			min_fact = math.min(fact[i],min_fact)
		end
		total_fact = total_fact + fact[i]
	end
	size = math.max(1, math.min(size, available))

	local coord = wa[y]
	local used_size = 0
	local unused = wa[height]
	for c = group.first,group.last do
		local geom = {}
		local hints = {}
		local i = c - group.first +1
		geom[width] = size
		geom[height] = wa[height]
		geom[x] = group.coord
		geom[y] = wa[y]
		gs[cls[c]] = geom
		hints.width, hints.height = apply_size_hints(cls[c], geom.width, geom.height, useless_gap)
		unused = unused - hints[height]
		total_fact = total_fact - fact[i]
		used_size = math.max(used_size, hints[width])
	end

	return used_size
end

local function do_tile(param, orientation)
	local t = param.tag or capi.screen[param.screen].selected_tag
	orientation = orientation or "right"

	-- This handles all different orientations.
	local width = "width"
	local x = "x"
	if orientation == "top" or orientation == "bottom" then
		width = "height"
		x = "y"
	end

	local gs = param.geometries
	local cls = param.clients
	local useless_gap = param.useless_gap
	local nmaster = math.min(t.master_count, #cls)
	local nother = math.max(#cls - nmaster,0)

	local mwfact = t.master_width_factor
	local wa = param.workarea
	local ncol = t.column_count

	local data = tag.getdata(t).windowfact

	if not data then
		data = {}
		tag.getdata(t).windowfact = data
	end

	local coord = wa[x]
	local place_master = true
	if orientation == "left" or orientation == "top" then
		-- if we are on the left or top we need to render the other windows first
		place_master = false
	end

	local grow_master = t.master_fill_policy == "expand"
	-- this was easier than writing functions because there is a lot of data we need
	for _ = 1,2 do
		if place_master and nmaster > 0 then
			local size = wa[width]
			if nother > 0 or not grow_master then
				size = math.min(wa[width] * mwfact, wa[width] - (coord - wa[x]))
			end
			if nother == 0 and not grow_master then
			  coord = coord + (wa[width] - size)/2
			end
			if not data[0] then
				data[0] = {}
			end
			coord = coord + tile_group(gs, cls, wa, orientation, data[0],
									   {first=1, last=nmaster, coord = coord, size = size}, useless_gap)
		end

		if not place_master and nother > 0 then
			local last = nmaster

			-- we have to modify the work area size to consider left and top views
			local wasize = wa[width]
			if nmaster > 0 and (orientation == "left" or orientation == "top") then
				wasize = wa[width] - wa[width]*mwfact
			end
			for i = 1,ncol do
				-- Try to get equal width among remaining columns
				local size = math.min( (wasize - (coord - wa[x])) / (ncol - i + 1) )
				local first = last + 1
				last = last + math.floor((#cls - last)/(ncol - i + 1))
				-- tile the column and update our current x coordinate
				if not data[i] then
					data[i] = {}
				end
				coord = coord + tile_group(gs, cls, wa, orientation, data[i],
										   { first = first, last = last, coord = coord, size = size }, useless_gap)
			end
		end
		place_master = not place_master
	end

end

function tile.skip_gap(nclients, t)
	return nclients == 1 and t.master_fill_policy == "expand"
end

--- The main tile algo, on the right.
-- @param screen The screen number to tile.
-- @clientlayout awful.layout.suit.tile.right
tile.right = {}
tile.right.name = "pab"
tile.right.arrange = do_tile
tile.right.skip_gap = tile.skip_gap
function tile.right.mouse_resize_handler(c, corner, x, y)
	-- idc
end

--- The main tile algo, on the left.
-- @param screen The screen number to tile.
-- @clientlayout awful.layout.suit.tile.left
tile.left = {}
tile.left.name = "pableft"
tile.left.skip_gap = tile.skip_gap
function tile.left.arrange(p)
	return do_tile(p, "left")
end
function tile.left.mouse_resize_handler(c, corner, x, y)
	-- idc
end

--- The main tile algo, on the bottom.
-- @param screen The screen number to tile.
-- @clientlayout awful.layout.suit.tile.bottom
tile.bottom = {}
tile.bottom.name = "pabbottom"
tile.bottom.skip_gap = tile.skip_gap
function tile.bottom.arrange(p)
	return do_tile(p, "bottom")
end
function tile.bottom.mouse_resize_handler(c, corner, x, y)
	-- idc
end

--- The main tile algo, on the top.
-- @param screen The screen number to tile.
-- @clientlayout awful.layout.suit.tile.top
tile.top = {}
tile.top.name = "pabtop"
tile.top.skip_gap = tile.skip_gap
function tile.top.arrange(p)
	return do_tile(p, "top")
end
function tile.top.mouse_resize_handler(c, corner, x, y)
	-- idc
end

tile.arrange = tile.right.arrange
tile.mouse_resize_handler = tile.right.mouse_resize_handler
tile.name = tile.right.name

return tile

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
