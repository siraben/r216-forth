#!/usr/bin/env luajit

local image_path, labels_path = ...

local label_to_addr = {}
local addr_to_label = {}
do
	local labels_handle = assert(io.open(labels_path, "rb"))
	for line in assert(labels_handle:read("*a")):gmatch("[^\r\n]+") do
		local label, addr = line:match("^([^ ]+) ([^ ]+)$")
		addr = tonumber(addr)
		label_to_addr[label] = addr
		addr_to_label[addr] = label
	end
	assert(labels_handle:close())
end

local get_data
do
	local image_handle = assert(io.open(image_path, "rb"))
	function get_data(addr)
		assert(image_handle:seek("set", addr * 4))
		local bytes = assert(image_handle:read(4))
		local a0, a1, a2, a3 = string.byte(bytes, 1, 4)
		return a0 + a1 * 0x100
	end
end

local function hash_for_link(ptr)
	local length = get_data(ptr + 1)
	local hash = length * 4
	for index = 1, math.min(3, length) do
		hash = bit.bxor(hash, get_data(ptr + 1 + index))
	end
	hash = bit.band(hash, 0x1F)
	return hash
end

local found = 0
local links = {}
for entry = 0, 31 do
	local ptr = get_data(label_to_addr["find_hashtable"] + entry)
	while ptr ~= 0 do
		found = found + 1
		local hash = hash_for_link(ptr)
		if hash ~= entry then
			print(("%s is in bucket 0x%02X, should be in bucket 0x%02X"):format(addr_to_label[ptr], entry, hash))
			os.exit(1)
		end
		ptr = get_data(ptr)
	end
end
print(("all %i links are good"):format(found))
