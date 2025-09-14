print 'input radius'

local r = io.read()
print('r : ' .. r, type(r))

r = tonumber(r)
print(type(r))
local pi = 3.14
local area = pi * r ^ 2
print(area)
