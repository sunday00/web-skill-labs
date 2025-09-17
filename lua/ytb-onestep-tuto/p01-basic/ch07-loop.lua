local k = 1
while k <= 5 do
    print(k)
    k = k + 1
end

local t = { 'apple', 'banana', 'durian', 'egg', 'fig' }
local i = 1
while i <= #t do
    print(t[i])
    i = i + 1
end

print('hello ' .. 10)

for i = 1, 5 do
    print(t[i])
end

for j = 2, 10, 2 do
    print(j)
end

print(j)

for i, e in ipairs(t) do
    print(i, e)
end

print(ipairs(t))

local t2 = { name='k', age=38, weight=60}
for i, e in ipairs(t2) do
    print(i, e)
end

local t3 = { 99, name='k', age=38, weight=60, 'HI'}
for i, e in ipairs(t3) do
    print(i, e)
end

for k, v in pairs(t2) do
    print(k, v)
end

print("-======--")

for k, v in pairs(t3) do
    print(k, v)
end
