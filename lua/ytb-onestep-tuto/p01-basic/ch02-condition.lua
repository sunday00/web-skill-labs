local a, b, c = 1, 1, 10

if a > 10 then
    print('gt 10')
else
    print('lt 10')
end

if a < 10 and a == b then
    print('OK')
end

if a ~= b then
    print('diff')
else
    print('same')
end

if not (a ~= b) then
    print('not diff')
end

if a > 10 or b > 10 then
    print'nice'
end

local z = 0
if z then
    print('zero also true')
end

local notZ = not 0
print(notZ)

local notEmpty = not ''
print(notEmpty) -- empty string is also true



