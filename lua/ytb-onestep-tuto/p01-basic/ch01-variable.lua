local a, b, c = 11, 22, 33.4
print(a, b, c)

local x, y = a + b, a - b
print(x, y)

local f = 2 ^ 0.5
print(f)


local s = 'hello strings'
local s2 = "hello strings"

print(s, s2)

print(#s) -- length of strings

a = 'much '
b = 'better'
print(a .. b) -- .. is concat

c = 44.4
local d = 'hello there'
print(type(c), type(d))
print(type(c) == 'number')
print(type(type(c) == 'number') == 'boolean')

print(1 ~= 2) -- 1 != 2
-- lua is weird... this statement...;;

print(0 == '0') -- false

s = 'hi'
s2 = 'hi'
print(s == s2) -- true. easy~ right?

print(#'한글의 글자수')
print(#'한글의글자수')
