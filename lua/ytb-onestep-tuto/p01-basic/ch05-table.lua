local t = {11, 22, 33, 44.5, -11, 'k', true}

print(t)

print(t[1]) -- not starts with zero
print(t[6])
print(t[7])

t[2] = -1 * t[2]
print(t[2])

local t2 = { 11, {'alpha', 'beta'}, {true, false, false}}
print(t2[2][1])

print(#t) -- length of table
print(#t2[2])

local ot = { name='walter', age=40 }
print(ot['name'])
print(ot['age'])

print(ot.name, ot.age)

local ct = { 11, true, name='kim', age=30, 172} -- 1, 2, name, age, 3
print(ct.name, ct.age, ct[1], ct[2], ct[3])
