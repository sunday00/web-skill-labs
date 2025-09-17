local a = nil
print(a)

print(b)

local c
print(c)

if a == nil then
    print("it's nil")
end

if a then
    print('truthy?')
end

if not a then
    print("falsy?")
end

print(not a)
