local x = 1

local function foo(a, b)
    local n = a + b
    return n - 1
end

local a = {11, 22, 33, 44}
local b = a

a[1] = -10
print(a[1])
print(b[1])

b[2] = 100
print(a[2])
print(b[2])

local function showTable(t)
    io.write("{ ")
    for k, v in pairs(t) do
        io.write('"'..k..'": '..v..', ')
    end
    io.write("}\n")
end

local t = {11, 22, 33, x = 44}
showTable(t)

local function updateOneToZero(t)
    t[1] = 0
end

local function updateYToN(t, n)
    t.y = n
end

updateOneToZero(t)
showTable(t)

updateYToN(t, 11)
showTable(t)
