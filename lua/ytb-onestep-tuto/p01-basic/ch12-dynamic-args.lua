local function showTable(t)
    io.write("{ ")
    for k, v in pairs(t) do
        io.write('"'..k..'": '..v..', ')
    end
    io.write("}\n")
end

local function foo(...)
    --print(type(arg))
    --print(type(...))
    --showTable(arg)
    --print(...)

    -- https://stackoverflow.com/questions/48273776/vararg-function-parameters-dont-work-with-arg-variable
    local t = {...}
    t.n = select('#', ...)
    showTable(t)
end

foo(1, 2, 3, 4, 5, 6)

local function calc(s, ...)
    local t = {...}
    t.n = select('#', ...)

    local ret

    if s == 'add' then
        ret = 0
        for _, v in ipairs(t) do
            ret = ret + v
        end
    elseif s == 'mul' then
        ret = 1
        for _, v in ipairs(t) do
            ret = ret * v
        end
    end

    return ret
end

print(calc('add', 1, 2, 3))

print "======-=-=-=-=-=-========="

local function coprime(a, b)
    local min = math.min(a, b)

    local ret = {}

    for i = min, 2, - 1 do
        if a % i == 0 and b % i == 0 then
            table.insert(ret, i)
        end
    end

    table.insert(ret, 1)

    return ret
end

showTable(coprime(4, 6))
showTable(coprime(4, 12))
showTable(coprime(6, 12))
showTable(coprime(12, 6))
showTable(coprime(12, 30))

print "======-=-=-=-=-=-========="

local function deFoo(a)
    if a == nil then
        a = 10
    end

    return a
end

print(deFoo())
print(deFoo(8))

local function deFoo2(a)
    a = a or 10

    return a
end

print(deFoo2())
print(deFoo2(8))
