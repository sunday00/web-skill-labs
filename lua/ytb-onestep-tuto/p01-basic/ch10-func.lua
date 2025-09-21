local function foo ()
    print('hello')
end

foo()

local function countDownFrom(n)
    for k = n , 0 , -1 do
        io.write(k..', ')
    end
    print("")
end

countDownFrom(8)

local function p2(x)
    return x  ^ 2
end

print(p2(8))

local function pn(x, y)
    return x ^ y
end

print(pn(3, 4))

local function bar(a, b, c)
    print(a, b, c)
end

local r = bar(1, 2, 3)
print(r)

bar(1, 2)

print("----------")

local function mulR ()
    return 1, 2
end

local x, y = mulR()
print(x, y)

local a = mulR()
print(a)

local i, j, k = mulR()
print(i, j, k )

print("------------")

local t = {11, 22, 45, 89}

local function avg(t)
    local sum = 0
    for _, v in ipairs(t) do
        sum = v + sum
    end

    return sum / #t
end

print(avg(t))
print(avg({11, 22, 45, 89}))
print(avg{11, 22, 45, 89})

print("------------")

local function stringArg(n)
    print("hello, "..n..".")
end

stringArg "master"

print("------------")

local function canICallBack(x)
    print(x())
end

canICallBack(function() return "abc" end)

local ft = { canICallBack, function() print("anonymous") end}

ft[2]()

print("------------")

local xx = "hello"

local function testClosure()
    print(xx)
end

testClosure()
