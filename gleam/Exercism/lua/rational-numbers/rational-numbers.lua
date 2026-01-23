local function negaConv(a)
    local r1 = a[1]
    local r2 = a[2]

    if (r2 < 0 and r1 >= 0) or (r1 < 0 and r2 < 0) then
        r1 = -1 * r1
        r2 = -1 * r2
    end

    if r1 == 0 then
        r2 = 1
    end

    return { r1, r2 }
end
local function reduce(a)
    local r1 = a[1]
    local r2 = a[2]

    local rr = 1
    for i = 2, math.min(math.abs(r1), math.abs(r2)) do
        if r1 % i == 0 and r2 % i == 0 then
            rr = i
        end
    end

    if rr ~= 1 then
        return reduce({ r1 / rr, r2 / rr })
    end

    return negaConv({ r1 / rr, r2 / rr })
end

local function add(a, b)
    local r2 = a[2] * b[2]
    if a[2] == b[2] then
        r2 = a[2]
    end

    local r1 = a[1] * b[2] + b[1] * a[2]
    if r1 == 0 then
        r2 = 1
    end

    return { r1, r2 }
end

local function subtract(a, b)
    local r2 = a[2] * b[2]
    if a[2] == b[2] then
        r2 = a[2]
    end

    local r1 = a[1] * b[2] - b[1] * a[2]

    return negaConv({ r1, r2 })
end

local function multiply(a, b)
    local r1 = a[1] * b[1]
    local r2 = a[2] * b[2]

    local r = reduce({ r1, r2 })

    return negaConv(r)
end

local function divide(a, b)
    local r1 = a[1] * b[2]
    local r2 = b[1] * a[2]

    return negaConv({ r1, r2 })
end

local function abs(a)
    return reduce({ math.abs(a[1]), math.abs(a[2]) })
end

local function exp_rational(a, p)
    local r1 = a[1] ^ math.abs(p)
    local r2 = a[2] ^ math.abs(p)

    if p < 0 then
        local t = r1
        r1 = r2
        r2 = t
    end

    --if p < 0 and p % 2 ~= 0 then
    --    r1 = r1 * -1
    --end



    return negaConv({ math.tointeger(r1), math.tointeger(r2) })
end

local function exp_real(p, a)
    return math.pow(p, a[1] / a[2])
end

return {
    add = add,
    subtract = subtract,
    multiply = multiply,
    divide = divide,
    abs = abs,
    exp_rational = exp_rational,
    exp_real = exp_real,
    reduce = reduce
}
