local function reduce(a)
end

local function add(a, b)
    return {
        a[1] * b[2] + b[1] * a[2],
        a[2] * b[2]
    }
end

local function subtract(a, b)
    return {
        a[1] * b[2] - b[1] * a[2],
        a[2] * b[2]
    }
end

local function multiply(a, b)
end

local function divide(a, b)
end

local function abs(a)
end

local function exp_rational(a, p)
end

local function exp_real(p, a)
end

local r = add({ 1, 2 }, { -2, 3 })
print(r[1], r[2])

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
