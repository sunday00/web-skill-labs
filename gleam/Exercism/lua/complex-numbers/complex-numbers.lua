local Complex
local mt = { __index = {} }

function mt.__add(c1, c2)
    return Complex(c1.r + c2.r, c1.i + c2.i)
end

function mt.__sub(c1, c2)
    return Complex(c1.r - c2.r, c1.i - c2.i)
end

function mt.__mul(c1, c2)
    return Complex(c1.r * c2.r - c1.i * c2.i, c1.r * c2.i + c1.i * c2.r)
end

function mt.__div(c1, c2)
    local x = c2.r ^ 2 + c2.i ^ 2

    if x == 0 then
        return 0
    end

    local newR = (c1.r * c2.r + c1.i * c2.i) / x
    local newI = (c1.i * c2.r - c1.r * c2.i) / x

    return Complex(newR, newI)
end

function mt.__eq(c1, c2)
    return math.abs(c1.r - c2.r) < 0.00000000001 and math.abs(c1.i - c2.i) < 0.00000000001
end

Complex = function(r, i)
    local c = {
        r = r,
        i = i or 0,

        abs = function()
            return math.sqrt(r ^ 2 + i ^ 2)
        end,

        conj = function()
            return Complex(r, -i)
        end,

        exp = function()
            return Complex(math.exp(r) * math.cos(i), math.exp(r) * math.sin(i))
        end,
    }

    return setmetatable(c, mt)
end

return Complex
