local triangle = {}

local validate = function(a, b, c)
    if a <= 0 or b <= 0 or c <= 0 then
        error('Input Error')
    end

    if a + b <= c or b + c <= a or c + a <= b then
        error('Input Error')
    end
end

function triangle.kind(a, b, c)
    validate(a, b, c)

    if a == b and b == c and c == a then
        return 'equilateral'
    end

    if a == b or b == c or c == a then
        return 'isosceles'
    end

    return 'scalene'
end

return triangle
