local all_your_base = {}

local ten = nil

local validate = function(n, from_base)
    if n < 0 then
        error('negative digits are not allowed')
    end

    if from_base <= 1 then
        error('invalid input base')
    end

    if n >= from_base then
        error('digit out of range')
    end
end

local outputValidate = function(n)
    if n <= 1 then
        error('invalid output base')
    end
end

local to = function(n)
    outputValidate(n)

    local rem = ten
    local res = {}

    while (rem >= n) do
        local ansRem = rem % n
        local ans = (rem - ansRem) / n

        table.insert(res, 1, ansRem)
        rem = ans
    end

    table.insert(res, 1, rem)

    return res
end

all_your_base.convert = function(from_digits, from_base)
    ten = 0

    local pow = 0
    for i = #from_digits, 1, -1 do
        local dit = from_digits[i]

        validate(dit, from_base)

        ten = ten + (dit * math.pow(from_base, pow))

        pow = pow + 1
    end

    return { to = to }
end

--local ans = all_your_base.convert({ 5 }, 10).to(2)
--print(ans[1], ans[2], ans[3], ans[4], ans[5])

return all_your_base
