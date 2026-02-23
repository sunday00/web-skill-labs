local function aliquot_sum(n)
    if n == 1 then
        return 0
    end

    local candies = { 1 }

    for i = 2, math.floor(n / 2) do
        if n % i == 0 then
            table.insert(candies, i)
        end
    end

    local s = 0
    for _, v in ipairs(candies) do
        s = s + v
    end

    return s
end

local function classify(n)
    if n <= 0 then
        error()
    end

    local as = aliquot_sum(n)

    if as == n then
        return 'perfect'
    elseif as < n then
        return 'deficient'
    else
        return 'abundant'
    end
end

return { aliquot_sum = aliquot_sum, classify = classify }
