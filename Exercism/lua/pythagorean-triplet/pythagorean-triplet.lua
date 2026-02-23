return function(sum)
    local res = {}

    for a = 1, math.floor(sum / 3) do
        local numerator = sum ^ 2 - 2 * sum * a
        local denominator = 2 * (sum - a)

        if numerator % denominator == 0 then
            local b = numerator / denominator
            if a < b then
                local c = sum - a - b
                table.insert(res, { a, b, c })
            end
        end
    end

    return res
end
