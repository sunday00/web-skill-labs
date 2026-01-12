local function maximum_value(maximum_weight, items)
    local n = #items
    local dp = {}

    for i = 0, maximum_weight do
        dp[i] = 0
    end

    for i = 1, n do
        local weight = items[i].weight
        local value = items[i].value

        for w = maximum_weight, weight, -1 do
            if dp[w - weight] + value > dp[w] then
                dp[w] = dp[w - weight] + value
            end
        end
    end

    return dp[maximum_weight]
end

local r = maximum_value(750, {
    { weight = 70, value = 135 },
    { weight = 73, value = 139 },
    { weight = 77, value = 149 },
    { weight = 80, value = 150 },
    { weight = 82, value = 156 },
    { weight = 87, value = 163 },
    { weight = 90, value = 173 },
    { weight = 94, value = 184 },
    { weight = 98, value = 192 },
    { weight = 106, value = 201 },
    { weight = 110, value = 210 },
    { weight = 113, value = 214 },
    { weight = 115, value = 221 },
    { weight = 118, value = 229 },
    { weight = 120, value = 240 }
})

return { maximum_value = maximum_value }
