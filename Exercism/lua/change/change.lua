function c (amount, values)
    if amount < 0 then
        error("target can't be negative")
    end

    local dp = { [0] = 0 }
    local res = {}

    for i = 1, amount do
        dp[i] = amount + 1
    end

    for i = 1, amount do
        for _, coin in ipairs(values) do
            if i >= coin then
                if dp[i - coin] + 1 < dp[i] then
                    dp[i] = dp[i - coin] + 1
                    res[i] = coin
                end
            end
        end
    end

    if dp[amount] > amount then
        error("can't make target with given coins")
    end

    local result = {}
    local curr = amount
    while curr > 0 do
        table.insert(result, res[curr])
        curr = curr - res[curr]
    end

    return result
end

--local cc = c(27, { 4, 5 })
--for i, v in ipairs(cc) do
--    print(i, v)
--end

return c
