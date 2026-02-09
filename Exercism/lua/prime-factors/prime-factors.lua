local function sol(input)
    if input == 1 then
        return {}
    elseif input == 2 then
        return { 2 }

    elseif input == 3 then
        return { 3 }
    end

    local ans = {}
    local rem = input

    while (rem > 1) do
        for i = 2, math.ceil(rem / 2) do
            if rem % i == 0 then
                rem = math.floor(rem / i)
                table.insert(ans, i)

                goto continue
            end
        end

        table.insert(ans, rem)
        rem = 1

        :: continue ::
    end

    return ans
end

--local r = sol(4)
--for _, v in ipairs(r) do
--    print(v)
--end

return sol
