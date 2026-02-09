local function sol(input)
    local ans = {}
    local rem = input

    while (rem > 0) do
        for i = 2, math.floor(rem / 2) do
            if rem % i == 0 then
                rem = rem / i
                table.insert(ans, i)

                goto continue
            end
        end

        goto breakOut

        :: continue ::
    end

    :: breakOut ::

    return ans
end

local r = sol(625)
for _, v in ipairs(r) do
    print(v)
end

return sol
