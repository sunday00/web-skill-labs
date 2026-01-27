local function reducer(acc, cur)
    if #cur == 0 then
        return acc
    end

    local f = table.remove(cur, 1)
    if type(f) ~= 'table' then
        table.insert(acc, f)

        return reducer(acc, cur)
    else
        for i, v in ipairs(f) do
            table.insert(cur, i, v)
        end

        return reducer(acc, cur)
    end
end

local function flatten(input)
    return reducer({}, input)
end

local r = flatten({ 0, 2, { { 2, 3 }, 8, 100, 4, { { { 50 } } } }, -2 })

--for _, v in ipairs(r) do
--    print(v)
--end

return flatten
