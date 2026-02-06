local function keep(xs, pred)
    local r = {}

    for _, v in ipairs(xs) do
        if pred(v) then
            table.insert(r, v)
        end
    end

    return r
end

local function discard(xs, pred)
    local r = {}

    for _, v in ipairs(xs) do
        if not pred(v) then
            table.insert(r, v)
        end
    end

    return r
end

--local r = keep({ 'apple', 'zebra', 'banana', 'zombies', 'cherimoya', 'zealot' }, function(s)
--    return string.sub(s, 1, 1) == "z"
--end)
--print(r[1], r[2], r[3])

return { keep = keep, discard = discard }
