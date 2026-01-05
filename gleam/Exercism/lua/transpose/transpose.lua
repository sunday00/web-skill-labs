function string.split (inputstr, sep)
    if sep == nil then
        sep = '.'
    else
        sep = '([^' .. sep .. ']+)'
    end

    local t = {}
    for str in string.gmatch(inputstr, sep)
    do
        table.insert(t, str)
    end

    return t
end

function string.join(t, sep)
    local r = ''

    for _, val in ipairs(t) do
        if r ~= '' then
            r = r .. sep
        end

        r = r .. val
    end

    return r
end

function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end

function table.debug(t)
    for i, el in pairs(t) do
        print(i, el)
    end
end

function transpose (s)
    local verticals = {}

    local horizontals = string.split(string.replace(s, '\n', '|\n'), '\n')

    local maxLen = 0
    for _, ho in ipairs(horizontals) do
        if maxLen <= #string.replace(ho, '|', '') then
            maxLen = #string.replace(ho, '|', '')
        end
    end

    for i = 1, maxLen do
        local v = ''
        local lastVI = 1
        for y = 1, #horizontals do
            if i <= #horizontals[y] then
                lastVI = y
            end
        end

        for y, ho in ipairs(horizontals) do
            local letter = string.sub(ho, i, i)

            if (letter == '' or letter == '|') and y <= lastVI then
                v = v .. ' '

            else

                v = v .. letter
            end
        end
        table.insert(verticals, v)
        --print('"' .. v .. '"')
    end

    return string.join(verticals, '\n')
end

--local x = transpose('T\n' ..
--        'EE\n' ..
--        'AAA\n' ..
--        'SSSS\n' ..
--        'EEEEE\n' ..
--        'RRRRRR')

--table.debug(x)
--print(x)

return transpose