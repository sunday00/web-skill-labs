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
    local maxLenIdx = 0
    for i, ho in ipairs(horizontals) do
        if maxLen <= #string.replace(ho, '|', '') then
            maxLen = #string.replace(ho, '|', '')
            maxLenIdx = i
        end
    end

    for i = 1, maxLen do
        local v = ''
        for y, ho in ipairs(horizontals) do
            local letter = string.sub(ho, i, i)

            if y > maxLenIdx and i > maxLenIdx and (letter == '' or letter == '|') then
                goto continue
            end

            if (letter == '' or letter == '|') then
                letter = ' '
            end

            v = v .. letter

            :: continue ::
        end
        table.insert(verticals, v)
        print('"' .. v .. '"')
    end

    return string.join(verticals, '\n')
end

local x = transpose("Chor. Two households, both alike in dignity,\n" ..
        "In fair Verona, where we lay our scene,\n" ..
        "From ancient grudge break to new mutiny,\n" ..
        "Where civil blood makes civil hands unclean.\n" ..
        "From forth the fatal loins of these two foes\n" ..
        "A pair of star-cross'd lovers take their life;\n" ..
        "Whose misadventur'd piteous overthrows\n" ..
        "Doth with their death bury their parents' strife.\n" ..
        "The fearful passage of their death-mark'd love,\n" ..
        "And the continuance of their parents' rage,\n" ..
        "Which, but their children's end, naught could remove,\n" ..
        "Is now the two hours' traffic of our stage;\n" ..
        "The which if you with patient ears attend,\n" ..
        "What here shall miss, our toil shall strive to mend.")

--table.debug(x)
--print(x)

return transpose