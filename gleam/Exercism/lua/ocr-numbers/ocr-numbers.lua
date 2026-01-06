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

function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end

local function convertLetter(s)
    local nn = {
        '     |  |',
        ' _  _||_ ',
        ' _  _| _|',
        '   |_|  |',
        ' _ |_  _|',
        ' _ |_ |_|',
        ' _   |  |',
        ' _ |_||_|',
        ' _ |_| _|',
        ' _ | ||_|'
    }

    for i, v in ipairs(nn) do
        if v == s then
            return i
        end
    end
end

local function removeLineSeparator (lines)
    local r = {}

    for i, v in ipairs(lines) do
        if i % 4 ~= 0 then
            table.insert(r, v)
        end
    end

    return r
end

local function reshape (lines)
    local r = ''

    while #lines > 0 do
        local top = string.sub(lines[1], 1, 3)
        local middle = string.sub(lines[2], 1, 3)
        local bottom = string.sub(lines[3], 1, 3)

        lines[1] = string.sub(lines[1], 4, #lines[1])
        lines[2] = string.sub(lines[2], 4, #lines[2])
        lines[3] = string.sub(lines[3], 4, #lines[3])

        local hasComma = false

        if #lines[1] == 0 then
            table.remove(lines, 1)
            table.remove(lines, 1)
            table.remove(lines, 1)

            if #lines > 0 then
                hasComma = true
            end
        end

        local letter = top .. middle .. bottom

        if #letter ~= 9 then
            error()
        end

        local no = convertLetter(letter)

        if no == 10 then
            no = 0
        end

        if no == nil then
            no = '?'
        end

        r = r .. no

        if hasComma then
            r = r .. ','
        end
    end

    return r
end

local o = {
    convert = function(s)
        local lines = string.split(s, '\n')
        lines = removeLineSeparator(lines)

        local reshaped = reshape(lines)

        return reshaped
    end
}

--local r = o.convert(
--        '   \n' ..
--                '| |\n' ..
--                '| |\n' ..
--                '   '
--)
--
--print(r)

return o
