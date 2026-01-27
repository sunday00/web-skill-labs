local function defineSquare(n)
    local c, r = math.ceil(math.sqrt(n)), math.floor(math.sqrt(n))

    if c * r < n then
        r = r + 1
    end

    return c, r
end

local function generateSquare(txt, c)
    local square = {}

    for i = 1, #txt, c do
        table.insert(square, txt:sub(i, i + c - 1))
    end

    return square
end

local function reformToVerticals (square, r)
    local encoded = {}

    for i = 1, #(square[1]) do
        local row = ''

        for _, v in ipairs(square) do
            row = row .. string.sub(v, i, i)
        end

        if #row < r then
            row = row .. ' '
        end

        table.insert(encoded, row)
    end

    return encoded
end

local cs = {
    ciphertext = function(plaintext)
        local plain = string.gsub(string.lower(plaintext), "[^%w]", "")

        if #plain == 0 then
            return ''
        end

        local c, r = defineSquare(#plain)
        local square = generateSquare(plain, c)
        local encoded = reformToVerticals(square, r)

        local res = ''

        for _, v in ipairs(encoded) do
            if #res == 0 then
                res = v
            else
                res = res .. ' ' .. v
            end
        end

        return res
    end
}

--local r = cs.ciphertext('clu hlt io ')
--print("'" .. r .. "'")

return cs