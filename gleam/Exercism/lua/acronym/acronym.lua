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

local function ac (s)
    local splits = string.split(s)

    local prevIsCapital = false
    local prevIsBlank = false
    local res = ''
    for _, letter in ipairs(splits) do
        if string.byte(letter) >= 65 and string.byte(letter) <= 90 then
            if not prevIsCapital then
                res = res .. letter
                prevIsCapital = true
                prevIsBlank = false

                goto continue
            end

            prevIsCapital = true
            prevIsBlank = false
            goto continue
        end

        if string.byte(letter) >= 97 and string.byte(letter) <= 122 then
            if prevIsBlank then
                res = res .. string.upper(letter)
                prevIsCapital = false
                prevIsBlank = false

                goto continue
            end

            prevIsCapital = false
            prevIsBlank = false
            goto continue
        end

        if letter == ' ' then
            prevIsCapital = false
            prevIsBlank = true

            goto continue
        end

        prevIsCapital = false
        prevIsBlank = false

        :: continue ::
    end

    return res
end

--local r = ac('PHP: Hypertext Processor')
--print(r)

return ac
