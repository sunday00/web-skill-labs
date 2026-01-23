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

local function checkRule1 (phrase)
    for _, v in ipairs({ 'a', 'e', 'i', 'o', 'u', 'xr', 'yt' }) do
        if string.find(phrase, v) == 1 then
            return phrase .. 'ay'
        end
    end

    return false
end

local function checkRule2 (phrase)
    local firstVowel = string.find(phrase:lower(), "[aeiou]")

    if firstVowel == 1 then
        return false
    end

    if firstVowel == nil then
        return false
    end

    local head = string.sub(phrase, 1, firstVowel - 1)
    local tail = string.sub(phrase, firstVowel, #phrase)

    return tail .. head .. 'ay'
end

local function checkRule3 (phrase)
    local firstQu = phrase:find('^[^aeiou]*qu')

    if firstQu == nil then
        return false
    end

    local match = phrase:match('^[^aeiou]*qu')

    local head = string.sub(phrase, 1, firstQu - 1) .. match
    local tail = string.sub(phrase, firstQu + (#match), #phrase)

    return tail .. head .. 'ay'
end

local function checkRule4 (phrase)
    local firstY = phrase:find('y')

    if firstY == nil then
        return false
    end

    local head = string.sub(phrase, 1, firstY - 1)
    local tail = string.sub(phrase, firstY, #phrase)

    return tail .. head .. 'ay'
end

local function processss(phrase)
    local res1 = checkRule1(phrase)

    if res1 then
        return res1
    end

    local res3 = checkRule3(phrase)

    if res3 then
        return res3
    end

    local res2 = checkRule2(phrase)

    if res2 then
        return res2
    end

    local res4 = checkRule4(phrase)

    if res4 then
        return res4
    end
end

function translate (phrase)
    local p = string.split(phrase, ' ')
    local res = {}

    for _, v in ipairs(p) do
        table.insert(res, processss(v))
    end

    return string.join(res, ' ')
end

return translate
