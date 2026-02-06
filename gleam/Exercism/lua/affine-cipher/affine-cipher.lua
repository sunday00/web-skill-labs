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

local list = {
    a = 1,
    b = 2,
    c = 3,
    d = 4,
    e = 5,
    f = 6,
    g = 7,
    h = 8,
    i = 9,
    j = 10,

    k = 11,
    l = 12,
    m = 13,
    n = 14,
    o = 15,
    p = 16,
    q = 17,
    r = 18,
    s = 19,
    t = 20,

    u = 21,
    v = 22,
    w = 23,
    x = 24,
    y = 25,
    z = 26,
}

local function flipTable(t)
    local r = {}
    for key, _ in pairs(list) do
        table.insert(r, key)
    end

    return r
end

local rList = flipTable()
table.sort(rList)

local function aToI (a)
    return list[a] - 1
end

local function iToA (i)
    return rList[i + 1]
end

local function isACop (x)
    for i = 2, 13 do
        if x % i == 0 and 26 % i == 0 then
            return false
        end
    end

    return true
end

local function encLetter (key, l)
    if tonumber(l) ~= nil then
        return l
    end

    if l:match('[a-z]') then
        return iToA((key.a * aToI(l) + key.b) % 26) .. ''
    end

    return ''
end

local function encode(phrase, key)
    if not isACop(key.a) then
        error('a and m must be coprime.')
    end

    local lower = string.split(phrase:lower())

    local enc = ''
    for _, v in ipairs(lower) do
        local el = encLetter(key, v)
        if #enc % 6 == 0 and el ~= '' then
            enc = enc .. ' ' .. el
        else
            enc = enc .. el
        end
    end

    return enc:sub(2, #enc)
end

local function getMMI(n)
    for i = 1, 26 do
        if (n * i) % 26 == 1 then
            return i
        end
    end
end

local function decodeLetter(key, l)
    if tonumber(l) ~= nil then
        return l
    end

    local mmi = getMMI(key.a)
    return iToA(mmi * (aToI(l) - key.b) % 26)
end

local function decode(phrase, key)
    if not isACop(key.a) then
        error('a and m must be coprime.')
    end

    local plain = string.replace(phrase, ' ', '')
    plain = string.split(plain)

    local dec = ''
    for _, v in ipairs(plain) do
        local dl = decodeLetter(key, v)
        dec = dec .. dl
    end

    return dec
end

--print(encode('This is a test.', { a = 6, b = 17 }))

return { encode = encode, decode = decode }
