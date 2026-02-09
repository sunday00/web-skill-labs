local function split (inputstr, sep)
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

local map = {
    { s1 = 'I', s5 = 'V', sX = 'X' },
    { s1 = 'X', s5 = 'L', sX = 'C' },
    { s1 = 'C', s5 = 'D', sX = 'M' },
    { s1 = 'M', s5 = nil, sX = nil },
}

local function conv(n, m)
    local nn = tonumber(n)

    if nn < 4 then
        return string.rep(m.s1, nn)
    elseif nn == 4 then
        return m.s1 .. m.s5
    elseif nn == 5 then
        return m.s5
    elseif nn < 9 then
        return m.s5 .. tostring(string.rep(m.s1, nn - 5))
    else
        return m.s1 .. m.sX
    end
end

local r = {
    to_roman = function(n)
        local r = split(tostring(n):reverse())

        local ans = ''

        for i, v in ipairs(r) do
            ans = conv(v, map[i]) .. ans
        end

        return ans
    end
}

--print(r.to_roman(1024))

return r