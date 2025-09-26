local resistor = {
    'black',
    'brown',
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'violet',
    'grey',
    'white',
}

local unitString = {
    "kiloohms",
    "megaohms",
    "gigaohms"
}

local get_color_to_int = function(lcolor, toS)
    local ret = -1

    for i, c in ipairs(resistor) do
        if c == lcolor then
            return i - 1
        end
    end

    if toS then
        return tostring(ret)
    else
        return ret
    end
end

local get_unit_v_and_remains = function(n)
    local v, r = math.floor(n / 3), n % 3

    if v > 0 then
        return unitString[v], string.rep("0", r)
    else
        return "ohms", string.rep("0", r)
    end

end

local decode = function(c1, c2, c3)
    local n = get_color_to_int(c1, true)
    local m = get_color_to_int(c2, true)

    local unit = get_color_to_int(c3, false)

    local v, r = '', ''
    if c2 == 'black' then
        v, r = get_unit_v_and_remains(unit + 1)
    else
        n = n .. m
        v, r = get_unit_v_and_remains(unit)
    end

    return tonumber(n .. r), v
end

return {
    decode = decode
}
