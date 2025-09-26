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

local get_color_to_int = function(lcolor)
    for i, c in ipairs(resistor) do
        if c == lcolor then
            return tostring(i - 1)
        end
    end
end

local value = function(colors)
    local res = ""

    for i = 1, #colors do
        if i > 2 then
            break
        end
        res = res .. get_color_to_int(string.lower(colors[i]))
    end

    return tonumber(res)
end

return {
    value = value
}
