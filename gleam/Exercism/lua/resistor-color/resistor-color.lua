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

local color_code = function(color)

    local lcolor = string.lower(color)

    for i, c in ipairs(resistor) do
        if c == lcolor then
            return i - 1
        end
    end
end

return {
    color_code = color_code
}


