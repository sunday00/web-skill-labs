local sol = {
    format = function(name, number)
        local str = tostring(number)
        local ends = string.sub(str, #str, #str)

        local th = 'th'

        if ends == '1' then
            if string.sub(str, #str - 1, #str) ~= '11' then
                th = 'st'
            end

        elseif ends == '2' then
            if string.sub(str, #str - 1, #str) ~= '12' then
                th = 'nd'
            end

        elseif ends == '3' then
            if string.sub(str, #str - 1, #str) ~= '13' then
                th = 'rd'
            end
        end

        return string.format('%s, you are the %d%s customer we serve today. Thank you!', name, number, th)
    end
}

return sol