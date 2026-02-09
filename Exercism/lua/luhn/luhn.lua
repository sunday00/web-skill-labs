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

local sol = {
    valid = function(s)
        local ts = string.replace(s, ' ', '')

        if #ts <= 1 then
            return false
        end

        ts = string.reverse(ts)
        ss = string.split(ts)
        local ns = {}

        for i, v in ipairs(ss) do
            if tonumber(v) == nil then
                return false
            end

            if i % 2 ~= 0 then
                table.insert(ns, tonumber(v))

            else
                local n = tonumber(v) * 2
                if n > 9 then
                    n = n - 9
                end

                table.insert(ns, n)
            end
        end

        local sum = 0
        for _, v in ipairs(ns) do
            sum = sum + v
        end

        return sum % 10 == 0
    end
}

return sol
