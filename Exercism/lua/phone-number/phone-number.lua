local function clean(s)
    local n = string.gsub(s, '%D', '')

    if #n < 10 or #n > 11 then
        error()
    end

    if #n == 11 then
        if string.sub(n, 1, 1) ~= '1' then
            error()
        else
            n = string.sub(n, 2, #n)
        end
    end

    if #n == 10 then
        if string.sub(n, 1, 1) == '0' or string.sub(n, 1, 1) == '1' then
            error()
        end

        if string.sub(n, 4, 4) == '0' or string.sub(n, 4, 4) == '1' then
            error()
        end
    end

    return n
end

return { clean = clean }
