function act (s)
    local s = string.lower(s)
    s = string.gsub(s, '-', '')
    s = string.gsub(s, ' ', '')
    local t = {}
    for i = 1, #s do
        local ii = string.sub(s, i, i)

        if t[ii] ~= nil then
            return false
        end

        if t[ii] == nil then
            t[ii] = 1
        end
    end

    return true
end

return act