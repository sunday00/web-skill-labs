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