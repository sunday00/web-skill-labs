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

function string.get(s, i) 
    return string.sub(s, i, i)
end

function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end