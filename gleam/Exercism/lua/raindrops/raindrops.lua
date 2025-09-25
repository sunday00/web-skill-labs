return function(n)
    local r = ""

    if n % 3 == 0 then
        r = r .. "Pling"
    end

    if n % 5 == 0 then
        r = r .. "Plang"
    end

    if n % 7 == 0 then
        r = r .. "Plong"
    end

    if #r == 0 then
        return tostring(n)
    else
        return r
    end
end
