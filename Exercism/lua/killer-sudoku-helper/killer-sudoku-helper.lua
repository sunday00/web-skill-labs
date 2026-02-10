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

local function combinations(sum, size, exclude)
    local r = {}

    local function insertable (x)
        local sp = string.split(tostring(x))

        for i = 1, #sp - 1 do
            for j = i + 1, #sp do
                if tonumber(sp[i]) > tonumber(sp[j]) or tonumber(sp[i]) == tonumber(sp[j]) or tonumber(sp[i]) == 0 or tonumber(sp[j]) == 0 then
                    return false
                end
            end

            if exclude ~= nil then
                for _, ex in ipairs(exclude) do
                    if ex == tonumber(sp[i]) or ex == tonumber(sp[i + 1]) then
                        return false
                    end
                end
            end
        end

        local ssum = 0
        local sg = {}

        for _, v in ipairs(sp) do
            ssum = ssum + tonumber(v)
            table.insert(sg, tonumber(v))
        end

        if ssum == sum then
            return sg
        end

        return false
    end

    local function main ()
        local strMin = ''
        local strMax = ''

        for i = 1, size do
            strMin = strMin .. tostring(i)
        end

        for i = 9, 10 - size, -1 do
            strMax = tostring(i) .. strMax
        end

        for i = tonumber(strMin), tonumber(strMax) do
            local isInsertable = insertable(i)

            if isInsertable then
                table.insert(r, isInsertable)
            end
        end
    end

    main()

    return r
end

--for _, v in ipairs(combinations(45, 9)) do
--    print(v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9])
--end

--for _, v in ipairs(combinations(15, 3)) do
--    print(v[1], v[2], v[3])
--end

return { combinations = combinations }
