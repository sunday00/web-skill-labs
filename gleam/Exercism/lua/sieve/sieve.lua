local function s(n)
    local candi = {}

    for i = 2, n do
        table.insert(candi, i)
    end

    local r = {}
    while (#candi > 0) do
        local c = table.remove(candi, 1)
        table.insert(r, c)

        local nCandi = {}
        for _, v in ipairs(candi) do
            if v % c ~= 0 then

                table.insert(nCandi, v)
            end
        end

        candi = nCandi
    end

    return r
end

--local r = s(100)
--for _, v in ipairs(r) do
--    print(v)
--end

return s