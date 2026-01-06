local function isFlower(garden, x, y)
    if garden[y] == nil then
        return 0
    end

    if string.sub(garden[y], x, x) == nil then
        return 0
    end

    if string.sub(garden[y], x, x) == '*' then
        return 1
    end

    return 0
end

local function annotate(garden)
    local res = {}

    for y, r in ipairs(garden) do
        local newRow = ''

        for i = 1, #r do
            local ch = string.sub(r, i, i)
            if ch == '*' then
                newRow = newRow .. '*'
                goto continue
            end

            if ch == ' ' then
                local cnt = 0

                cnt = cnt + isFlower(garden, i - 1, y - 1)
                cnt = cnt + isFlower(garden, i, y - 1)
                cnt = cnt + isFlower(garden, i + 1, y - 1)

                cnt = cnt + isFlower(garden, i - 1, y)
                cnt = cnt + isFlower(garden, i + 1, y)

                cnt = cnt + isFlower(garden, i - 1, y + 1)
                cnt = cnt + isFlower(garden, i, y + 1)
                cnt = cnt + isFlower(garden, i + 1, y + 1)

                if cnt == 0 then
                    newRow = newRow .. ' '
                    goto continue
                end

                newRow = newRow .. cnt
            end

            :: continue ::
        end

        table.insert(res, newRow)
    end

    return res
end

return { annotate = annotate }
