local dic = {
    [1] = 'eggs',
    [2] = 'peanuts',
    [4] = 'shellfish',
    [8] = 'strawberries',
    [16] = 'tomatoes',
    [32] = 'chocolate',
    [64] = 'pollen',
    [128] = 'cats',
}

local function getRem (score)
    if score > 255 then
        local rem = 128

        while (rem * 2 < score) do
            rem = rem * 2
        end

        return score - rem
    end

    return score
end

local function list(score)
    local rem = getRem(score)
    local res = {}

    local id = 128
    while (id >= 1) do
        if rem >= id then
            table.insert(res, 1, dic[id])
            rem = rem - id
        end

        id = id / 2
    end

    return res
end

local function allergic_to(score, which)
    local l = list(score)

    for _, v in ipairs(l) do
        if v == which then
            return true
        end
    end

    return false
end

--local ll = list(0)
--print(ll[1], ll[2], ll[3], ll[4], ll[5])

return { list = list, allergic_to = allergic_to }
