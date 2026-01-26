function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end

local kidsId = {
    Alice = 1,
    Bob = 3,
    Charlie = 5,
    David = 7,
    Eve = 9,
    Fred = 11,
    Ginny = 13,
    Harriet = 15,
    Ileana = 17,
    Joseph = 19,
    Kincaid = 21,
    Larry = 23,
}

local plantId = {
    C = 'clover',
    V = 'violets',
    G = 'grass',
    R = 'radishes',
}

local function g (s)
    local garden = string.replace(s, '\n', '')

    return {
        plants = function(kid)
            local id = kidsId[kid]
            local plantCodes = {
                string.sub(garden, id, id),
                string.sub(garden, id + 1, id + 1),
                string.sub(garden, (#garden / 2) + id, (#garden / 2) + id),
                string.sub(garden, (#garden / 2) + id + 1, (#garden / 2) + id + 1)
            }

            local plantsNames = {}
            for _, v in ipairs(plantCodes) do
                table.insert(plantsNames, plantId[v])
            end

            return plantsNames
        end
    }
end

--local r = g('VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV').plants('Ginny')
--print(r[1], r[2], r[3], r[4])

return g
