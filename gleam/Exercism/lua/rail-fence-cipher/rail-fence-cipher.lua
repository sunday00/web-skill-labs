-- utils

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

-- encode line
local function en_allocate(letters, rail_count)
    local locations = {}
    for i = 1, rail_count do
        table.insert(locations, '')
    end

    local id = 1
    local dir = 'inc'
    for _, letter in ipairs(letters) do
        locations[id] = locations[id] .. letter

        if dir == 'inc' then
            id = id + 1
        end
        if dir == 'dec' then
            id = id - 1
        end

        if dir == 'inc' and id == rail_count then
            dir = 'dec'
        end
        if dir == 'dec' and id == 1 then
            dir = 'inc'
        end
    end

    return locations
end

local function encode(plaintext, rail_count)
    local letters = string.split(plaintext)

    local allocated = en_allocate(letters, rail_count)

    local r = ''
    for _, v in ipairs(allocated) do
        r = r .. v
    end

    return r
end

-- decode line

local function de_findLocation(letters, rail_count)
    local locations = {}
    for i = 1, rail_count do
        table.insert(locations, '')
    end

    local id = 1
    local dir = 'inc'
    for i = 1, #letters do
        locations[id] = locations[id] .. 'i'

        if dir == 'inc' then
            id = id + 1
        end
        if dir == 'dec' then
            id = id - 1
        end

        if dir == 'inc' and id == rail_count then
            dir = 'dec'
        end
        if dir == 'dec' and id == 1 then
            dir = 'inc'
        end
    end

    return locations
end

local function de_allocate(letters, locations)
    local r = {}
    for _ = 1, #locations do
        table.insert(r, {})
    end

    for i, location in ipairs(locations) do
        for _ = 1, #location do
            local letter = table.remove(letters, 1)
            table.insert(r[i], letter)
        end
    end

    return r
end

local function de_reshape(allocated)
    local item = table.remove(allocated[1], 1)
    local r = ''

    local id = 1
    local dir = 'inc'
    while item ~= nil do
        r = r .. item

        if dir == 'inc' then
            id = id + 1
        end
        if dir == 'dec' then
            id = id - 1
        end

        if dir == 'inc' and id == #allocated then
            dir = 'dec'
        end
        if dir == 'dec' and id == 1 then
            dir = 'inc'
        end

        item = table.remove(allocated[id], 1)
    end

    return r
end

local function decode(ciphertext, rail_count)
    local letters = string.split(ciphertext)

    local locations = de_findLocation(letters, rail_count)

    local allocated = de_allocate(letters, locations)

    local r = de_reshape(allocated)

    return r
end

return { encode = encode, decode = decode }
