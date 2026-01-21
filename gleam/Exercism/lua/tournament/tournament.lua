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

local r = {}

local function recordR(home, away, res, reverse)
    if r[home] ~= nil then
        r[home].mp = r[home].mp + 1
        r[home][res] = r[home][res] + 1
        if res == 'win' then
            r[home].point = r[home].point + 3
        end
        if res == 'draw' then
            r[home].point = r[home].point + 1
        end

    else

        r[home] = {
            mp = 1,
            win = 0,
            loss = 0,
            draw = 0,
            point = 0
        }

        recordR(home, away, res, reverse)
    end

    if reverse then
        return

    else

        local unRes = 'draw'
        if res == 'win' then
            unRes = 'loss'

        elseif res == 'loss' then
            unRes = 'win'
        end

        recordR(away, home, unRes, true)
    end
end

local function tournament (results)
    for _, v in ipairs(results) do
        local row = string.split(v, ';')

        if #row ~= 3 then
            goto continue
        end

        local a = row[1]
        local b = row[2]
        local m = row[3]

        if m ~= 'win' and m ~= 'loss' and m ~= 'draw' then
            goto continue
        end

        recordR(a, b, m, false)

        :: continue ::
    end

    for i, v in pairs(r) do
        print(i, '===')

        for ii, vv in pairs(v) do
            print('  ', ii, vv)
        end
    end

    return r
end

tournament({
    'Courageous Californians;Devastating Donkeys;win',
    'Allegoric Alaskans;Blithering Badgers;win',
    'Devastating Donkeys;Allegoric Alaskans;loss',
    'Courageous Californians;Blithering Badgers;win',
    'Blithering Badgers;Devastating Donkeys;draw',
    'Allegoric Alaskans;Courageous Californians;draw'
})

return tournament