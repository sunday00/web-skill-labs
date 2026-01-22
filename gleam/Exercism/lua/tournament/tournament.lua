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

function string.rightPad(str, len, char)
    char = char or " "
    local padLen = len - #tostring(str)
    if padLen <= 0 then
        return str
    end
    return tostring(str) .. string.rep(char, padLen)
end

function string.leftPad(str, len, char)
    char = char or " "
    local padLen = len - #tostring(str)
    if padLen <= 0 then
        return str
    end
    return string.rep(char, padLen) .. tostring(str)
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
            label = home,
            mp = 0,
            win = 0,
            loss = 0,
            draw = 0,
            point = 0
        }

        recordR(home, away, res, reverse)

        return
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

        return
    end
end

local function tournament (results)
    r = {}

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

    local localR = {}
    for _, v in pairs(r) do
        table.insert(localR, v)
    end

    table.sort(localR, function(a, b)
        if a.point == b.point then
            return a.label < b.label
        end

        return a.point > b.point
    end)

    local res = {
        'Team                           | MP |  W |  D |  L |  P'
    }

    for _, v in ipairs(localR) do
        local label = string.rightPad(v.label, 30) .. ' | '
        local mp = string.leftPad(tostring(v.mp), 2) .. ' | '
        local w = string.leftPad(tostring(v.win), 2) .. ' | '
        local d = string.leftPad(tostring(v.draw), 2) .. ' | '
        local l = string.leftPad(tostring(v.loss), 2) .. ' | '
        local p = string.leftPad(tostring(v.point), 2)

        table.insert(res, label .. mp .. w .. d .. l .. p)
    end

    return res
end

local res = tournament({
    'Allegoric Alaskans;Blithering Badgers;win',
    'Devastating Donkeys;Courageous Californians;draw',
    'Devastating Donkeys;Allegoric Alaskans;win',
    'Courageous Californians;Blithering Badgers;loss',
    'Blithering Badgers;Devastating Donkeys;loss',
    'Allegoric Alaskans;Courageous Californians;win'
})

--for _, v in ipairs(res) do
--    print(v)
--end

return tournament