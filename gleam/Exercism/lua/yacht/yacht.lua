local yacht = {}

yacht.pickN = function(dice, pick)
    local cnt = 0
    for _, v in ipairs(dice) do
        if v == pick then
            cnt = cnt + 1
        end
    end

    return cnt * pick
end

yacht.fullHouse = function(dice)
    local a = {}
    local b = {}

    for i, v in ipairs(dice) do
        if i == 1 then
            table.insert(a, v)
        else
            if v == a[1] then
                table.insert(a, v)
            elseif v == b[1] or #b == 0 then
                table.insert(b, v)

            end
        end
    end

    if (#a == 2 and #b == 3) then
        return a[1] * 2 + b[1] * 3
    end

    if (#a == 3 and #b == 2) then
        return a[1] * 3 + b[1] * 2
    end

    return 0
end

yacht.fourKind = function(dice)
    local a = {}
    local b = {}

    for i, v in ipairs(dice) do
        if i == 1 then
            table.insert(a, v)
        else
            if v == a[1] then
                table.insert(a, v)
            elseif v == b[1] or #b == 0 then
                table.insert(b, v)

            end
        end
    end

    if (#a >= 4) then
        return a[1] * 4
    end

    if (#b >= 4) then
        return b[1] * 4
    end

    return 0
end

yacht.straight = function(dice, category)
    table.sort(dice)

    local l = ''
    for _, v in ipairs(dice) do
        l = l .. tostring(v)
    end

    if l == '12345' and category == 'little' then
        return 30
    end
    if l == '23456' and category == 'big' then
        return 30
    end

    return 0
end

yacht.choice = function(dice)
    local n = 0
    for _, v in ipairs(dice) do
        n = n + v
    end

    return n
end

yacht.yacht = function(dice)
    local n = {}

    for _, v in ipairs(dice) do
        if #n == 0 or n[#n] == v then
            table.insert(n, v)
        end
    end

    if #n == 5 then
        return 50
    end

    return 0
end

yacht.score = function(dice, category)
    if category == 'ones' then
        return yacht.pickN(dice, 1)
    elseif category == 'twos' then
        return yacht.pickN(dice, 2)
    elseif category == 'threes' then
        return yacht.pickN(dice, 3)
    elseif category == 'fours' then
        return yacht.pickN(dice, 4)
    elseif category == 'fives' then
        return yacht.pickN(dice, 5)
    elseif category == 'sixes' then
        return yacht.pickN(dice, 6)
    elseif category == 'full house' then
        return yacht.fullHouse(dice)
    elseif category == 'four of a kind' then
        return yacht.fourKind(dice)
    elseif category == 'little straight' then
        return yacht.straight(dice, 'little')
    elseif category == 'big straight' then
        return yacht.straight(dice, 'big')
    elseif category == 'choice' then
        return yacht.choice(dice)
    elseif category == 'yacht' then
        return yacht.yacht(dice)
    end

    return 0
end

--print(yacht.score({}, 'yacht'))

return yacht
