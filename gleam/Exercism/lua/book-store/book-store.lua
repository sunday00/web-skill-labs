local function avg_ver (basket)
    local rr = {}

    for _, v in ipairs(basket) do
        if #rr == 0 then
            table.insert(rr, tostring(v))

            goto continue
        end

        local min = 6
        for _, vv in ipairs(rr) do
            if #vv <= min then
                min = #vv
            end
        end

        for ii, vv in ipairs(rr) do
            if #vv <= min and not string.find(vv, v) then
                rr[ii] = vv .. tostring(v)

                goto continue
            end
        end

        table.insert(rr, tostring(v))

        :: continue ::
    end

    return rr
end

local function maxize_ver(basket)
    local rr = {}

    for _, v in ipairs(basket) do
        if #rr == 0 then
            table.insert(rr, tostring(v))

            goto continue
        end

        for ii, vv in ipairs(rr) do
            if not string.find(vv, v) then
                rr[ii] = rr[ii] .. tostring(v)

                goto continue
            end
        end

        table.insert(rr, tostring(v))

        :: continue ::
    end

    return rr
end

local function optimize_maximize(rr)
    local threeId = 0
    local fiveId = 0

    for ii, vv in ipairs(rr) do
        if #vv == 3 then
            threeId = ii

            if fiveId > 0 then
                rr[threeId] = 'xxxx'
                rr[fiveId] = 'xxxx'

                threeId = 0
                fiveId = 0
            end
        end

        if #vv == 5 then
            fiveId = ii

            if threeId > 0 then
                rr[threeId] = 'xxxx'
                rr[fiveId] = 'xxxx'

                threeId = 0
                fiveId = 0
            end
        end
    end

    return rr
end

local function get_value (rr)
    local sum = 0
    for _, v in ipairs(rr) do
        if #v == 1 then
            sum = sum + 800
        end
        if #v == 2 then
            sum = sum + (800 * 2 * 0.95)
        end
        if #v == 3 then
            sum = sum + (800 * 3 * 0.9)
        end
        if #v == 4 then
            sum = sum + (800 * 4 * 0.8)
        end
        if #v == 5 then
            sum = sum + (800 * 5 * 0.75)
        end
    end

    return sum
end

local function total(basket)
    local rr1 = avg_ver(basket)
    local rr2 = maxize_ver(basket)

    rr2 = optimize_maximize(rr2)

    local s1 = get_value(rr1)
    local s2 = get_value(rr2)

    if s1 < s2 then
        return math.tointeger(s1)
    end

    return math.tointeger(s2)
end

--local r = total({ 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5 })
--
--print(r)

return { total = total }
