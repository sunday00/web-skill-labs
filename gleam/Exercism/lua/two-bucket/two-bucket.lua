local record = {
    lastAction = ''
}

local function init(args)
    if args.bucket_one_capacity < args.goal_volume and args.bucket_two_capacity < args.goal_volume then
        error()
    end

    local one = {
        fill = 0,
        max = args.bucket_one_capacity,
        name = 'one'
    }

    local two = {
        fill = 0,
        max = args.bucket_two_capacity,
        name = 'two'
    }

    if args.start_bucket == 1 then
        one.fill = one.max
        record.lastAction = 'fillOne'
    else
        two.fill = two.max
        record.lastAction = 'fillTwo'
    end

    return {
        moves = 1,
        one = one,
        two = two
    }
end

local function poul(x, y)
    if (y.max - y.fill) > x.fill then
        y.fill = y.fill + x.fill
        x.fill = 0
    else
        x.fill = x.fill - (y.max - y.fill)
        y.fill = y.max
    end

    if x.name == 'one' then
        record.lastAction = 'poulOneToTwo'
    else
        record.lastAction = 'poulTwoToOne'
    end

    return { x, y }
end

local function emptying(x)
    x.fill = 0

    if x.name == 'one' then
        record.lastAction = 'emptyOne'
    else
        record.lastAction = 'emptyTwo'
    end

    return x
end

local function filling(x)
    x.fill = x.max

    if x.name == 'one' then
        record.lastAction = 'fillOne'
    else
        record.lastAction = 'fillTwo'
    end

    return x
end

local function repeater (initO, args, tried)
    local tr = initO

    --print(record.lastAction, tr.one.fill, tr.two.fill, args.goal_volume, tr.one.fill == args.goal_volume or tr.two.fill == args.goal_volume)

    if (tr.one.fill == args.goal_volume) then
        tr.goal_bucket_number = 1
        tr.other_bucket_volume = tr.two.fill
        return tr
    end

    if (tr.two.fill == args.goal_volume) then
        tr.goal_bucket_number = 2
        tr.other_bucket_volume = tr.one.fill
        return tr
    end

    if tr.moves > 100 or tried > 100 then
        return false
    end

    if args.start_bucket == 1 and initO.two.max == args.goal_volume then
        tr.moves = tr.moves + 1
        tr.goal_bucket_number = 2
        tr.other_bucket_volume = tr.one.fill

        return tr
    end

    if args.start_bucket == 2 and initO.one.max == args.goal_volume then
        tr.moves = tr.moves + 1
        tr.goal_bucket_number = 1
        tr.other_bucket_volume = tr.two.fill

        return tr
    end

    if record.lastAction ~= 'poulOneToTwo' and record.lastAction ~= 'poulTwoToOne' and initO.one.fill > 0 and initO.two.fill < initO.two.max then
        local ttr = poul(initO.one, initO.two)

        tr.one = ttr[1]
        tr.two = ttr[2]
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end

    if record.lastAction ~= 'poulTwoToOne' and record.lastAction ~= 'poulOneToTwo' and initO.two.fill > 0 and initO.one.fill < initO.one.max then
        local ttr = poul(initO.two, initO.one)

        tr.one = ttr[2]
        tr.two = ttr[1]
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end

    if (args.start_bucket ~= 1 or initO.two.fill ~= initO.two.max) and record.lastAction ~= 'fillOne' and record.lastAction ~= 'emptyOne' and record.lastAction ~= 'emptyTwo' and initO.one.fill > 0 and initO.two.fill > 0 then
        local ttr = emptying(initO.one)
        tr.one = ttr
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end

    if (args.start_bucket ~= 2 or initO.one.fill ~= initO.one.max) and record.lastAction ~= 'fillTwo' and record.lastAction ~= 'emptyOne' and record.lastAction ~= 'emptyTwo' and initO.one.fill > 0 and initO.two.fill > 0 then
        local ttr = emptying(initO.two)
        tr.two = ttr
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end

    if (args.start_bucket ~= 2 or initO.two.fill > 0) and record.lastAction ~= 'fillOne' and record.lastAction ~= 'fillTwo' and record.lastAction ~= 'emptyOne' then
        local ttr = filling(initO.one)

        tr.one = ttr
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end

    if (args.start_bucket ~= 1 or initO.one.fill > 0) and record.lastAction ~= 'fillOne' and record.lastAction ~= 'fillTwo' and record.lastAction ~= 'emptyTwo' then
        local ttr = filling(initO.two)

        tr.two = ttr
        tr.moves = tr.moves + 1

        return repeater(tr, args, tried + 1)
    end
end

local o = {
    measure = function(args)
        local initO = init(args)

        local res = repeater(initO, args, 1)

        if res == false then
            error()
        end

        return {
            moves = res.moves, --
            other_bucket_volume = res.other_bucket_volume, --
            goal_bucket_number = res.goal_bucket_number --
        }
    end
}

--local r = o.measure({
--    bucket_one_capacity = 2, --
--    bucket_two_capacity = 3, --
--    goal_volume = 3, --
--    start_bucket = 1 --
--})
--for i, v in pairs(r) do
--    print(i, v)
--end

return o
