local strToDayInt = {
    Sunday = 1,
    Monday = 2,
    Tuesday = 3,
    Wednesday = 4,
    Thursday = 5,
    Friday = 6,
    Saturday = 7,
}

local weekAdd = {
    first = 0,
    second = 7,
    third = 14,
    fourth = 21
}

local function addWeekAdd (cur, configWeek)
    cur.day = cur.day + weekAdd[configWeek]

    return cur.day
end

local function findLast (cur)
    local dup = os.date('*t', os.time(cur))

    while true do
        local td = dup.day

        dup.day = dup.day + 7
        dup = os.date('*t', os.time(dup))

        if dup.month ~= cur.month then
            return td
        end
    end
end

local function findTeenth(cur)
    local dup = os.date('*t', os.time(cur))

    while true do
        if dup.day >= 13 and dup.day <= 19 then
            return dup.day
        end

        dup.day = dup.day + 7
        dup = os.date('*t', os.time(dup))
    end
end

local function ddd (config)
    local firstTime = os.time({ year = config.year, month = config.month, day = 1 })
    local firstDate = os.date("*t", firstTime)
    local weekDay = strToDayInt[config.day]

    local firstDay = firstDate.wday

    local sameTargetFirstWeekDayDate = os.date("*t", firstTime)
    sameTargetFirstWeekDayDate.day = firstDate.day + (weekDay - firstDay + 7) % 7

    if config.week ~= 'teenth' and config.week ~= 'last' then
        return addWeekAdd(sameTargetFirstWeekDayDate, config.week)
    end

    if config.week == 'last' then
        return findLast(sameTargetFirstWeekDayDate)
    end

    return findTeenth(sameTargetFirstWeekDayDate)
end

--local r = ddd({ year = 2013, month = 12, week = 'last', day = 'Friday' })
--print(r)

return ddd