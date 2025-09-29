local gigasecond = {}

function gigasecond.anniversary(any_date)
    local t = any_date + 1000000000
    return os.date('%x', t)
end

return gigasecond
