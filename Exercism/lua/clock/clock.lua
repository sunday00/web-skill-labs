local Clock = { h = 0, m = 0 }
Clock.__index = Clock

function Clock:__tostring()
    return string.format("%02d:%02d", self.h, self.m)
end

local function fitTime(clock)
    local hh = clock.h
    local mm = clock.m

    if clock.m >= 60 then
        mm = clock.m % 60
        hh = hh + math.floor(clock.m / 60)
    end

    if clock.m < 0 then
        mm = (60 + (clock.m % -60)) % 60
        hh = hh + math.floor(clock.m / 60)
    end

    if hh >= 24 then
        hh = hh % 24
    end

    if hh < 0 then
        hh = (24 + (hh % -24)) % 24
    end

    clock.h = hh
    clock.m = mm
end

function Clock.at (h, m)
    m = m or 0
    local self = setmetatable({ h = 0, m = 0 }, Clock)

    self.h = h
    self.m = m

    fitTime(self)

    return self
end

function Clock:plus(m)
    self.m = self.m + m
    fitTime(self)

    return self
end

function Clock:minus(m)
    self.m = self.m - m
    fitTime(self)

    return self
end

function Clock:equals(clock)
    return self.h == clock.h and self.m == clock.m
end

return Clock
