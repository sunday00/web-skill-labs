local g
local maxW = 0
local maxH = 0

local checkHorizonDash = function(x, xx, y, yy)
    for x1 = x, xx do
        local pointT = g[y]:sub(x1, x1)
        local pointB = g[yy]:sub(x1, x1)

        if (pointT ~= '+' and pointT ~= '-') or (pointB ~= '+' and pointB ~= '-') then
            return true
        end
    end

    return false
end

local findVerticalPoints = function(x, xx, y)
    local sum = 0

    for yy = y + 1, maxH do
        local v = g[yy]

        local pointLU = g[y]:sub(x, x)
        local pointRU = g[y]:sub(xx, xx)
        local pointLB = v:sub(x, x)
        local pointRB = v:sub(xx, xx)

        if (pointLB ~= '+' and pointLB ~= '|') or (pointRB ~= '+' and pointRB ~= '|') then
            return sum
        end

        if pointLU == '+' and pointRU == '+' and pointLB == '+' and pointRB == '+' then
            if checkHorizonDash(x, xx, y, yy) then return sum end

            sum = sum + 1
        end
    end

    return sum
end

local find4Points = function(x, y)
    local sum = 0

    local v = g[y]

    for xx = x+1, maxW do
        local pointRU = v:sub(xx, xx)

        if pointRU ~= '+' and pointRU ~= '-' then
            return sum
        end

        if pointRU == '+' then
            sum = sum + findVerticalPoints(x, xx, y)
        end
    end

    return sum
end

local checkIsZero = function(grid)
    if #grid == 0 then return true end
end

local count = function(grid)
    local isZero = checkIsZero(grid)

    if isZero then return 0 end

    g = grid
    maxW = #grid[1]
    maxH = #grid

    local sum = 0

    for y, v in ipairs(grid) do
        for x = 1, maxW do
            local pointLU = v:sub(x, x)

            if pointLU ~= '+' then
                goto continue
            end

            sum = sum + find4Points(x, y)

            :: continue ::
        end
    end

    return sum
end

count({
    '+-+ +-+', -- ,
    '| | | |', -- ,
    '+-+-+-+', -- ,
    '  | |  ', -- ,
    '+-+-+-+', -- ,
    '| | | |', -- ,
    '+-+ +-+' --
})

return {
    count = count
}
