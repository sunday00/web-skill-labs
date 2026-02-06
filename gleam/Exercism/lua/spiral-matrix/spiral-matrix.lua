local s = function(size)
    if size == 1 then
        return { { 1 } }
    end
    if size == 2 then
        return { { 1, 2 }, { 4, 3 } }
    end

    local dY = 0
    local dX = 1

    local minX = 1
    local maxX = size
    local minY = 2
    local maxY = size

    local p = { x = 1, y = 1 }

    local r = {}
    for i = 1, size do
        r[i] = {}
        for j = 1, size do
            r[i][j] = 0
        end
    end

    for i = 1, size * size do
        r[p.y][p.x] = i

        if p.x == maxX and dX == 1 then
            dX = 0
            dY = 1
            maxX = maxX - 1
        elseif p.y == maxY and dY == 1 then
            dY = 0
            dX = -1
            maxY = maxY - 1
        elseif p.x == minX and dX == -1 then
            dY = -1
            dX = 0
            minX = minX + 1
        elseif p.y == minY and dY == -1 then
            dY = 0
            dX = 1
            minY = minY + 1
        end

        p.y = p.y + dY
        p.x = p.x + dX
    end

    return r
end

return s