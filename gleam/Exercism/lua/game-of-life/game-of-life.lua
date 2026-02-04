local gMatrix = {}

local function cntLiveNeighbors(x, y)
    local r = 0

    if gMatrix[y - 1] then
        if gMatrix[y - 1][x - 1] == 1 then
            r = r + 1
        end
        if gMatrix[y - 1][x] == 1 then
            r = r + 1
        end
        if gMatrix[y - 1][x + 1] == 1 then
            r = r + 1
        end
    end

    if gMatrix[y][x - 1] == 1 then
        r = r + 1
    end
    if gMatrix[y][x + 1] == 1 then
        r = r + 1
    end

    if gMatrix[y + 1] then
        if gMatrix[y + 1][x - 1] == 1 then
            r = r + 1
        end
        if gMatrix[y + 1][x] == 1 then
            r = r + 1
        end
        if gMatrix[y + 1][x + 1] == 1 then
            r = r + 1
        end
    end

    return r
end

local function tick(matrix)
    gMatrix = matrix

    local newMatrix = {}

    for y, v in ipairs(matrix) do
        local newRow = {}

        for x, vv in ipairs(v) do
            local localCntLiveNeighbors = cntLiveNeighbors(x, y)

            if vv == 1 and (localCntLiveNeighbors == 2 or localCntLiveNeighbors == 3) then
                table.insert(newRow, 1)
            elseif vv == 0 and localCntLiveNeighbors == 3 then
                table.insert(newRow, 1)
            else
                table.insert(newRow, 0)
            end
        end

        table.insert(newMatrix, newRow)
    end

    return newMatrix
end

--local r = tick({ { 1, 0, 1 }, --
--                 { 1, 0, 1 }, --
--                 { 1, 0, 1 } --
--})
--
--for _, v in ipairs(r) do
--    local s = ''
--
--    for _, vv in ipairs(v) do
--        s = s .. vv .. ','
--    end
--
--    print(s)
--end

return { tick = tick }
