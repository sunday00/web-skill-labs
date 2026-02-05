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

function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end

local MATRIX = {}

local function rebuildMap (board)
    MATRIX = {}

    for y, v in ipairs(board) do
        local simplified = string.replace(string.replace(v, ' ', ''), '%.', 'B')
        local fragile = string.split(simplified)

        local row = {}
        for x, point in ipairs(fragile) do
            table.insert(row, { x = x, y = y, t = point, visit = false })
        end

        table.insert(MATRIX, row)
    end
end

local function makeNeighbors (cur)
    local neis = {}
    if MATRIX[cur.y - 1] then
        table.insert(neis, MATRIX[cur.y - 1][cur.x])
        table.insert(neis, MATRIX[cur.y - 1][cur.x + 1])
    end

    table.insert(neis, MATRIX[cur.y][cur.x - 1])
    table.insert(neis, MATRIX[cur.y][cur.x + 1])

    if MATRIX[cur.y + 1] then
        table.insert(neis, MATRIX[cur.y + 1][cur.x - 1])
        table.insert(neis, MATRIX[cur.y + 1][cur.x])
    end

    return neis
end

local function isXCrossLR ()
    local QUE = {}

    for _, v in ipairs(MATRIX) do
        if v[1].t == 'X' then
            table.insert(QUE, v[1])
        end
    end

    while #QUE > 0 do
        local cur = table.remove(QUE, 1)

        local neis = makeNeighbors(cur)

        for _, nei in ipairs(neis) do
            if nei.t == 'X' and nei.x == #MATRIX[1] then
                return true
            end

            if nei.t == 'X' and nei.visit == false then
                nei.visit = true
                table.insert(QUE, nei)
            end
        end
    end

    return false
end

local function isOCrossTB ()
    local QUE = {}

    for _, v in ipairs(MATRIX[1]) do
        if v.t == 'O' then
            table.insert(QUE, v)
        end
    end

    while #QUE > 0 do
        local cur = table.remove(QUE, 1)

        local neis = makeNeighbors(cur)

        for _, nei in ipairs(neis) do
            if nei.t == 'O' and nei.y == #MATRIX then
                return true
            end

            if nei.t == 'O' and nei.visit == false then
                nei.visit = true
                table.insert(QUE, nei)
            end
        end
    end

    return false
end

local s = {
    winner = function(board)
        if #board == 1 and board[1] == 'X' then
            return 'X'
        end
        if #board == 1 and board[1] == 'O' then
            return 'O'
        end

        rebuildMap(board)
        local isXWin = isXCrossLR()

        rebuildMap(board)
        local isOWin = isOCrossTB()

        if isXWin then
            return 'X'
        elseif isOWin then
            return 'O'
        else
            return ''
        end
    end
}

return s