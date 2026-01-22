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

local tBoard = {}

local function checkWinner (who)
    if (tBoard[1][1] == who and tBoard[1][2] == who and tBoard[1][3] == who) or
            (tBoard[2][1] == who and tBoard[2][2] == who and tBoard[2][3] == who) or
            (tBoard[3][1] == who and tBoard[3][2] == who and tBoard[3][3] == who) or

            (tBoard[1][1] == who and tBoard[2][1] == who and tBoard[3][1] == who) or
            (tBoard[1][2] == who and tBoard[2][2] == who and tBoard[3][2] == who) or
            (tBoard[1][3] == who and tBoard[2][3] == who and tBoard[3][3] == who) or

            (tBoard[1][1] == who and tBoard[2][2] == who and tBoard[3][3] == who) or
            (tBoard[1][3] == who and tBoard[2][2] == who and tBoard[3][1] == who) then
        return true
    end

    return false
end

local function checkCount ()
    local x = 0
    local o = 0
    local blank = 0

    for i = 1, 3 do
        for ii = 1, 3 do
            if tBoard[i][ii] == 'O' then
                o = o + 1
            elseif tBoard[i][ii] == 'X' then
                x = x + 1
            else
                blank = blank + 1
            end
        end
    end

    if o > x then
        error()
    end

    if o + 1 < x then
        error()
    end

    if blank == 0 then
        return 'draw'
    end

    return 'ongoing'
end

local function gamestate(board)
    tBoard = {}

    for _, v in ipairs(board) do
        table.insert(tBoard, string.split(v))
    end

    local isXWin = checkWinner('X')
    local isOWin = checkWinner('O')

    if isXWin and isOWin then
        error()
    end

    local countR = checkCount()

    if isXWin or isOWin then
        return 'win'
    end

    return countR
end

--local r = gamestate({
--    'XOO', --
--    'X  ', --
--    'X  ' --
--})
--
--print(r)

return { gamestate = gamestate }
