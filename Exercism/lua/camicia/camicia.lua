local function newGame(playerA, playerB)
    local game = {
        rnd = 0,
        tricked = 0,
        totalCards = 0,
        status = '',

        playerA = { init = {}, dec = {}, tricked = false },
        playerB = { init = {}, dec = {}, tricked = false },
        turn = 'A',

        piles = {}
    }

    for _, v in ipairs(playerA) do
        table.insert(game.playerA.init, v)
        table.insert(game.playerA.dec, v)
    end

    for _, v in ipairs(playerB) do
        table.insert(game.playerB.init, v)
        table.insert(game.playerB.dec, v)
    end

    return game
end

local function drawCard(game)
    local player = 'player' .. game.turn
    local card = table.remove(game[player].dec, 1)

    table.insert(game.piles, card)

    return card
end

local function askOppositePay(game, card)
    local enemy = 'playerA'
    if game.turn == 'A' then
        enemy = 'playerB'
    end

    local shouldPay = 0
    if card == 'J' then
        shouldPay = 1
    elseif card == 'Q' then
        shouldPay = 2
    elseif card == 'K' then
        shouldPay = 3
    elseif card == 'A' then
        shouldPay = 4
    end

    for i = 1, shouldPay do
        local pay = table.remove(game[enemy].dec, 1)
        table.insert(game.piles, pay)

        if tonumber(pay) == nil then

        end
    end
end

local function processTurn (game)
    local card = drawCard(game)

    if tonumber(card) == nil then
        askOppositePay(game, card)
    else
        -- normal turnover
    end

    return game
end

local function simulate_game(playerA, playerB)
    local game = newGame(playerA, playerB)

    return processTurn(game)
end

return { simulate_game = simulate_game }
