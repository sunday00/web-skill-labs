local function debug(game, line)
    if line ~= nil then
        print('L: ', line)
    end
    --print('A: ', game.A.history[#game.A.history])
    --print('B: ', game.B.history[#game.B.history])

    print('A: ', game.A.dec)
    print('B: ', game.B.dec)

    print('piles: ', game.piles)
    print('game: ', game.tricks, game.cards)
    print('\n\n')
end

local function cardToI (abcCard)
    if abcCard == 'J' then
        return 1
    elseif abcCard == 'Q' then
        return 2
    elseif abcCard == 'K' then
        return 3
    elseif abcCard == 'A' then
        return 4
    else
        return 0
    end
end

local function opposite(player)
    if player == 'A' then
        return 'B'
    else
        return 'A'
    end
end

local function newGame(playerA, playerB)
    local game = {
        tricks = 0,
        cards = 0,
        status = '',

        A = { dec = '' },
        B = { dec = '' },
        turn = 'A',

        lastAskPay = nil,
        history = {},

        piles = ''
    }

    for _, v in ipairs(playerA) do
        game.A.dec = game.A.dec .. cardToI(v)
    end

    for _, v in ipairs(playerB) do
        game.B.dec = game.B.dec .. cardToI(v)
    end

    table.insert(game.history, game.A.dec .. '|' .. game.B.dec)

    return game
end

local function drawCard(game)
    local card = string.sub(game[game.turn].dec, 1, 1)

    if card == '' or card == nil then
        return nil
    else
        game[game.turn].dec = string.sub(game[game.turn].dec, 2, #game[game.turn].dec)
        game.piles = game.piles .. card
    end

    return card
end

local function processPayday(game, card)
    game.lastAskPay = game.turn

    for _ = 1, tonumber(card) do
        local paying = string.sub(game[opposite(game.turn)].dec, 1, 1)

        if paying == '' or paying == nil then
            return false, 'noCard'
        end

        game[opposite(game.turn)].dec = string.sub(game[opposite(game.turn)].dec, 2, #game[opposite(game.turn)].dec)
        game.piles = game.piles .. paying

        if paying ~= '0' then
            return false, 'payChange'
        end
    end

    return true, 'payDone'
end

local function processPayChange(game)
    game.lastAskPay = opposite(game.turn)
    game.turn = opposite(game.turn)

    return string.sub(game.piles, #game.piles, #game.piles)
end

local function processPayDn(game)
    game[game.lastAskPay].dec = game[game.lastAskPay].dec .. game.piles

    game.tricks = game.tricks + 1
    game.cards = game.cards + #game.piles

    game.piles = ''

    game.turn = game.lastAskPay

    if game[opposite(game.lastAskPay)].dec == '' then
        game.status = 'finished'
        return true
    end

    return false
end

local function processNoCard (game)
    if game.piles ~= '' then
        game[opposite(game.turn)].dec = game[opposite(game.turn)].dec .. game.piles
        game.tricks = game.tricks + 1
        game.cards = game.cards + #game.piles
    end

    game.status = 'finished'
    game.piles = ''
end

local function recordAndCheck(game)
    for i, v in ipairs(game.history) do
        if v == game.A.dec .. '|' .. game.B.dec .. '|' .. game.piles then
            return true
        end
    end

    table.insert(game.history, game.A.dec .. '|' .. game.B.dec .. '|' .. game.piles)
    return false
end

local function processTurn (game, prevCard)
    local card = prevCard or drawCard(game)

    --print(card == nil, card == '', card)
    --debug(game)

    if card == nil or card == '' then
        processNoCard(game)

        return { status = 'finished', tricks = game.tricks, cards = game.cards }
    elseif card == '0' then
        if recordAndCheck(game) then
            return { status = 'loop', tricks = game.tricks, cards = game.cards }
        end

        game.turn = opposite(game.turn)

        return processTurn(game)
    else
        local dn, why = processPayday(game, card)

        if not dn and why == 'payChange' then
            local chgTrigger = processPayChange(game)

            if recordAndCheck(game) then
                return { status = 'loop', tricks = game.tricks, cards = game.cards }
            end

            return processTurn(game, chgTrigger)
        elseif not dn and why == 'noCard' then
            processNoCard(game)
            return { status = 'finished', tricks = game.tricks, cards = game.cards }
        else
            local gameDn = processPayDn(game)
            if gameDn then
                return { status = 'finished', tricks = game.tricks, cards = game.cards }
            end

            --if recordAndCheck(game) then
            --    return { status = 'loop', tricks = game.tricks, cards = game.cards }
            --end

            return processTurn(game)
        end
    end

    return { status = 'finished', tricks = game.tricks, cards = game.cards }
end

local function simulate_game(playerA, playerB)
    local game = newGame(playerA, playerB)

    return processTurn(game)
end

return { simulate_game = simulate_game }
