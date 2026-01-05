local single = {
    n1 = 'one',
    n2 = 'two',
    n3 = 'three',
    n4 = 'four',
    n5 = 'five',
    n6 = 'six',
    n7 = 'seven',
    n8 = 'eight',
    n9 = 'nine',
}

function handleSingleDigit (n)
    if n < 0 then
        return -1
    end
    if n == 0 then
        return 'zero'
    end

    if n == 0 then
        return 'zero'
    end

    return single['n' .. n]
end

function handleUnder20Digit(n)
    if n == 10 then
        return 'ten'
    end
    if n == 11 then
        return 'eleven'
    end
    if n == 12 then
        return 'twelve'
    end

    if n == 13 then
        return 'thirteen'
    end

    if n == 15 then
        return 'fifteen'
    end

    if n == 18 then
        return 'eighteen'
    end

    return single['n' .. (n - 10)] .. 'teen'
end

function handleDoubleDigit(n)
    local front = math.floor(n / 10)
    local tail = n % 10

    local x = single['n' .. front] .. 'ty'
    if front == 2 then
        x = 'twenty'
    end

    if front == 3 then
        x = 'thirty'
    end

    if front == 4 then
        x = 'forty'
    end

    if front == 5 then
        x = 'fifty'
    end

    if front == 8 then
        x = 'eighty'
    end

    if tail > 0 then
        return x .. '-' .. single['n' .. tail]
    end

    return x
end

function handleTripleDigit(n)
    local front = math.floor(n / 100)
    local tail = n % 100

    local x = single['n' .. front] .. ' ' .. 'hundred'

    if tail > 0 and tail < 10 then
        return x .. ' ' .. single['n' .. tail]
    end

    if tail > 0 and tail < 20 then
        return x .. ' ' .. handleUnder20Digit(tail)
    end

    if tail > 0 and tail > 0 then
        return x .. ' ' .. handleDoubleDigit(tail)
    end

    return x
end

function handleStepProcess(n)
    if n < 10 then
        return handleSingleDigit(n)
    end

    if n < 20 then
        return handleUnder20Digit(n)
    end

    if n < 100 then
        return handleDoubleDigit(n)
    end

    if n < 1000 then
        return handleTripleDigit(n)
    end
end

function handleBigDigit(n, complete, step)
    local rem, tail = string.sub(tostring(n), 1, (#tostring(n) - 3)), string.sub(tostring(n), -3)
    if #tostring(n) < 3 then
        rem = ''
    end

    local tailW = ''
    if #tail > 0 and tonumber(tail) > 0 then
        tailW = handleStepProcess(tonumber(tail)) .. complete
        if #tostring(rem) > 0 then
            tailW = ' ' .. tailW
        end
    end

    if step == 1 then
        complete = ' thousand'
    end

    if step == 2 then
        complete = ' million'
    end

    if step == 3 then
        complete = ' billion'
    end

    local headW = ''
    if #rem > 0 and tonumber(rem) > 0 then
        headW = handleBigDigit(rem, complete, step + 1)
        return headW .. tailW
    else
        return tailW
    end
end

function x (n)
    if n < 1000 then
        return handleStepProcess(n)
    end

    if n > 999999999999 then
        return -1
    end

    return handleBigDigit(n, '', 1)
end

--print(x(1000))
--print(x(1010))
--print(x(1100))
--print(x(1407))
--print(x(2156))
--print(x(4004))
--print(x(8556))
--print(x(10070))
--print(x(987654321123))
--print(x(1000000000000))

return x