function table.debug(t)
    for i, el in pairs(t) do
        print(i, el)
    end
end

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
    --return string.gsub(s, prev, after)
    return string.gsub(s, prev:gsub("[%(%)%.%%%+%-%*%?%[%^%$]", "%%%1"), after)
end

local function checkDefinable(instructions)
    for _, v in ipairs(instructions) do
        if v:find(':') ~= nil then
            local worlds = v:split(' ')
            local key = worlds[2]

            if type(tonumber(key)) == 'number' then
                error()
            end
        end
    end
end

local function parseDefined(instructions)
    for i = 1, #instructions do
        instructions[i] = string.lower(instructions[i])
    end

    checkDefinable(instructions)

    if #instructions < 2 then
        return instructions[1]
    end

    local state = table.remove(instructions, #instructions)
    local defins = {}
    for _, v in ipairs(instructions) do
        local words = string.split(v, ' ')
        local key = words[2]
        local value = ''

        for i = 3, #words - 1 do
            if defins[words[i]] ~= nil then
                value = value .. ' ' .. defins[words[i]]

            else

                value = value .. ' ' .. words[i]
            end
        end

        value = string.sub(value, 2, #value)

        defins[key] = value
    end

    for k, v in pairs(defins) do
        state = string.replace(state, k, v)
    end

    return state
end

local function doDup(stack)
    if #stack < 1 then
        error()
    end

    table.insert(stack, stack[#stack])
end

local function doDrop(stack)
    if #stack < 1 then
        error()
    end

    table.remove(stack, #stack)
end

local function doSwap(stack)
    if #stack < 2 then
        error()
    end

    local tail = table.remove(stack, #stack)
    local head = table.remove(stack, #stack)

    table.insert(stack, tail)
    table.insert(stack, head)
end

local function doOver(stack)
    if #stack < 2 then
        error()
    end

    local target = stack[#stack - 1]
    table.insert(stack, target)
end

local function calculate(state)
    local words = string.split(state, ' ')
    local stack = {}
    for _, w in ipairs(words) do
        if w == 'dup' then
            doDup(stack)

            goto continue
        end

        if w == 'drop' then
            doDrop(stack)

            goto continue
        end

        if w == 'swap' then
            doSwap(stack)

            goto continue
        end

        if w == 'over' then
            doOver(stack)

            goto continue
        end

        if w == '+' then
            local tail = table.remove(stack, #stack)
            local head = table.remove(stack, #stack)

            table.insert(stack, tonumber(head) + tonumber(tail))

            goto continue
        end

        if w == '-' then
            local tail = table.remove(stack, #stack)
            local head = table.remove(stack, #stack)

            table.insert(stack, tonumber(head) - tonumber(tail))

            goto continue
        end

        if w == '*' then
            local tail = table.remove(stack, #stack)
            local head = table.remove(stack, #stack)

            table.insert(stack, tonumber(head) * tonumber(tail))

            goto continue
        end

        if w == '/' then
            local tail = table.remove(stack, #stack)
            local head = table.remove(stack, #stack)

            if tonumber(tail) == 0 then
                error()
            end

            table.insert(stack, math.floor(tonumber(head) / tonumber(tail)))

            goto continue
        end

        if type(tonumber(w)) ~= 'number' then
            error()
        end

        table.insert(stack, tonumber(w))

        :: continue ::
    end

    return stack
end

local function evaluate(instructions)
    local state = parseDefined(instructions)

    local res = calculate(state)

    return res
end

--local r = evaluate({ 'foo' })
--table.debug(r)

return { evaluate = evaluate }
