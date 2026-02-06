function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
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

local function answer(question)
    local trimed = string.replace(string.replace(string.replace(question, 'What is ', ''), '?', ''), 'by ', '')
    local s = string.split(trimed, ' ')

    local x = 0
    local oper = ''
    for i, v in ipairs(s) do
        if i == 1 then
            if tonumber(v) then
                x = tonumber(v)
            else
                error('Invalid question')
            end
        elseif i % 2 == 1 then
            if tonumber(v) == nil then
                error('Invalid question')
            end

            if oper == 'plus' then
                x = x + tonumber(v)
            elseif oper == 'minus' then
                x = x - tonumber(v)
            elseif oper == 'multiplied' then
                x = x * tonumber(v)
            elseif oper == 'divided' then
                x = x / tonumber(v)
            else
                error('Invalid question')
            end

            oper = ''
        else
            oper = v
        end
    end

    if oper ~= '' then
        error('Invalid question')
    end

    return x
end

--print(answer('What is 1 plus plus 2?'))

return { answer = answer }
