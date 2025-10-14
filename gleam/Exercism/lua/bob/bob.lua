local bob = {}

function bob.hey(say)
    say = string.gsub(say, '%s', '')

    if #say == 0 or say == "" then
        return 'Fine. Be that way!'
    end

    local isQuest = false
    local isYell = true

    for ch in string.gmatch(say, '.') do
        if string.lower(ch) == string.upper(ch) then
            goto continue
        end

        if string.lower(ch) == ch then
            isYell = false
            break
        end

        :: continue ::
    end

    if string.sub(say, -1) == '?' then
        isQuest = true
    end

    if string.lower(say) == string.upper(say) then
        isYell = false
    end

    if isYell and isQuest then
        return 'Calm down, I know what I\'m doing!'

    elseif isQuest then
        return 'Sure.'

    elseif isYell then
        return 'Whoa, chill out!'

    else
        return 'Whatever.'
    end
end

return bob
