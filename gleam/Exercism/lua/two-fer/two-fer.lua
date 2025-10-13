local TwoFer = {}

function TwoFer.two_fer(name)
    local txt = 'One for you, one for me.'

    if name == nil then
        return txt
    end

    return string.gsub(txt, 'you', name)
end

return TwoFer
