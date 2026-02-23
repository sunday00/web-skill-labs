local Proverb = {}

function Proverb.recite(strings)
    if #strings == 0 then
        return ''
    elseif #strings == 1 then
        return string.format('And all for the want of a %s.\n', strings[1])
    else
        local res = ''

        for i = 1, #strings - 1 do
            res = res .. string.format('For want of a %s the %s was lost.\n', strings[i], strings[i + 1])
        end

        res = res .. string.format('And all for the want of a %s.\n', strings[1])

        return res
    end
end

--local r = Proverb.recite({ 'nail', 'shoe', 'horse' })

return Proverb
