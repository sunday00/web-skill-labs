local house = {}

local dic = {
    { o = 'house', act = 'built by Jack' },
    { o = 'malt', act = 'lay in' },
    { o = 'rat', act = 'ate' },
    { o = 'cat', act = 'killed' },
    { o = 'dog', act = 'worried' },
    { o = 'cow with the crumpled horn', act = 'tossed' },
    { o = 'maiden all forlorn', act = 'milked' },
    { o = 'man all tattered and torn', act = 'kissed' },
    { o = 'priest all shaven and shorn', act = 'married' },
    { o = 'rooster that crowed in the morn', act = 'woke' },
    { o = 'farmer sowing his corn', act = 'kept' },
    { o = 'horse and the hound and the horn', act = 'belonged to' },
}

house.verse = function(which)
    if which == 1 then
        return 'This is the house that Jack built.'
    end

    local s = 'This is the '
    for i = which, 2, -1 do
        local t = dic[i]
        local verse = string.format([[%s
that %s the ]], t.o, t.act)

        s = s .. verse
    end

    s = s .. 'house that Jack built.'

    return s
end

house.recite = function()
    local song = ''

    for i = 1, 12 do
        song = song .. house.verse(i)
        if i ~= 12 then
            song = song .. '\n'
        end
    end

    return song
end

--local t = house.verse(3)
--print(t)

return house
