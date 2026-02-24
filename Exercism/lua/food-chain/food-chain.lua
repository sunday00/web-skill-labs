local dic = {
    {
        o = 'fly',
        f = ''
    },
    {
        o = 'spider',
        f = 'It wriggled and jiggled and tickled inside her.\n',
        n = 'spider that wriggled and jiggled and tickled inside her'
    },
    {
        o = 'bird',
        f = 'How absurd to swallow a bird!\n'
    },
    {
        o = 'cat',
        f = 'Imagine that, to swallow a cat!\n'
    },
    {
        o = 'dog',
        f = 'What a hog, to swallow a dog!\n'
    },
    {
        o = 'goat',
        f = 'Just opened her throat and swallowed a goat!\n'
    },
    {
        o = 'cow',
        f = "I don't know how she swallowed a cow!\n"
    },
    {
        o = 'horse',
        f = ''
    }
}

local startVerse = "I know an old lady who swallowed a %s.\n"
local why = 'She swallowed the %s to catch the %s.\n'
local endVerse = "I don't know why she swallowed the fly. Perhaps she'll die.\n"

local function verse(which)
    if which == 1 then
        return string.format(startVerse, 'fly') .. endVerse
    end

    if which == 2 then
        return string.format(startVerse, 'spider') .. dic[2].f .. string.format(why, 'spider', 'fly') .. endVerse
    end

    if which == 8 then
        return string.format(startVerse, 'horse') .. "She's dead, of course!\n"
    end

    local t = dic[which]
    local v = string.format(startVerse, t.o) .. t.f
    local cur = t.o

    for i = which, 2, -1 do
        local next = dic[i - 1].o
        if i - 1 == 2 then
            next = dic[2].n
        end

        v = v .. string.format(why, cur, next)
        cur = dic[i - 1].o
    end

    return v .. endVerse
end

local function verses(from, to)
    local v = ''

    for i = from, to do
        v = v .. verse(i) .. '\n'
    end

    return v
end

local function sing()
    return verses(1, 8)
end

return { verse = verse, verses = verses, sing = sing }
