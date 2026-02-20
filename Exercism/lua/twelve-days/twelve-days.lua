local dic = {
    { no = 'first', obj = 'a Partridge' },
    { no = 'second', obj = 'two Turtle Doves' },
    { no = 'third', obj = 'three French Hens' },
    { no = 'fourth', obj = 'four Calling Birds' },
    { no = 'fifth', obj = 'five Gold Rings' },
    { no = 'sixth', obj = 'six Geese-a-Laying' },
    { no = 'seventh', obj = 'seven Swans-a-Swimming' },
    { no = 'eighth', obj = 'eight Maids-a-Milking' },
    { no = 'ninth', obj = 'nine Ladies Dancing' },
    { no = 'tenth', obj = 'ten Lords-a-Leaping' },
    { no = 'eleventh', obj = 'eleven Pipers Piping' },
    { no = 'twelfth', obj = 'twelve Drummers Drumming' },
}

local function recite(start_verse, end_verse)
    local verseTemp = 'On the "DAY" day of Christmas my true love gave to me: "OBJECTS" in a Pear Tree.'
    local verses = {}

    for i = start_verse, end_verse do
        local t = dic[i]
        local objects = ''
        for ii = 1, i do
            local o = dic[ii]

            if ii == 1 then
                objects = o.obj
            elseif ii == 2 then
                objects = o.obj .. ', and ' .. objects
            else
                objects = o.obj .. ', ' .. objects
            end
        end

        local verse = ('' .. verseTemp):gsub('"DAY"', t.no):gsub('"OBJECTS"', objects)

        table.insert(verses, verse)
    end

    return verses
end

return { recite = recite }
