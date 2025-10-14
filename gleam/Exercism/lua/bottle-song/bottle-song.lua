local BottleSong = {}

local verse = {
    ":cnt: green :bottles: hanging on the wall,\n",
    "And if one green bottle should accidentally fall,\n",
    "There'll be :rem: green :remBottles: hanging on the wall.\n",
}

local cntTxt = {
    'One', 'Two', 'Three', 'Four', 'Five',
    'Six', 'Seven', 'Eight', 'Nine', 'Ten',
}

function bottleOrBottles(cnt)
    if cnt == 1 then
        return 'bottle'
    end

    return 'bottles'
end

function remainBottleCnt(originalCnt)
    local remCnt = originalCnt - 1

    if remCnt > 0 then
        return string.lower(cntTxt[remCnt])
    end

    return 'no'
end

function BottleSong.recite(start_bottles, take_down)
    local verses = {}

    for i = start_bottles, start_bottles - take_down + 1, -1 do
        if #verses > 0 then
            table.insert(verses, '\n')
        end

        local curTxt = cntTxt[i]
        local bottleTxt = bottleOrBottles(i)
        local rem = remainBottleCnt(i)
        local remBottle = bottleOrBottles(i - 1)

        local line1 = string.gsub(verse[1], ':cnt:', curTxt)
        line1 = string.gsub(line1, ':bottles:', bottleTxt)

        local line4 = string.gsub(verse[3], ':rem:', rem)
        line4 = string.gsub(line4, ':remBottles:', remBottle)

        table.insert(verses, line1)
        table.insert(verses, line1)
        table.insert(verses, verse[2])
        table.insert(verses, line4)
    end

    return table.concat(verses)
end

--BottleSong.recite(10, 4)

return BottleSong
