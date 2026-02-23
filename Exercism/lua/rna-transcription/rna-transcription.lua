local function sol (dna)
    local ans = ''

    local function conv (d)
        if d == 'G' then
            return 'C'
        elseif d == 'C' then
            return 'G'
        elseif d == 'T' then
            return 'A'
        elseif d == 'A' then
            return 'U'
        end
    end

    for i = 1, #dna do
        ans = ans .. conv(string.sub(dna, i, i))
    end

    return ans
end

return sol