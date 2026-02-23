local dic = {
    AUG = 'Methionine',
    UUU = 'Phenylalanine',
    UUC = 'Phenylalanine',
    UUA = 'Leucine',
    UUG = 'Leucine',
    UCU = 'Serine',
    UCC = 'Serine',
    UCA = 'Serine',
    UCG = 'Serine',
    UAU = 'Tyrosine',
    UAC = 'Tyrosine',
    UGU = 'Cysteine',
    UGC = 'Cysteine',
    UGG = 'Tryptophan',
    UAA = 'STOP',
    UAG = 'STOP',
    UGA = 'STOP'
}

local function proteins(strand)
    local r = {}

    for i = 1, #strand - 2, 3 do
        local v = string.sub(strand, i, i + 2)
        local t = dic[v]

        if t == 'STOP' then
            break
        end

        if t == nil then
            error()
        end

        table.insert(r, t)
    end

    return r
end

return { proteins = proteins }
