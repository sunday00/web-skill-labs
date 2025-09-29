local grains = {}

function grains.square(n)
    return 2 ^ (n - 1)
end

function grains.total()
    local s = 0
    for i = 1, 64 do
        s = s + grains.square(i)
    end

    return s
end

return grains
