local EliudsEggs = {}

local function toBinaryString(n)
    if n == 0 then
        return "0"
    end
    local binary = ""
    while n > 0 do
        binary = (n % 2) .. binary
        n = math.floor(n / 2)
    end
    return binary
end

function EliudsEggs.egg_count(number)
    local b = toBinaryString(number)

    local bt = string.gmatch(b, '.')

    local n = 0
    for v in bt do
        if v == '1' then
            n = n + 1
        end
    end

    return n
end

return EliudsEggs
