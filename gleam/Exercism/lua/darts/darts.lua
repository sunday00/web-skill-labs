local Darts = {}

function Darts.score(x, y)
    local x, y = math.abs(x), math.abs(y)
    local c = (x ^ 2 + y ^ 2) ^ 0.5

    if c > 10 then
        return 0
    elseif c > 5 then
        return 1
    elseif c > 1 then
        return 5
    else
        return 10
    end
end

return Darts
