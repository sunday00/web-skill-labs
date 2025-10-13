local SquareRoot = {}

function SquareRoot.square_root(radicand)
    if radicand == 1 then
        return 1
    end

    for i = 1, radicand / 2 do
        if i * i == radicand then
            return i
        end
    end
end

return SquareRoot
