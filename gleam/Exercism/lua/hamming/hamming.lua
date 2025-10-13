local Hamming = {}

function Hamming.compute(a, b)
    if #a ~= #b then
        return error("strands must be of equal length")
    end

    local diff = 0
    for i = 1, #a do
        if string.sub(a, i, i) ~= string.sub(b, i, i) then
            diff = diff + 1
        end
    end

    return diff
end

return Hamming
