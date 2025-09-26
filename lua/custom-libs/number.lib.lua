function toBinaryString(n)
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

function fromBinaryString(bin_str)
    local num = 0
    for i = 1, #bin_str do
        local bit_val = tonumber(string.sub(bin_str, i, i))
        num = num * 2 + bit_val
    end
    return num
end