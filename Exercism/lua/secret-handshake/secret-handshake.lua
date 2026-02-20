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

local dic = {
    '',
    'jump',
    'close your eyes',
    'double blink',
    'wink',
}

local function sol(n)
    local binary = toBinaryString(n)

    if binary == '0' then
        return {}
    end

    local s = 5
    local e = 2
    local st = -1

    if #binary == 5 and string.sub(binary, 1, 1) == '1' then
        s = 2
        e = 5
        st = 1
    end

    binary = string.rep('0', 5 - #binary) .. binary

    local res = {}

    for i = s, e, st do
        if string.sub(binary, i, i) == '1' then
            table.insert(res, dic[i])
        end
    end

    return res
end

return sol