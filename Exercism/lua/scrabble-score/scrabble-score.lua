local dic = {
    a = 1,
    b = 3,
    c = 3,
    d = 2,
    e = 1,

    f = 4,
    g = 2,
    h = 4,
    i = 1,
    j = 8,

    k = 5,
    l = 1,
    m = 3,
    n = 1,
    o = 1,

    p = 3,
    q = 10,
    r = 1,
    s = 1,
    t = 1,

    u = 1,
    v = 4,
    w = 4,
    x = 8,
    y = 4,

    z = 10,
}

local function score(word)
    if word == nil then
        return 0
    end

    local sum = 0

    local w = string.lower(word)
    for i = 1, #word do
        local t = string.sub(w, i, i)
        sum = sum + dic[t]
    end

    return sum
end

return { score = score }
