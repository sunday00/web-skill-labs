local function append(xs, ys)
    for _, v in ipairs(ys) do
        table.insert(xs, v)
    end

    return xs
end

local function concat(...)
    local ts = { ... }

    if #ts == 0 then
        return {}
    end

    local r = {  }

    for _, v in ipairs(ts) do
        for _, vv in ipairs(v) do
            table.insert(r, vv)
        end
    end

    return r
end

local function length(xs)
    return #xs
end

local function reverse(xs)
    local r = {}

    for _, v in ipairs(xs) do
        table.insert(r, 1, v)
    end

    return r
end

local function foldl(xs, value, f)
    local r = value

    for i, v in ipairs(xs) do
        r = f(r, v)
    end

    return r
end

local function foldr(xs, value, f)
    local r = value

    for i, v in ipairs(reverse(xs)) do
        r = f(r, v)
    end

    return r
end

local function map(xs, f)
    local r = {}

    for _, v in ipairs(xs) do
        table.insert(r, f(v))
    end

    return r
end

local function filter(xs, pred)
    local r = {}

    for _, v in ipairs(xs) do
        if pred(v) then
            table.insert(r, v)
        end
    end

    return r
end

--for i, v in ipairs(concat({ 1, 2, 3 }, { 2, 3, 4 }, { 3, 4, 5 })) do
--    print(v)
--end

return {
    append = append,
    concat = concat,
    length = length,
    reverse = reverse,
    map = map,
    foldl = foldl,
    foldr = foldr,
    filter = filter
}
