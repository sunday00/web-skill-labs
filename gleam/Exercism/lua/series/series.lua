local function validate (s, length)
    if #s == 0 then
        error('series cannot be empty')
    end

    if length == 0 then
        error('slice length cannot be zero')
    elseif length < 0 then
        error('slice length cannot be negative')
    end

    if length > #s then
        error('slice length cannot be greater than series length')
    end
end

local function sol (s, length)
    validate(s, length)

    local r = {}

    for i = 1, #s - length + 1 do
        table.insert(r, string.sub(s, i, i + length - 1))
    end

    local i = 0
    return function()
        i = i + 1
        return r[i]
    end
end

--local r = sol("918493904243", 5)
--for v in r do
--    print(v)
--end

return sol