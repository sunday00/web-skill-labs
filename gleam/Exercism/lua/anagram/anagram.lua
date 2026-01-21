function string.split (inputstr, sep)
    if sep == nil then
        sep = '.'
    else
        sep = '([^' .. sep .. ']+)'
    end

    local t = {}
    for str in string.gmatch(inputstr, sep)
    do
        table.insert(t, str)
    end

    return t
end

local Anagram = {
    pure = '',
    source = {},
}

function Anagram:new (source)
    self.pure = string.lower(source)
    self.source = string.split(string.lower(source))
    table.sort(self.source)

    return self
end

function Anagram:match(candis)
    local r = {}

    for _, v in ipairs(candis) do
        if #v ~= #self.source or string.lower(v) == self.pure then
            goto continue
        end

        local c = string.split(string.lower(v))
        table.sort(c)

        for i, cv in ipairs(c) do
            if cv ~= self.source[i] then
                goto continue
            end
        end

        table.insert(r, v)

        :: continue ::
    end

    return r
end

--local a = Anagram:new('BANANA')
--local r = a:match({ 'BANANA' })
--
--for i, v in ipairs(r) do
--    print(v)
--end

return Anagram
