local l = 'abcdefghijklmnopqrstuvwxyz'
local u = string.upper(l)

il = string.gmatch(l, '.')
iu = string.gmatch(u, '.')

local toTable = function(i)
    local t = {}

    for n in i do
        table.insert(t, n)
    end

    return t
end

local tl = toTable(il)
local tu = toTable(iu)

local rotate = function(input, key)
    local res = ''
    for ch in string.gmatch(input, '.') do
        --print('ch', ch, ch == ' ', type(ch))

        --if ch == ' ' or ch == '.' or tonumber(ch) ~= nil then
        if string.find(l, string.lower(ch)) == nil or ch == '.' then
            res = res .. ch
            goto continue
        end

        local curI = string.find(l, ch)
        --print(ch, curI)

        if curI ~= nil then
            local id = (curI + key) % #l
            if id == 0 then
                id = 26
            end

            res = res .. tl[id]
        else
            local curI = string.find(u, ch)

            local id = (curI + key) % #u

            if id == 0 then
                id = 26
            end

            res = res .. tu[id]
        end

        :: continue ::
    end

    return res
end

--print(rotate('Gur dhvpx oebja sbk whzcf bire gur ynml qbt.', 13))

return {
    rotate = rotate
}
