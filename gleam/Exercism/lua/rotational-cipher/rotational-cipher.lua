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
        print('ch', ch, ch == ' ', type(ch))

        if ch == ' ' or type(ch) == 'number' then
            res = res .. ch
            goto continue
        end

        local curI = string.find(l, ch)

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

print(rotate('Testing 1 2 3 testing', 5))

return {
    rotate = rotate
}
