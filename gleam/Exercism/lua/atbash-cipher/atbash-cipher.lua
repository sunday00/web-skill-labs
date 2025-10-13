local o = {
    encode = function(plaintext)
        local abc = 'abcdefghijklmnopqrstuvwxyz'
        local cba = string.gmatch(string.reverse(abc), '.')

        local toTable = function(i)
            local t = {}

            for n in i do
                table.insert(t, n)
            end

            return t
        end

        cba = toTable(cba)

        local res = ''
        for ch in string.gmatch(plaintext, '.') do
            if ch == ' ' or ch == ',' or ch == '.' then
                res = res
            else

                local chi = string.find(abc, string.lower(ch))

                if chi ~= nil then
                    if (#res + 1) % 6 == 0 then
                        res = res .. ' ' .. string.lower(cba[chi])
                    else
                        res = res .. string.lower(cba[chi])
                    end
                else
                    res = res .. ch
                end
            end


        end

        return res
    end
}

--print(o.encode('The quick brown fox jumps over the lazy dog.'))

return o