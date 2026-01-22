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

function string.replace(s, prev, after)
    return string.gsub(s, prev, after)
end

local rne = {
    encode = function(s)
        local res = ''
        local rv = ''
        local rc = 0

        for i, v in ipairs(string.split(s)) do
            if rv ~= v then
                if rc ~= 1 and rc ~= 0 then
                    res = res .. tostring(rc) .. rv
                else
                    res = res .. rv
                end

                rv = v
                rc = 1
            else
                rc = rc + 1
            end

            if i == #s then
                if rc ~= 1 and rc ~= 0 then
                    res = res .. tostring(rc) .. rv
                else
                    res = res .. rv
                end
            end
        end

        return res
    end,
    decode = function(s)
        local res = ''
        local rc = ''

        for i, v in ipairs(string.split(s)) do
            if tonumber(v) ~= nil then
                rc = rc .. v
                goto continue
            end

            if rc == 0 then
                res = res .. v

            else
                res = res .. string.rep(v, tonumber(rc))
                rc = 0
            end

            :: continue ::
        end

        return res
    end
}

--local er = rne.encode('AABCCCCD')
--print(er)
--
--local er = rne.decode('2A10B4C')
--print(er)

return rne