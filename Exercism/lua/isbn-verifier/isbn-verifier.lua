return {
    valid = function(isbn)
        --local s = string.gsub(isbn, 'X$', '\1'):gsub('[^%d\1]', ''):gsub('\1', 'X')
        local s = string.gsub(isbn, '-', '')

        if #s ~= 10 then
            return false
        end

        local sum = 0
        for i = 1, #s do
            local ch = string.sub(s, i, i)
            if ch == 'X' then
                if i == 10 then
                    ch = '10'
                else
                    return false
                end
            end

            local no = tonumber(ch)

            if no == nil then
                return false
            end

            local mul = 10 - (i - 1)

            sum = sum + no * mul
        end

        return sum % 11 == 0
    end
}
