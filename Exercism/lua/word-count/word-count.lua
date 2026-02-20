local function count_words(s)
    local ss = (' ' .. s .. ' '):gsub('[^%w\'%s]', ' '):gsub("%s'", ' '):gsub("'%s", ' '):lower()
    local sss = {}

    for w in string.gmatch(ss, '([^%s]+)') do
        if sss[w] == nil then
            sss[w] = 1
        else
            sss[w] = sss[w] + 1
        end
    end

    return sss
end

--local r = count_words("'First: don't laugh. Then: don't cry. You're getting it.'")
--for k, v in pairs(r) do
--    print(k, v)
--end

return { count_words = count_words }
