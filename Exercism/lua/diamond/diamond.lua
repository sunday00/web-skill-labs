local function sol (which)
    -- 65: A
    -- 70: F
    -- 75: K
    -- 80: P
    -- 85: U
    -- 90: Z

    --print(which:byte() - 65)

    local maxAscii = which:byte()

    local lines = {}
    local r = ''

    for i = 65, maxAscii do
        local letter = string.char(i)
        local untrimCnt = maxAscii - i
        local middleCnt = 0

        if i == 65 then
            middleCnt = 0
        elseif i == 66 then
            middleCnt = 1
        else
            middleCnt = 1 + (2 * (i - 66))
        end

        local line = ''
        if i == 65 then
            line = string.rep(' ', untrimCnt) .. 'A' .. string.rep(' ', untrimCnt)
        else
            line = string.rep(' ', untrimCnt) .. string.char(i) .. string.rep(' ', middleCnt) .. string.char(i) .. string.rep(' ', untrimCnt)
        end

        r = r .. line .. '\n'

        if i ~= maxAscii then
            table.insert(lines, line)
        end

    end

    if #lines > 0 then
        for i = #lines, 1, -1 do
            r = r .. lines[i] .. '\n'
        end
    end

    return r
end

--sol('A')
--sol('B')
--sol('C')
--print(sol('E'))
--sol('Z')

return sol