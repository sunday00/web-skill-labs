local function flagExists(flags)
    local res = {}

    for _, v in ipairs(flags) do
        res[v:sub(2, 2)] = true
    end

    return res
end

local function isMatch (line, pat, flags)
    local mPatt = pat
    local mLine = line

    local res = false

    if flags['i'] then
        mPatt = string.lower(pat)
        mLine = string.lower(line)
    end

    if flags['x'] then
        res = mLine == mPatt

    else

        res = mLine:find(mPatt) ~= nil
    end

    if flags['v'] then
        return not res
    else
        return res
    end
end

local function g (options)
    local res = {}
    local flags = flagExists(options.flags)

    for _, file in ipairs(options.files) do
        local lineNo = 0
        for line in io.lines(file) do
            lineNo = lineNo + 1

            local m = isMatch(line, options.pattern, flags)
            if not m then
                goto continue
            end

            if flags['l'] then
                table.insert(res, file)
                goto fileContinue
            end

            local sub = ''
            if #options.files > 1 then
                sub = file .. ':'
            end

            if flags['n'] then
                sub = sub .. lineNo .. ':'
            end

            table.insert(res, sub .. line)

            :: continue ::
        end

        :: fileContinue ::
    end

    return res
end

--local r = g({
--    files = { 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt' },
--    flags = { '-i', '-n' },
--    pattern = 'who'
--})
--for _, v in ipairs(r) do
--    print(v)
--end

return g