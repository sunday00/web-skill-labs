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

    if flags['i'] then
        mPatt = string.lower(pat)
        mLine = string.lower(line)
    end

    if flags['x'] then
        return mLine == mPatt
    end

    return mLine:find(mPatt) ~= nil
end

local function g (options)
    local res = {}
    local flags = flagExists(options.flags)

    for _, file in ipairs(options.files) do
        local lineNo = 0
        for line in io.lines(file) do
            lineNo = lineNo + 1

            local m = isMatch(line, options.pattern, flags)

        end
    end

    return res
end

local r = g({
    files = { 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt' },
    flags = { '-i' },
    pattern = 'who'
})
for _, v in ipairs(r) do
    print(v)
end

return g