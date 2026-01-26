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

local function lsp (config)
    if tonumber(config.digits) == nil then
        error()
    end

    if #config.digits < config.span then
        error()
    end

    if config.span < 0 then
        error()
    end

    local lg = 0

    for i = 0, #config.digits - config.span do
        local prod = string.sub(config.digits, i + 1, config.span + i)

        local prodS = string.split(prod)

        local ans = 1
        for _, v in ipairs(prodS) do
            ans = ans * tonumber(v)
        end

        if lg < ans then
            lg = ans
        end
    end

    return lg
end

--local r = lsp({ digits = '0123456789', span = 3 })
--
--print(r)

return lsp