local select = function(n, ...)
    local t = table.pack(...)

    for _, v in ipairs(t) do
        print('v', v)
    end

    return t[n]
end

local function primes_from(co)
    local primes = {}

    while true do
        local _, prime = coroutine.resume(co)

        if prime == nil then
            return primes
        end
        table.insert(primes, prime)
    end
end

local function s(n)
    local candis = {}
    for i = 2, n do
        table.insert(candis, { n = i, marked = false })
    end

    return coroutine.create(function()
        for _, c in ipairs(candis) do
            if not c.marked then
                for _, m in ipairs(candis) do
                    if m.n % c.n == 0 then
                        m.marked = true
                    end
                end

                coroutine.yield(c.n)
            end

        end
    end)
end

--local r = s(10)
--select(2, coroutine.resume(r))

--local rr = primes_from(r)
--for _, v in ipairs(rr) do
--    print(v)
--end

return s