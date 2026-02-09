local function sol(n)
    if n == 1 then
        return 2
    elseif n == 2 then
        return 3
    elseif n < 1 then
        error()
    end

    local primes = { 2, 3 }
    local curI = 3
    local curV = 5
    while (curI <= n) do
        for i = 1, #primes do
            local prime = primes[i]
            if prime > math.sqrt(curV) then
                break
            end

            if curV % prime == 0 then
                goto continue
            end
        end

        if curI == n then
            break
        end

        table.insert(primes, curV)
        curI = curI + 1

        :: continue ::

        curV = curV + 2
    end

    return curV
end

--print(sol(10001))

return sol