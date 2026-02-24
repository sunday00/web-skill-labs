return function(n)
    if n == 1 then
        return 0
    end

    if n <= 0 then
        error()
    end

    local st = 0
    while (n > 1) do
        if n % 2 == 0 then
            n = n / 2
        else
            n = n * 3 + 1
        end

        st = st + 1
    end

    return st
end
