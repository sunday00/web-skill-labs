return function(s)
    local ans = ''

    for i = #s, 1, -1 do
        ans = ans .. string.sub(s, i, i)
    end

    return ans
end
