local function ss (numbers)
    local candi = {}
    candi.set = {}
    candi.numbers = numbers

    candi.to = function(max)
        for _, v in ipairs(candi.numbers) do
            for i = 1, max do
                local vv = v * i

                if vv >= max then
                    break
                end

                if candi.set[tostring(vv)] == nil then
                    candi.set[tostring(vv)] = 1
                end
            end
        end

        local x = 0
        for k, _ in pairs(candi.set) do
            x = x + tonumber(k)
        end

        return x
    end

    return candi
end

--print(ss({ 3 }).to(9))

return ss
