local sol = {
    transform = function(dataset)
        local dic = {}

        for i, v in pairs(dataset) do
            for _, vv in ipairs(v) do
                dic[string.lower(vv)] = i
            end
        end

        return dic
    end
}

return sol