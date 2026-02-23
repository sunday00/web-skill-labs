local function sol(s)
    local mat = {}

    for v in string.gmatch(s, '[^\n]+') do
        local row = {}
        for vv in string.gmatch(v, '[^%s]+') do
            table.insert(row, tonumber(vv))
        end

        table.insert(mat, row)
    end

    return {
        row = function(n)
            return mat[n]
        end,

        column = function(n)
            local col = {}

            for _, v in ipairs(mat) do
                table.insert(col, v[n])
            end

            return col
        end
    }
end

return sol