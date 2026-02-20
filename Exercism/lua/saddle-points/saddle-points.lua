local function sol (matrix)
    local res = {}

    for i, v in ipairs(matrix) do
        local max = 0
        local targets = {}

        for y, vv in ipairs(v) do
            if vv == max then
                table.insert(targets, { y, i })
            elseif vv > max then
                targets = { { y, i } }
                max = vv
            end
        end

        for ti, vv in ipairs(targets) do
            local t = matrix[vv[2]][vv[1]]

            for _, row in ipairs(matrix) do
                local candy = row[vv[1]]

                if candy < t then
                    table.remove(targets, ti)
                    goto continue
                end
            end

            table.insert(res, { row = vv[2], column = vv[1] })

            :: continue ::
        end
    end

    return res
end

return sol