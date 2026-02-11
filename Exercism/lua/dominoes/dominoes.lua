local function can_chain(dominoes)
    if #dominoes == 0 then
        return true
    elseif #dominoes == 1 and dominoes[1][1] ~= dominoes[1][2] then
        return false
    end

    local processes = {}

    for i, v in ipairs(dominoes) do
        local pros = { pros = { v }, remains = {} }

        for ii, dom in ipairs(dominoes) do
            if i ~= ii then
                table.insert(pros.remains, dom)
            end
        end

        table.insert(processes, pros)
    end

    while (#processes > 0) do
        local cur = table.remove(processes, #processes)
        local curLast = cur.pros[#cur.pros]

        if #cur.remains == 0 and (
                cur.pros[1][1] == cur.pros[#dominoes][2]
                        or cur.pros[1][1] == cur.pros[#dominoes][1]
                        or cur.pros[1][2] == cur.pros[#dominoes][1]
                        or cur.pros[1][2] == cur.pros[#dominoes][2]
        ) and #cur.pros == #dominoes then
            return true
        end

        for i, v in ipairs(cur.remains) do
            if curLast[1] == v[1] or curLast[1] == v[2] or curLast[2] == v[1] or curLast[2] == v[2] then
                local pros = { pros = {}, remains = {} }

                for _, cpros in ipairs(cur.pros) do
                    table.insert(pros.pros, cpros)
                end

                table.insert(pros.pros, v)

                for ii, dom in ipairs(cur.remains) do
                    if i ~= ii then
                        table.insert(pros.remains, dom)
                    end
                end

                table.insert(processes, pros)
            end
        end
    end

    return false
end

--print(can_chain({ { 1, 2 } }))

return { can_chain = can_chain }
