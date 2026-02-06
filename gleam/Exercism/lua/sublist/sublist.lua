local function sol (l1, l2)
    if #l1 == 0 and #l2 == 0 then
        return 'equal'
    elseif #l1 == 0 and #l2 ~= 0 then
        return 'sublist'
    elseif #l1 ~= 0 and #l2 == 0 then
        return 'superlist'
    end

    local l1s = ''
    local l2s = ''

    for i = 1, math.max(#l1, #l2) do
        if l1[i] ~= nil then
            l1s = l1s .. tostring(l1[i]) .. ','
        end
        if l2[i] ~= nil then
            l2s = l2s .. tostring(l2[i]) .. ','
        end
    end

    if l1s == l2s then
        return 'equal'
    elseif string.find(l1s, l2s) ~= nil then
        return 'superlist'
    elseif string.find(l2s, l1s) ~= nil then
        return 'sublist'
    end

    return 'unequal'
end

--print(sol({ 1, 2, 5 }, { 0, 1, 2, 3, 1, 2, 5, 6 }))

return sol