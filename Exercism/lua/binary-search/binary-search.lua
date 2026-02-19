--local TracedArray = require('TracedArray')

local function sol(array, target)
    if #array == 0 then
        return -1
    end

    local cur = 0

    while (#array > 1) do
        if array[1] == target then
            return cur
        end

        local midId = math.ceil(#array / 2)

        print(target, array[midId], ":", array[1], array[2], array[3], array[4], array[5], array[6], array[7], array[8], array[9], array[10], array[11], array[12])

        if array[midId] == target then
            return cur + midId
        elseif array[midId] < target then
            for i = 1, midId do
                cur = cur + 1
                table.remove(array, 1)
            end
        else
            for i = midId, #array do
                table.remove(array)
            end
        end
    end

    return -1
end

--local arr = TracedArray { 6 }
--local r = sol(arr, 6)
--print(r, arr.access_count)

local r = sol({ 94 }, 602)
print(r)

return sol