local function smallest(min, max)
    local value = nil
    local x1 = nil
    local x2 = nil

    for i = min, max do
        for ii = i, max do
            local x = tostring(i * ii)

            for xi = 1, math.floor(#x / 2) do
                if x[xi] ~= x[#x - (xi) + 1] then
                    goto continue
                end
            end

            value = i * ii
            x1 = i
            x2 = ii

            print(value)

            goto breakOut

            :: continue ::
        end
    end

    :: breakOut ::

    print(value, x1, x2)
end

local function largest(min, max)

end

local r = smallest(1000, 9999)
print(r)

return { smallest = smallest, largest = largest }
