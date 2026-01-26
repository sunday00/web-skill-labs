local function getFactorsInRange(x, min, max)
    local f = {}

    for i = min, max do
        if i > 1 and i > (x / 2) then
            break
        end

        for ii = i, max do
            if i > 1 and ii > 1 and ii > (x / 2) then
                break
            end

            if i * ii == x then
                table.insert(f, { i, ii })
            end
        end
    end

    return f
end

local function smallest(min, max)
    if min > max then
        error('min must be <= max')
    end

    local value = nil

    for i = min, max do
        for ii = i, max do
            local x = tostring(i * ii)

            for xi = 1, math.floor(#x / 2) do
                if x:sub(xi, xi) ~= x:sub(#x - (xi) + 1, #x - (xi) + 1) then
                    goto continue
                end
            end

            if value == nil or value > i * ii then
                value = i * ii
            end

            :: continue ::
        end
    end

    if value == nil then
        return { value = nil, factors = {} }
    end

    local f = getFactorsInRange(value, min, max)

    return { value = value, factors = f }
end

local function largest(min, max)
    if min > max then
        error('min must be <= max')
    end

    local value = nil

    for i = max, min, -1 do
        for ii = i, min, -1 do
            local x = tostring(i * ii)

            for xi = 1, math.floor(#x / 2) do
                if x:sub(xi, xi) ~= x:sub(#x - (xi) + 1, #x - (xi) + 1) then
                    goto continue
                end
            end

            if value == nil or value < i * ii then
                value = i * ii
            end

            :: continue ::
        end
    end

    if value == nil then
        return { value = nil, factors = {} }
    end

    local f = getFactorsInRange(value, min, max)

    return { value = value, factors = f }
end

local r = largest(1000, 9999)
print(r.value, r.factors[1])

return { smallest = smallest, largest = largest }
