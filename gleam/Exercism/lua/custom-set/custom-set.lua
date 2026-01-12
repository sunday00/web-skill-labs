local function init (raw)
    local els = {}
    for _, v in ipairs(raw) do
        local existed = false
        for _, ex in ipairs(els) do
            if ex == v then
                existed = existed or true
            end
        end

        if not existed then
            table.insert(els, v)
        end
    end

    return els
end

function Set(...)
    local els = init({ ... })

    return {
        values = els,

        is_empty = function()
            return #els == 0
        end,

        contains = function(el)
            local res = false
            for _, v in ipairs(els) do
                if v == el then
                    return true
                end
            end

            return res
        end,

        is_subset = function(maybeParent)
            if #els == 0 then
                return true
            end

            for _, v in ipairs(els) do
                local exist = false
                for _, vv in ipairs(maybeParent.values) do
                    if v == vv then
                        exist = exist or true
                    end
                end

                if not exist then
                    return false
                end

                return true
            end
        end
    }
end

local s = Set(1, 2, 3)

print(s.is_subset(Set(1, 2, 3, 4)))
print(s.is_subset(Set(2, 3, 4, 5)))

return Set