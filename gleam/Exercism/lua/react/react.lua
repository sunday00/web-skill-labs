local function Reactor()
    local acts = {}
    local calculated = nil

    local ins = {
        InputCell = function(n)
            local subIns = {
                v = nil
            }

            subIns.v = n

            subIns.get_value = function()
                return subIns.v
            end

            subIns.set_value = function(nn)
                subIns.v = nn

                --

                if #acts == 0 then return end

                -- TODO:
                -- WORK: here
                calculated = n * 100
            end

            return subIns
        end,

        ComputeCell = function(...)
            local args = {...}
            table.insert(acts, args[#args])

            local v = {}

            for i=1, #args - 1 do
                table.insert(v, args[i].get_value())
            end

            calculated = args[#args](table.unpack(v))

            return { get_value = function() return calculated end }
        end
    }

    return ins
end

local r = Reactor()
local input = r.InputCell(1)
local times_two = r.ComputeCell(input, function(x)
    return x * 2
end)

local times_thirty = r.ComputeCell(input, function(x)
    return x * 30
end)

local output = r.ComputeCell(times_two, times_thirty, function(x, y)
    return x + y
end)

input.set_value(3)

print(output.get_value())

return { Reactor = Reactor }
