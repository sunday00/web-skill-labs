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

            end

            return subIns
        end,

        ComputeCell = function(...)
            local args = {...}
            local v = {}

            for i=1, #args - 1 do
                table.insert(v, args[i])
            end

            local action =  args[#args]
            local calculator = function(...)
                local localArgs = {...}
                local localV = {}

                for i=1, #localArgs do
                    table.insert(localV, localArgs[i].get_value())
                end

                return action(table.unpack(localV))
            end

            table.insert(acts, calculator)

            return { get_value = function() return calculator(table.unpack(v)) end }
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
