local function Reactor()
    local acts = {}
    local GArgs = {}
    local cbs = {}

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
                if subIns.v == nn then return end

                local prev
                for ii = 1, #acts do
                    prev = acts[ii](table.unpack(GArgs[ii]))
                end

                subIns.v = nn

                --

                if #acts == 0 then return end

                for i = 1, #cbs do
                    local isUpdated = false
                    local n

                    for ii = 1, #acts do
                        n = acts[ii](table.unpack(GArgs[ii]))
                    end

                    if prev ~= n then cbs[i](n) end
                end
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
            table.insert(GArgs, v)

            return {
                get_value = function() return calculator(table.unpack(v)) end,
                watch = function(cb) table.insert(cbs, cb) end,
                unwatch = function(cb)
                    for i = 1, #cbs do
                        if cbs[i] == cb then table.remove(cbs, i) end
                    end
                end
            }
        end
    }

    return ins
end

return { Reactor = Reactor }
