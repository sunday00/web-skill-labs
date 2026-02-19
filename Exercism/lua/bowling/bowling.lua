local function br ()
    local downs = {}

    local function normalFr (pins)
        local last = downs[#downs]

        if last == nil or #last == 2 or last[1] == 10 then
            table.insert(downs, { pins })
        elseif last ~= nil and last[1] ~= 10 then
            table.insert(downs[#downs], pins)
        end

        if downs[#downs][1] ~= nil and downs[#downs][2] and downs[#downs][1] + downs[#downs][2] > 10 then
            error()
        end
    end

    local function tenthFr(pins)
        if downs[10][1] == 10 then
            if downs[10][2] ~= nil and downs[10][2] ~= 10 and downs[10][2] + pins > 10 then
                error()
            end
        end

        if downs[10][1] ~= nil and downs[10][2] ~= nil and (downs[10][1] + downs[10][2]) < 10 then
            error()
        end

        if downs[10][1] ~= nil and downs[10][2] ~= nil and downs[10][3] ~= nil then
            error()
        end

        table.insert(downs[10], pins)
    end

    local function isFinishedOnThrow ()
        if #downs < 10 or (#downs == 10 and #downs[10] < 2) then
            error()
        end

        if #downs == 10 and ((downs[10][1] == 10 or (downs[10][1] + downs[10][2] == 10)) and downs[10][3] == nil) then
            error()
        end
    end

    local function getBonusScore (dd, i)
        if i >= 10 then
            return 0
        end

        if dd[1] == 10 then
            if downs[i + 1][1] == 10 and i < 9 then
                return downs[i + 1][1] + downs[i + 2][1]
            else
                return downs[i + 1][1] + downs[i + 1][2]
            end

        elseif dd[1] + (dd[2] or 0) == 10 then
            return downs[i + 1][1]
        end

        return 0
    end

    return {
        roll = function(pins)
            if pins > 10 or pins < 0 then
                error()
            end

            if #downs < 10 then
                return normalFr(pins)
            else
                return tenthFr(pins)
            end
        end,

        score = function()
            isFinishedOnThrow()

            local sum = 0
            for i, dd in ipairs(downs) do
                --print(dd[1], dd[2], dd[3])

                sum = sum + getBonusScore(dd, i)

                sum = sum + dd[1]

                if dd[2] ~= nil then
                    sum = sum + dd[2]
                end

                if dd[3] ~= nil then
                    sum = sum + dd[3]
                end
            end

            return sum
        end
    }
end

return br