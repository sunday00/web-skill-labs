local function findFirst(cur, next, prisms)
    local deg = cur + next.angle
    local angleRad = math.rad(deg)

    local dirX = math.cos(angleRad)
    local dirY = math.sin(angleRad)

    local epsilon = 0.01 -- for floating dot number fix
    local firstTarget = nil
    local minDistance = math.huge -- infinite

    for i, t in ipairs(prisms) do
        local vecX = t.x - next.x
        local vecY = t.y - next.y

        local dot = (vecX * dirX) + (vecY * dirY)
        local perpDist = math.abs(vecX * (-dirY) + vecY * dirX)

        if perpDist > epsilon then
            goto continue
        end

        if dot > 0 then
            local distSq = (vecX * vecX) + (vecY * vecY)

            if distSq < minDistance then
                minDistance = distSq
                firstTarget = t
            end
        end

        :: continue ::
    end

    return firstTarget, deg
end

local function find_sequence(start, prisms)
    local res = {}

    local point = start
    local curDeg = 0
    while (#res <= #prisms) do
        local t, deg = findFirst(curDeg, point, prisms)

        if t == nil then
            break
        end

        table.insert(res, t.id)

        point = t
        curDeg = deg
    end

    return res
end

--local r = find_sequence({ x = 0, y = 0, angle = 0 }, { { id = 1, x = 10, y = 10, angle = 0 }, { id = 2, x = 10, y = 0, angle = 90 } })
--print(r[1], r[2])

return { find_sequence = find_sequence }
