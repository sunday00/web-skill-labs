return function(pos)
    if pos.row < 0 or pos.column < 0 then
        error()
    end

    if pos.row > 7 or pos.column > 7 then
        error()
    end

    local p = pos

    return {
        pos = pos,
        can_attack = function(enemy)
            if p.row == enemy.pos.row or p.column == enemy.pos.column then
                return true
            end

            if math.abs(p.row - enemy.pos.row) == math.abs(p.column - enemy.pos.column) then
                return true
            end

            return false
        end
    }
end
