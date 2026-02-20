local dirToInt = {
    north = 1,
    east = 2,
    south = 3,
    west = 4
}

local turnToInt = {
    R = 1,
    L = -1
}

local intToDir = {
    { 'y', 1, 'north', },
    { 'x', 1, 'east', },
    { 'y', -1, 'south', },
    { 'x', -1, 'west' }
}

local function sol(config)
    local ins = {
        moveProc = function(self, ch)
            if ch == 'R' or ch == 'L' then
                self.headingInt = self.headingInt + turnToInt[ch]
                if self.headingInt > 4 then
                    self.headingInt = self.headingInt - 4
                elseif self.headingInt < 1 then
                    self.headingInt = self.headingInt + 4
                end
            elseif ch == 'A' then
                local to = intToDir[self.headingInt]
                self[to[1]] = self[to[1]] + to[2]
            else
                error()
            end
        end,

        move = function(self, commands)
            for i = 1, #commands do
                local command = string.sub(commands, i, i)
                self.moveProc(self, command)
            end

            self.heading = intToDir[self.headingInt][3]
        end
    }

    ins.x = config.x
    ins.y = config.y
    ins.heading = config.heading
    ins.headingInt = dirToInt[config.heading]

    return ins
end

--local robot = sol { x = 5, y = 10, heading = 'north' }
--robot:move('ARALZR')

return sol