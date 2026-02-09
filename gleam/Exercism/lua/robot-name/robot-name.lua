local abc = 'abcdefghijklmnopqrstuvwxyz'

local Robot = {
    new = function()
        local self = {
            name = '',

            genName = function()
                local name = ''

                for i = 1, 2 do
                    local rn = math.random(1, #abc)
                    name = name .. string.sub(abc, rn, rn)
                end

                for i = 1, 3 do
                    local rn = math.random(0, 9)
                    name = name .. tostring(rn)
                end

                return name:upper()
            end,

            reset = function(self)
                self.name = self.genName()
            end
        }

        self.name = self:genName()

        return self
    end,
}

--local one = Robot:new()
--local two = Robot:new()
--
--print(one.name, two.name)

return Robot
