local CircularBuffer = {
    new = function(n)
        local bbb = {
            space = {},

            wp = 1,
            rp = 1,

            write = function(self, el)
                table.insert(self, el)
                self.wp = self.wp + 1
            end
        }

        return bbb
    end
}

local b = CircularBuffer:new(4)
b:write('8')
b:write('9')
b:write('10')

print(b.space[1])

return CircularBuffer
