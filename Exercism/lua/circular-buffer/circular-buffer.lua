local CircularBuffer = {
    new = function(parent, n)
        local function adjN (x)
            if x > n then
                return 1
            end
            if x < 1 then
                return n
            end

            return x
        end

        local bbb = {
            space = {},

            wp = 1,
            rp = 1,

            write = function(self, el)
                if #self.space == n then
                    error('buffer is full')
                end

                self.space[self.wp] = el
                self.wp = adjN(self.wp + 1)
            end,

            forceWrite = function(self, el)
                if #self.space == n then
                    self.space[self.rp] = el
                    self.rp = adjN(self.rp + 1)

                    return
                end

                self.write(self, el)
            end,

            read = function(self)
                local t = self.space[self.rp]

                if #self.space == n then
                    self.wp = self.rp
                end

                if #self.space == 0 then
                    error('buffer is empty')
                end

                self.space[self.rp] = nil
                self.rp = adjN(self.rp + 1)

                return t
            end
        }

        return bbb
    end
}

local b = CircularBuffer:new(3)
b:write('1')
--print(b:read())
b:write('2')
--print(b:read())
b:write('3')
--b:write('11')
print(b:read())
--b:forceWrite('12')
--b:forceWrite('13')

--print(b.space[1], b.space[2], b.space[3], b.space[4], b.space[5], b.wp, b.rp)

return CircularBuffer
