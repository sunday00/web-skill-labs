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

            len = 0,

            write = function(self, el)
                if self.len == n then
                    error('buffer is full')
                end

                if el == nil then
                    self.space[self.wp] = nil
                    return
                end

                self.space[self.wp] = el
                self.len = self.len + 1

                self.wp = adjN(self.wp + 1)
            end,

            forceWrite = function(self, el)
                if self.len == n then
                    self.space[self.rp] = el
                    self.rp = adjN(self.rp + 1)

                    return
                end

                self.write(self, el)
            end,

            read = function(self)
                local t = self.space[self.rp]

                if self.len == n then
                    self.wp = self.rp
                end

                if self.len == 0 then
                    error('buffer is empty')
                end

                self.space[self.rp] = nil
                self.rp = adjN(self.rp + 1)
                self.len = self.len - 1

                return t
            end,

            clear = function(self)
                self.space = {}
                self.wp = 1
                self.rp = 1
                self.len = 0
            end
        }

        return bbb
    end
}

--local b = CircularBuffer:new(2)
--print('len0', b.len)
--b:write('1')
--print('len after add 1', b.len)
--b:forceWrite('2')
--print('len after add 2', b.len)
--print(b:read())
--print('len after read 1', b.len)
--print(b:read())
--print('len after read 2', b.len)
--print(b:read())
--print('len after read empty', b.len)

return CircularBuffer
