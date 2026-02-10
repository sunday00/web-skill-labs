local function sol ()
    local id = 1

    return {
        l = {},

        head = nil,
        tail = nil,
        len = 0,

        push = function(self, x)
            local next = {
                id = id,
                value = x,
                prev = nil,
                next = nil
            }

            if self.tail ~= nil then
                self.tail.next = id
                next.prev = self.tail.id
            end

            self.l['id_' .. id] = next
            self.tail = next

            if self.head == nil then
                self.head = next
            end

            id = id + 1
            self.len = self.len + 1
        end,

        get = function(self, lId)
            if lId == nil then
                return nil
            end

            return self.l['id_' .. lId]
        end,

        pop = function(self)
            local oLast = self.tail.value
            local nLast = self.get(self, self.tail.prev)

            self.len = self.len - 1
            self.l['id_' .. self.tail.id] = nil
            self.tail = nLast

            return oLast
        end,

        shift = function(self)
            local oFirst = self.head.value
            local nFirst = self.get(self, self.head.next)

            self.len = self.len - 1
            self.l['id_' .. self.head.id] = nil
            self.head = nFirst

            return oFirst
        end,

        unshift = function(self, x)
            local prev = {
                id = id,
                value = x,
                prev = nil,
                next = nil
            }

            if self.head ~= nil then
                self.head.prev = id
                prev.next = self.head.id
            end

            self.l['id_' .. id] = prev
            self.head = prev

            if self.tail == nil then
                self.tail = prev
            end

            id = id + 1
            self.len = self.len + 1
        end,

        delete = function(self, x)
            local cur = self.head

            while (true) do
                if cur == nil then
                    break
                end

                if cur.value == x then
                    if cur.prev ~= nil and cur.next ~= nil then
                        self.l['id_' .. cur.prev].next = self.l['id_' .. cur.next].id
                        self.l['id_' .. cur.next].prev = self.l['id_' .. cur.prev].id
                    elseif cur.prev ~= nil then
                        self.l['id_' .. cur.prev].next = nil
                    elseif cur.next ~= nil then
                        self.l['id_' .. cur.next].prev = nil
                    end

                    self.l['id_' .. cur.id] = nil

                    self.len = self.len - 1

                    if self.head.id == cur.id then
                        self.head = self.get(self, cur.next)
                    end

                    if self.tail.id == cur.id then
                        self.tail = self.get(self, cur.prev)
                    end

                    break
                end

                cur = self.get(self, cur.next)
            end
        end,

        count = function(self)
            return self.len
        end,
    }
end

--local r = sol()
--r:push(8)
--r:delete(7)
--print(r:count())

--r:push(3)
--r:push(8)
--
--print('==========')
--for k, v in pairs(r.l) do
--    print(k, v.value, v.next)
--end
--
--r:shift()
--
--print('==========')
--print(r.head.value)
--for k, v in pairs(r.l) do
--    print(k, v.value, v.next)
--end
--
--r:unshift(75)
--
--print('==========')
--print(r.head.value)
--for k, v in pairs(r.l) do
--    print(k, v.value, v.next)
--end

return sol