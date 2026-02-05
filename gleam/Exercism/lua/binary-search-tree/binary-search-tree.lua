local BinarySearchTree = {
    value = nil,
    left = nil,
    right = nil,

    new = function(_, n)

        local s = {
            value = n,
            left = nil,
            right = nil,
            nestedInsert = function(self, t, wh, child)
                if t[wh] == nil then
                    t[wh] = child

                elseif child.value <= t[wh].value then
                    self.nestedInsert(self, t[wh], 'left', child)
                elseif child.value > t[wh].value then
                    self.nestedInsert(self, t[wh], 'right', child)
                end
            end,

            insert = function(self, insetV)
                local child = { value = insetV, left = nil, right = nil }

                if insetV <= self.value then
                    self.nestedInsert(self, self, 'left', child)
                end

                if insetV > self.value then
                    self.nestedInsert(self, self, 'right', child)
                end
            end,

            values = function(self)
                local v = { self.value }

                local q = { self.left, self.right }
                while (#q > 0) do
                    local cur = table.remove(q, 1)

                    if cur ~= nil then
                        table.insert(v, cur.value)
                        table.insert(q, cur.left)
                        table.insert(q, cur.right)
                    end
                end

                table.sort(v)

                return ipairs(v)
            end
        }

        return s
    end,

    from_list = function(self, l)
        if #l == 0 then
            error()
        end

        local f = table.remove(l, 1)
        local bst = self.new(self, f)

        for _, n in ipairs(l) do
            bst:insert(n)
        end

        return bst
    end,
}

--local tree = BinarySearchTree:from_list({ 2, 4, 6, 3, 5, 1 })
--local l = tree:values()
--for _, item in ipairs(l) do
--    print(item)
--end

--tree:insert(6)
--tree:insert(3)
--tree:insert(5)
--print(tree.value)
--print(tree.right.value)
--print(tree.right.left.value)

return BinarySearchTree
