local School = {
    new = function(parent)
        return {
            list = {  },
            add = function(self, name, gr)
                for _, v in ipairs(self.list) do
                    if v.name == name then
                        return false
                    end
                end

                table.insert(self.list, { name = name, gr = gr })

                return true
            end,

            grade = function(self, gr)
                local r = {}

                for _, v in ipairs(self.list) do
                    if v.gr == gr then
                        table.insert(r, v.name)
                    end
                end

                table.sort(r)

                return r
            end,

            roster = function(self)
                table.sort(self.list, function(a, b)
                    if a.gr == b.gr then
                        return a.name < b.name
                    end

                    return a.gr < b.gr
                end)

                local r = {}

                for _, v in ipairs(self.list) do
                    table.insert(r, v.name)
                end

                return r
            end
        }
    end
}

return School
