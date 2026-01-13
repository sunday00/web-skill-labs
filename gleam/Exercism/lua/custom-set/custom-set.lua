local function init (raw)
    local els = {}
    for _, v in ipairs(raw) do
        local existed = false
        for _, ex in ipairs(els) do
            if ex == v then
                existed = existed or true
            end
        end

        if not existed then
            table.insert(els, v)
        end
    end

    return els
end

local function clone (t)
    local c = {}

    for i = 1, #t do
        c[i] = t[i]
    end

    return c
end

function Set(...)
    local ttt = init({ ... })

    local o = {
        values = ttt,
        tmp = clone(ttt),
        size = #ttt,
    }

    function o:toString()
        local s = ''
        for i, v in ipairs(self.values) do
            s = s .. v

            if self.values[i + 1] ~= nil then
                s = s .. ', '
            end
        end

        return s
    end

    function o:is_empty ()
        return self.size == 0
    end

    function o:contains (el)
        for _, v in ipairs(self.values) do
            if v == el then
                return true
            end
        end

        return false
    end

    function o:is_subset (maybeParent)
        if self.size == 0 then
            return true
        end

        local allContains = true
        for _, v in ipairs(self.values) do
            allContains = allContains and maybeParent:contains(v)
        end

        return allContains
    end

    function o:is_disjoint (other)
        if self.size == 0 or other.size == 0 then
            return true
        end

        local exist = false
        for _, v in ipairs(self.values) do
            for _, vv in ipairs(other.values) do
                if v == vv then
                    exist = exist or true
                end
            end
        end

        return not exist
    end

    function o:equals (other)
        if self.size == 0 and other.size == 0 then
            return true
        end

        if self.size ~= other.size then
            return false
        end

        table.sort(self.tmp)
        table.sort(other.tmp)

        for i = 1, self.size do
            if self.tmp[i] ~= other.tmp[i] then
                return false
            end
        end

        return true
    end

    function o:add (el)
        if self:contains(el) then
            return self.size
        end

        self.size = self.size + 1
        self.values[self.size] = el
        self.tmp[self.size] = el
    end

    function o:intersection(other)
        local s = Set()

        if self.size == 0 or other.size == 0 then
            return s
        end

        for _, v in ipairs(self.values) do
            if other:contains(v) then
                s:add(v)
            end
        end

        return s
    end

    function o:difference(other)
        local s = Set()

        if self.size == 0 then
            return s
        end

        for _, v in ipairs(self.values) do
            if not other:contains(v) then
                s:add(v)
            end
        end

        return s
    end

    function o:union(other)
        local s = Set()

        for _, v in ipairs(self.values) do
            s:add(v)
        end

        for _, v in ipairs(other.values) do
            s:add(v)
        end

        return s
    end

    return o
end

return Set