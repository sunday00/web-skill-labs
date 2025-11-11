-- utils

local dictionary = {}

local function contains(table, target)
    for _, v in ipairs(table) do
        if v == target then
            return true
        end
    end

    return false
end

local function cloneTree(tree)
    local clone = {}
    for k, v in pairs(tree) do
        if type(v) == 'table' then
            clone[k] = cloneTree(v)
        else
            clone[k] = v
        end
    end
    return clone
end

local function toNamedTable(tree)
    if type(tree[1]) == 'string' and type(tree[2]) == 'table' then
        dictionary[tree[1]] = tree[2]

        local r = {}
        r[tree[1]] = toNamedTable(tree[2])
        return r
    end

    local r = {}

    for _, children in ipairs(tree) do
        if type(children[1]) == 'string' and type(children[2]) == 'table' then
            dictionary[children[1]] = children[2]

            r[children[1]] = toNamedTable(children[2])

            else

            table.insert(r, children[1])
        end
    end

    return r
end

local function reTree(tree, target, acc)
    if type(tree) == 'table' then
        for n, ch in pairs(tree) do
            if n == target or (type(ch) == 'table' and contains(ch, target)) then
                local removeSelf = {}
                for i = 1, #ch do
                    if ch[i] ~= target then
                        table.insert(removeSelf, ch[i])
                    end
                end

                local x = removeSelf

            end

            acc[n] = ch
            return reTree(ch, target, acc)
        end
    end

end

-- logic start

-- pov....

local function pov_from(target)
    return {
        of = function(tree)


            local namedTable = toNamedTable(tree)

            local newTree = reTree(namedTable, target, {})

            return newTree
        end
    }
end

-- path....



local function path_from(target)

end

-- test

local res = pov_from('leaf').of({ 'grand_parent', {
    { 'parent', {
        { 'sibling_1' }, { 'leaf' }, { 'sibling_2' } } },
    { 'uncle', {
        { 'cousin_1' }, { 'cousin_2' } } } } })

for _, r in ipairs(res) do
    print(r)
end

return { pov_from = pov_from, path_from = path_from }
