-- utils

local nameDic = {}
local ups = {}

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

local function povReduce(tree, parent, target)
    if #tree == 2 and type(tree[1]) == 'string' and type(tree[2]) == 'table' then
        nameDic[tree[1]] = tree[2]

        povReduce(tree[2], tree[1])
    end

    for _, ch in ipairs(tree) do
        if #ch == 2 and type(ch[1]) == 'string' and type(ch[2]) == 'table' then
            nameDic[ch[1]] = ch[2]
            ups[ch[1]] = parent

            povReduce(ch[2], ch[1])

            elseif #ch == 1 and type(ch[1]) == 'string' then

            ups[ch[1]] = parent

        end
    end
end

-- logic start

-- pov....

local function pov_from(target)
    return {
        of = function(tree)
            povReduce(tree, nil, target)

            for n, v in pairs(ups) do
                print(n, v)
            end

            return {}
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
