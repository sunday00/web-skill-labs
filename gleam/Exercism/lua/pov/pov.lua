-- utils

local _tree
local nameDic = {}
local ups = {}

local function debugDescribe(res, label)
    for _,v in pairs(res) do
        if type(v) == 'string' then
            print(label, v)

        else
            debugDescribe(v, label .. '-ch')
        end
    end
end

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

local function remove(tree, target)
    local res = {}

    for _, v in ipairs(tree) do
        if #v == 2 and type(v[1]) == 'string' and type(v[2]) == 'table' and v[1] ~= target then
            table.insert(res, v)
        elseif type(v) == 'table' and type(v[2]) ~= 'table' then
            for _, e in ipairs(v) do
                if e ~= target then
                    table.insert(res, { e })
                end
            end
        end
    end

    return res
end

local function povMakePrev(tree, parent)
    if #tree == 2 and type(tree[1]) == 'string' and type(tree[2]) == 'table' then
        nameDic[tree[1]] = tree[2]

        povMakePrev(tree[2], tree[1])
    end

    for _, ch in ipairs(tree) do
        if #ch == 2 and type(ch[1]) == 'string' and type(ch[2]) == 'table' then
            nameDic[ch[1]] = ch[2]
            ups[ch[1]] = parent

            povMakePrev(ch[2], ch[1])

            elseif #ch == 1 and type(ch[1]) == 'string' then

            ups[ch[1]] = parent
        end
    end
end

local function povReduce(target)
    local me = nameDic[target]

    local parentN = ups[target]
    local parent = nameDic[parentN]

    parent = remove(parent, target)

    nameDic[parentN] = parent

    if ups[parentN] then
        povReduce(parentN)
    end

    if me then
        if(#parent > 0) then
            table.insert(me, { parentN, parent })
        else
            table.insert(me, { parentN })
        end

        local res =  { target, me }

        return res
    else
        local res =  { target, { { parentN, parent } }}

        return res
    end
end

-- logic start

-- pov....

local function pov_from(target)
    _tree = nil
    nameDic = {}
    ups = {}

    return {
        of = function(tree)
            _tree = cloneTree(tree)
            povMakePrev(tree, nil)

            local res = povReduce(target)

            --print("================")

            return res
        end
    }
end

-- path....



local function path_from(target)

end

-- test

local res = pov_from('leaf').of({ 'level1', {
    { 'level2', {
        { 'level3', {
            { 'level4', {
                { 'leaf' }}}}}}}}})

return { pov_from = pov_from, path_from = path_from }
