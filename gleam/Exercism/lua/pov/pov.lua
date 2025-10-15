-- utils

local function clone(t)
    local c = {}

    for _, v in ipairs(t) do
        table.insert(c, v)
    end

    return c
end

local function toString(t)
    local l = ''

    for _, v in ipairs(t) do
        l = l .. v .. ','
    end

    return l
end

-- logic start

-- pov....

local function pov_from(target)
    local r = {}

    function r.of(struct)
        if #struct == 1 then
            return struct
        end

        local pf = path_from(target).of()
    end

    return r
end

-- path....

local function pathReducer (acc, cur, target)
    for _, v in ipairs(cur) do
        if type(v[1]) == 'string' and v[2] and type(v[2]) == 'table' then
            local c = clone(acc)
            table.insert(c, v[1])
            local midAcc, done = pathReducer(c, v[2], target)

            if done then
                return midAcc
            end
        end

        for _, e in ipairs(v) do
            if e == target then
                local c = clone(acc)
                table.insert(c, e)
                return c, true
            end
        end
    end
end

local function path_from(target)
    local r = {}

    function r.to ()
        
    end

    function r.of(struct)
        if #struct == 1 then
            return struct
        end

        local res = pathReducer({ struct[1] }, struct[2], target)

        return res
    end

    return r
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
