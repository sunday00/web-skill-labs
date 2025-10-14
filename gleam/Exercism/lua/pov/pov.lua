local function pov_from(node_name)
    local r = {}

    function r:of(struct)
        if #struct == 1 then
            return struct
        end


    end

    return r
end

local function pathReducer (acc, cur, branchPoints)

end

local function path_from(source)
    local r = {}

    function r:of(struct)
        if #struct == 1 then
            return struct
        end


    end

    return r
end

local res = pov_from('leaf').of({ 'grand_parent', {
    { 'parent', {
        { 'sibling_1' }, { 'leaf' }, { 'sibling_2' } } },
    { 'uncle', {
        { 'cousin_1' }, { 'cousin_2' } } } } })

for _, r in ipairs(res) do
    print(r)
end

return { pov_from = pov_from, path_from = path_from }
