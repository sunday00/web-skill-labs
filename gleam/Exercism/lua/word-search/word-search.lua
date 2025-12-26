function table.debug(t)
    for i, el in pairs(t) do
        if type(el) == 'table' then
            for ii, eel in pairs(el) do
                if type(eel) == 'table' then
                    print(i, ii, eel[1], eel[2])
                else
                    print(i, ii, eel)
                end
            end
        else
            print(i, el)
        end
    end
end

function searchHorizon (grid, word)
    for i, v in ipairs(grid) do
        local tmp = string.find(v, word)

        if tmp ~= nil then
            return { start = { tmp, i }, ['end'] = { tmp + #word - 1, i } }
        end

        tmp = string.find(string.reverse(v), word)

        if tmp ~= nil then
            return { start = { #v - tmp + 1, i }, ['end'] = { #v - tmp - #word + 1 + 1, i } }
        end
    end

    return nil
end

function searchVerticel(vGrid, word)
    for i, v in ipairs(vGrid) do
        local tmp = string.find(v, word)

        if tmp ~= nil then
            return { start = { i, tmp }, ['end'] = { i, tmp + #word - 1 } }
        end

        tmp = string.find(string.reverse(v), word)

        if tmp ~= nil then
            return { start = { i, #v - tmp + 1 }, ['end'] = { i, #v - tmp + 1 - #word + 1 } }
        end
    end

    return nil
end

function searchDia(grid, word)
    for i, row in ipairs(grid) do
        local char = word:sub(1, 1)
        local loc = string.find(row, char)
        if loc == nil then
            goto continue
        end

        :: redo ::

        local tdlr = char
        local tdrl = char
        local dtrl = char
        local dtlr = char
        for ii = 1, #word - 1 do
            if grid[i + ii] ~= nil then
                tdlr = tdlr .. grid[i + ii]:sub(loc + ii, loc + ii)
            end
            if grid[i + ii] ~= nil then
                tdrl = tdrl .. grid[i + ii]:sub(loc - ii, loc - ii)
            end
            if grid[i - ii] ~= nil then
                dtrl = dtrl .. grid[i - ii]:sub(loc - ii, loc - ii)
            end
            if grid[i - ii] ~= nil then
                dtlr = dtlr .. grid[i - ii]:sub(loc + ii, loc + ii)
            end
        end

        if word == tdlr then
            return { start = { loc, i }, ['end'] = { loc + #word - 1, i + #word - 1 } }
        end

        if word == tdrl then
            return { start = { loc, i }, ['end'] = { loc - #word + 1, i + #word - 1 } }
        end

        if word == dtrl then
            return { start = { loc, i }, ['end'] = { loc - #word + 1, i - #word + 1 } }
        end

        if word == dtlr then
            return { start = { loc, i }, ['end'] = { loc + #word - 1, i - #word + 1 } }
        end

        loc = string.find(row, char, loc + 1)

        if loc ~= nil then
            goto redo
        end

        :: continue ::
    end
end

function solve (grid)
    local vGrid

    function vAxis()
        local t = {}

        for i = 1, #grid[1] do
            local row = ''
            for _, v in ipairs(grid) do
                row = row .. v:sub(i, i)
            end

            table.insert(t, row)
        end

        return t
    end

    local ins = {
        search = function(words)
            local res = {}

            for _, w in ipairs(words) do
                local r = searchHorizon(grid, w)

                if r ~= nil then
                    res[w] = r
                    goto continue
                end

                vGrid = vAxis()
                r = searchVerticel(vGrid, w)

                if r ~= nil then
                    res[w] = r
                    goto continue
                end

                r = searchDia(grid, w)
                if r ~= nil then
                    res[w] = r
                    goto continue
                end

                :: continue ::
            end

            return res
        end,
    }

    return ins
end

--local r = solve({ --
--    'jefblpepre', --
--    'camdcimgtc', --
--    'oivokprjsm', --
--    'pbwasqroua', --
--    'rixilelhrs', --
--    'wolcqlirpc', --
--    'screeaumgr', --
--    'alxhpburyi', --
--    'jalaycalmp', --
--    'clojurermt'
--})
--        .search({ --
--    'clojure', --
--    'elixir', --
--    'ecmascript', --
--    'rust', --
--    'java', --
--    'lua'
--})
--.vAxis()

--table.debug(r)

return solve
