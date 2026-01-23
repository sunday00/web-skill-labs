local function triangle(n)
    local r = { { 1 }, { 1, 1 } }

    if n == 1 then
        r = { { 1 } }
    elseif n == 2 then
        r = { { 1 }, { 1, 1 } }
    else
        for i = 3, n do
            local sm = { 1 }

            for ii = 2, #r[i - 1] do
                table.insert(sm, r[i - 1][ii - 1] + r[i - 1][ii])
            end

            table.insert(sm, 1)
            table.insert(r, sm)
        end
    end

    return {
        rows = r,
        last_row = r[#r]
    }
end

return triangle
