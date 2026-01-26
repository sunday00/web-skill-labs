function string.split (inputstr, sep)
    if sep == nil then
        sep = '.'
    else
        sep = '([^' .. sep .. ']+)'
    end

    local t = {}
    for str in string.gmatch(inputstr, sep)
    do
        table.insert(t, str)
    end

    return t
end

local mb = {
    valid = function(s)
        local stack = {}
        local ss = string.split(s)

        for _, v in ipairs(ss) do
            if v == '(' or v == '{' or v == '[' then
                table.insert(stack, v)
            end

            if v == ')' then
                if table.remove(stack, #stack) ~= '(' then
                    return false
                end


            end

            if v == '}' then
                if table.remove(stack, #stack) ~= '{' then
                    return false
                end
            end

            if v == ']' then
                if table.remove(stack, #stack) ~= '[' then
                    return false
                end
            end
        end

        if #stack > 0 then
            return false
        end

        return true
    end
}

--local r1 = mb.valid('([{}({}[])])')
--local r2 = mb.valid('{}[]')
--local r3 = mb.valid('[({]})')
--local r4 = mb.valid('{)()')
--local r5 = mb.valid([[\left(\begin{array}{cc} \frac{1}{3} & x\\ \mathrm{e}^{x} &... x^2 \end{array}\right)]])
--
--print(r1, r2, r3, r4, r5)

return mb