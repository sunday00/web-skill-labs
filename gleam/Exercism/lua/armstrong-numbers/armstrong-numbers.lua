local ArmstrongNumbers = {}

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

function ArmstrongNumbers.is_armstrong_number(number)
    local s = tostring(number)
    local ss = string.split(s)

    local sum = 0
    for i, v in ipairs(ss) do
        sum = sum + (v ^ #ss)
    end

    return sum == number
end

--for x in string.gmatch("hello every one", "%S+") do
--    print(x)
--end
--
--for x in string.gmatch("hello,every,one", '([^' .. ',' .. ']+)') do
--    print(x)
--end

--for _, x in ipairs(string.split('hello,my,friend,we meet again', ',')) do
--    print(x)
--end
--
--for _, x in ipairs(string.split('hello,my,friend,we meet again', ' ')) do
--    print(x)
--end

--print(ArmstrongNumbers.is_armstrong_number(153))

return ArmstrongNumbers
