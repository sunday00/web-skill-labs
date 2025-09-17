print("")
io.write('input any number: ')
local n = tonumber(io.read())

if n%2 == 0 then
    print("EVEN")
elseif n > 100 then
    print("BIGG")
else
    print("ODD")
end
