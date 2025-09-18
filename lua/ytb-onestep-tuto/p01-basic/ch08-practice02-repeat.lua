local n

repeat
    io.write("input any number: ")
    n = tonumber( io.read() ) -- to number from none numeric be nil falsy.
until n

print(n)
