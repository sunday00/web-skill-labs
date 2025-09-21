local t = {11, 22, 100, -3, 56, 0, 121, 300}

for _, e in ipairs(t) do
    print(e)

    if e == 0 then
        break
    end
end

local n = 101
local isPrime = true
for k=n-1,2,-1 do
    if n % k == 0 then
        isPrime = false
        break
    end
end

print(isPrime)
