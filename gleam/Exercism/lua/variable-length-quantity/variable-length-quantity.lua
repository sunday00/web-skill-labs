local function decode(bytes)
    local result = {}
    local current_num = 0

    for _, b in ipairs(bytes) do
        -- MSB(128)가 있는지 확인
        if b >= 128 then
            -- MSB가 1이면: 128을 빼고(7비트 추출), 기존 값에 128을 곱한 뒤 더함
            current_num = (current_num * 128) + (b - 128)
        else
            -- MSB가 0이면: 숫자의 마지막 바이트이므로 결과에 추가
            current_num = (current_num * 128) + b

            table.insert(result, current_num)
            current_num = 0
        end
    end

    if #result == 0 then
        error()
    end

    return result
end

local function encode(values)
    local result = {}
    for _, n in ipairs(values) do
        -- 0 처리 (VQL에서 0은 단일 바이트 0x00)

        if n == 0 then
            table.insert(result, 0)
        else
            local bytes = {}
            -- 1. 가장 오른쪽 7비트 추출 (끝을 의미하므로 MSB는 0)
            table.insert(bytes, 1, n % 128)
            n = math.floor(n / 128)

            -- 2. 남은 숫자가 있으면 7비트씩 쪼개고 MSB를 1(128 더하기)로 설정
            while n > 0 do
                local b = (n % 128) + 128
                table.insert(bytes, 1, b)
                n = math.floor(n / 128)
            end

            -- 결과 테이블에 병합
            for i = 1, #bytes do
                table.insert(result, bytes[i])
            end
        end
    end
    return result
end

--local r = encode({ 0x0 })
--for i, v in ipairs(r) do
--    print(v)
--end

return { decode = decode, encode = encode }
