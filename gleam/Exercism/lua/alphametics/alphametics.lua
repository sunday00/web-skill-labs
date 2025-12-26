--function string.split (inputstr, sep)
--    if sep == nil then
--        sep = '.'
--    else
--        sep = '([^' .. sep .. ']+)'
--    end
--
--    local t = {}
--    for str in string.gmatch(string.replace(inputstr, ' ', '_'), sep) do
--        local ss = string.replace(str, '_', ' ')
--        table.insert(t, ss)
--    end
--
--    return t
--end
--
--function string.replace(s, prev, after)
--    return string.gsub(s, prev, after)
--end
--
--function table.uniqueId(t)
--    local tt = {}
--    local size = 0
--
--    for _, el in ipairs(t) do
--        if tt[el] == nil then
--            tt[el] = size + 1
--            size = size + 1
--        end
--    end
--
--    --local ttt = {}
--    --for k, _ in pairs(tt) do
--    --    table.insert(ttt, k)
--    --end
--
--    --return ttt
--
--    return tt
--end
--
function table.debug(t)
    for i, el in pairs(t) do
        print(i, el)
    end
end
--
--local function solve(puzzle)
--    local leftRight = string.split(puzzle, ' == ')
--    local lefts = string.split(leftRight[1], ' %W ')
--
--    local ABC = string.split(string.replace(puzzle, '%W', ''), nil)
--
--    ABC = table.uniqueId(ABC)
--
--    table.debug(ABC)
--end
--
--local r = solve('SEND + MORE == MONEY')
----print(r)
--
--return { solve = solve }

local function get_first_letters(formula)
    local first_letters = {}
    -- 단어(%a+)를 하나씩 찾아서 첫 글자([%a])를 캡처
    for word in formula:gmatch("%a+") do
        local first = word:sub(1, 1)
        first_letters[first] = true
    end
    return first_letters
end

local function solve_formula(formula, mapping)
    local actual_formula = formula:gsub("%a", mapping)
    -- "="을 "=="로 변경하여 Lua 코드로 실행 가능하게 함
    --actual_formula = actual_formula:gsub("=", "==")

    -- load를 사용하여 문자열 수식 실행 (안전한 환경 권장)
    local f = load("return " .. actual_formula)
    return f and f()
end

-- 1. 고유 알파벳 추출 함수
local function solve_cryptarithmetic(formula)
    local chars = {}
    local char_list = {}
    local first_letters = get_first_letters(formula) -- 첫 글자 집합 생성

    -- 고유 알파벳 추출
    local seen = {}
    for c in formula:gmatch("%a") do
        if not seen[c] then
            seen[c] = true
            table.insert(char_list, c)
        end
    end

    local mapping = {}
    local used_nums = {}

    local function backtrack(index)
        -- 모든 알파벳에 숫자를 할당한 경우
        if index > #char_list then
            return solve_formula(formula, mapping) -- 수식 검증
        end

        local current_char = char_list[index]

        for num = 0, 9 do
            -- 1. 중복 숫자 체크
            -- 2. 첫 글자인데 0을 할당하려는지 체크
            if not used_nums[num] then
                if not (num == 0 and first_letters[current_char]) then

                    mapping[current_char] = num
                    used_nums[num] = true

                    if backtrack(index + 1) then
                        return true
                    end

                    -- 백트래킹 (원상 복구)
                    used_nums[num] = false
                    mapping[current_char] = nil
                end
            end
        end
        return false
    end

    if backtrack(1) then
        return mapping
    else
        return nil
    end
end

--local res = solve_cryptarithmetic("SEND + MORE == MONEY")
--
--table.debug(res)

return { solve = solve_cryptarithmetic }
