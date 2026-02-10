local HighScores = {}

local gScores = {}

function HighScores:scores()
    return gScores
end

function HighScores:latest()
    return gScores[#gScores]
end

function HighScores:personal_best()
    local max = 0

    for _, v in ipairs(gScores) do
        max = math.max(max, v)
    end

    return max
end

function HighScores:personal_top_three()
    local inner = {}

    for _, v in ipairs(gScores) do
        table.insert(inner, v)
    end

    table.sort(inner)

    local innnner = {}
    for i = 1, math.min(#inner, 3) do
        table.insert(innnner, inner[#inner - (i - 1)])
    end

    return innnner
end

return function(scores)
    local high_scores = {}

    gScores = scores

    setmetatable(high_scores, { __index = HighScores })

    return high_scores
end
