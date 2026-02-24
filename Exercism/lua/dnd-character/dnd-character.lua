local Character = {

}

local function ability()
    return math.random(1, 6) + math.random(1, 6) + math.random(1, 6)
end

local function modifier(input)
    return math.floor((input - 10) / 2)
end

function Character:new (name)
    local ab = {
        name = name,
        strength = ability(),
        dexterity = ability(),
        constitution = ability(),
        intelligence = ability(),
        wisdom = ability(),
        charisma = ability(),
        hitpoints = 0
    }

    ab.hitpoints = 10 + modifier(ab.constitution)

    return ab
end

return { Character = Character, ability = ability, modifier = modifier }
