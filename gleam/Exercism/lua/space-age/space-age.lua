local SpaceAge = {}

function SpaceAge:new (n)
    local age = {}

    age.seconds = n

    local function mutator (ratio)
        local x = age.seconds / 60 / 60 / 24 / 365.25 / ratio

        return tonumber(string.format("%.2f", x))
    end

    age.on_earth = function()
        return mutator(1)
    end

    age.on_mercury = function()
        return mutator(0.2408467)
    end

    age.on_venus = function()
        return mutator(0.61519726)
    end

    age.on_mars = function()
        return mutator(1.8808158)
    end

    age.on_jupiter = function()
        return mutator(11.862615)
    end

    age.on_saturn = function()
        return mutator(29.447498)
    end

    age.on_uranus = function()
        return mutator(84.016846)
    end

    age.on_neptune = function()
        return mutator(164.79132)
    end

    return age
end

--local age = SpaceAge:new(2134835688)
--print(age.on_earth())
--print(age.on_mercury())

return SpaceAge
