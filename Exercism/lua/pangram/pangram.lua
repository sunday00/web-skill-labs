local abc

return function(s)
    local initMap = function()
        abc = 'abcdefghijklmnopqrstuvwxyz'
    end

    initMap()

    for i = 1, #s do
        abc = string.gsub(abc, string.lower(string.sub(s, i, i)), '')
    end

    return #abc == 0
end
