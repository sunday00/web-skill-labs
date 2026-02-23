local DNA = {
    new = function(_parent, s)
        local raw = {
            nucleotideCounts = { A = 0, T = 0, C = 0, G = 0 },
            count = function(self, ch)
                if self.nucleotideCounts[ch] == nil then
                    error('Invalid Nucleotide')
                end

                return self.nucleotideCounts[ch]
            end
        }

        for i = 1, #s do
            local id = string.sub(s, i, i)

            if raw.nucleotideCounts[id] == nil then
                error('Invalid Sequence')
            end

            raw.nucleotideCounts[id] = raw.nucleotideCounts[id] + 1
        end

        return raw
    end
}

return DNA
