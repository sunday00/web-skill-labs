local mapKey = require("utils/keymapper").mapKey

-- Neotree toggle
--mapKey('<leader>e', ':Neotree toggle<cr>')
mapKey("<leader>e", ":NERDTreeToggle<CR>")

-- Pane navigation
mapKey("<C-h>", "<C-w>h") -- Left
mapKey("<C-j>", "<C-w>j") -- Down
mapKey("<C-k>", "<C-w>k") -- Up
mapKey("<C-l>", "<C-w>l") -- Right

--clear search hl
mapKey("<leader>h", ":nohlsearch<CR>")

-- indent
mapKey("<", "<gv", "v")
mapKey(">", ">gv", "v")

mapKey("<C-t>", ":ToggleTerm<CR>")

mapKey("<C-i>", ":CocCommand tsserver.executeAutofix<CR>")
