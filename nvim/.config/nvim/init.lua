require("config.lazy")

vim.opt.clipboard = { "unnamed", "unnamedplus" }

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.number = true
vim.opt.relativenumber = true

-- Navigate splits
vim.keymap.set("n", "<leader>h", "<C-w>h", { desc = "Move to left split" })
vim.keymap.set("n", "<leader>j", "<C-w>j", { desc = "Move to bottom split" })
vim.keymap.set("n", "<leader>k", "<C-w>k", { desc = "Move to top split" })
vim.keymap.set("n", "<leader>l", "<C-w>l", { desc = "Move to right split" })

-- Create/manage splits
vim.keymap.set("n", "<leader>sv", ":vsplit<CR>", { desc = "Split vertically" })
vim.keymap.set("n", "<leader>sh", ":split<CR>", { desc = "Split horizontally" })
vim.keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make splits equal size" })
vim.keymap.set("n", "<leader>sx", ":close<CR>", { desc = "Close current split" })

-- Create separate highlight groups
vim.opt.colorcolumn = ""
vim.api.nvim_set_hl(0, "ColorColumn80", { fg = "#ffaa00" })
vim.api.nvim_set_hl(0, "ColorColumn110", { fg = "#ffaa00" })

-- Create thin vertical lines at 80 and 110 with different colors
vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
	callback = function()
		vim.fn.matchadd("ColorColumn80", "\\%81v", 100)
		vim.fn.matchadd("ColorColumn110", "\\%111v", 100)
	end,
})
