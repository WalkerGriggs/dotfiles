return {
	{
		"slugbyte/lackluster.nvim",
		lazy = false,
		priority = 1000,
		init = function()
			vim.cmd.colorscheme("lackluster-hack")
		end,
	},
	{
		"dstein64/vim-startuptime",
		cmd = "StartupTime",
		init = function()
			vim.g.startuptime_tries = 10
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		event = { "BufReadPre", "BufNewFile" },
		build = ":TSUpdate",
		dependencies = { "windwp/nvim-ts-autotag" },
		config = function()
			require("nvim-treesitter.configs").setup({
				highlight = { enable = true },
				indent = { enable = true },
				autotag = { enable = true },
				ensure_installed = {
					"lua",
					"bash",
					"json",
					"yaml",
					"html",
					"css",
					"typescript",
					"tsx",
					"markdown",
				},
			})
		end,
	},
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		opts = {
			formatters_by_ft = {
				go = { "gofmt", "goimports" },
				lua = { "stylua" },
				typescript = { "prettier" },
				javascript = { "prettier" },
				json = { "prettier" },
				yaml = { "prettier" },
				markdown = { "prettier" },
			},
			format_on_save = {
				timeout_ms = 500,
				lsp_fallback = true,
			},
		},
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.5",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		config = function()
			local telescope = require("telescope")
			local actions = require("telescope.actions")

			telescope.setup({
				defaults = {
					-- Better UI
					selection_caret = "➜ ",
					path_display = { "truncate" },

					-- Better sorting
					file_ignore_patterns = {
						"node_modules",
						".git/",
						"dist/",
						"build/",
						"%.lock",
					},

					-- Keybindings inside telescope
					mappings = {
						i = {
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,
							["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
							["<Esc>"] = actions.close,
						},
						n = {
							["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
							["q"] = actions.close,
						},
					},
				},
				pickers = {
					find_files = {
						hidden = true, -- Show hidden files
					},
				},
			})

			-- Load fzf extension for better performance
			pcall(telescope.load_extension, "fzf")

			-- Keybindings
			local builtin = require("telescope.builtin")

			-- File finding
			vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
			vim.keymap.set("n", "<leader>fg", builtin.git_files, { desc = "Find git files" })
			vim.keymap.set("n", "<C-p>", builtin.find_files, { desc = "Find files" })

			-- Text searching (like fzf + ripgrep)
			vim.keymap.set("n", "<leader>fs", builtin.live_grep, { desc = "Search text in project" })
			vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "Search word under cursor" })

			-- Buffer/recent files
			vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Find buffers" })
			vim.keymap.set("n", "<leader>fo", builtin.oldfiles, { desc = "Find recent files" })

			-- Help and commands
			vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find help" })
			vim.keymap.set("n", "<leader>fc", builtin.commands, { desc = "Find commands" })
			vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "Find keymaps" })

			-- Git integration
			vim.keymap.set("n", "<leader>gc", builtin.git_commits, { desc = "Git commits" })
			vim.keymap.set("n", "<leader>gs", builtin.git_status, { desc = "Git status" })

			-- LSP integration
			vim.keymap.set("n", "<leader>fr", builtin.lsp_references, { desc = "Find references" })
			vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "Find diagnostics" })
		end,
	},
	{
		"neovim/nvim-lspconfig",
		config = function()
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local opts = { buffer = args.buf }
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
					vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
					vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
					vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
				end,
			})

			-- Go
			vim.lsp.config.gopls = {
				cmd = { "gopls" },
				filetypes = { "go", "gomod" },
				root_markers = { "go.mod", ".git" },
			}
			vim.lsp.enable("gopls")

			-- Lua
			vim.lsp.config.lua_ls = {
				cmd = { "lua-language-server" },
				filetypes = { "lua" },
				root_markers = { ".git" },
				settings = {
					Lua = {
						diagnostics = { globals = { "vim" } },
						workspace = { library = vim.api.nvim_get_runtime_file("", true) },
						telemetry = { enable = false },
					},
				},
			}
			vim.lsp.enable("lua_ls")

			-- TypeScript
			vim.lsp.config.tsserver = {
				cmd = { "typescript-language-server", "--stdio" },
				filetypes = { "javascript", "typescript", "javascriptreact", "typescriptreact" },
				root_markers = { "package.json", "tsconfig.json", ".git" },
			}
			vim.lsp.enable("tsserver")
		end,
	},
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				signs = {
					add = { text = "│" },
					change = { text = "│" },
					delete = { text = "_" },
					topdelete = { text = "‾" },
					changedelete = { text = "~" },
					untracked = { text = "┆" },
				},
				on_attach = function(bufnr)
					local gs = package.loaded.gitsigns
					local opts = { buffer = bufnr }

					-- Navigation between hunks
					vim.keymap.set("n", "]c", function()
						if vim.wo.diff then
							return "]c"
						end
						vim.schedule(function()
							gs.next_hunk()
						end)
						return "<Ignore>"
					end, vim.tbl_extend("force", opts, { expr = true }))

					vim.keymap.set("n", "[c", function()
						if vim.wo.diff then
							return "[c"
						end
						vim.schedule(function()
							gs.prev_hunk()
						end)
						return "<Ignore>"
					end, vim.tbl_extend("force", opts, { expr = true }))

					-- Actions
					vim.keymap.set("n", "<leader>hs", gs.stage_hunk, opts)
					vim.keymap.set("n", "<leader>hr", gs.reset_hunk, opts)
					vim.keymap.set("n", "<leader>hp", gs.preview_hunk, opts)
					vim.keymap.set("n", "<leader>hb", function()
						gs.blame_line({ full = true })
					end, opts)
					vim.keymap.set("n", "<leader>hd", gs.diffthis, opts)
				end,
			})
		end,
	},
	{
		"greggh/claude-code.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim", -- Required for git operations
		},
		config = function()
			require("claude-code").setup()
		end,
	},
	{
		"github/copilot.vim",
		config = function()
			-- Accept suggestion with Tab
			vim.g.copilot_no_tab_map = true
			vim.keymap.set("i", "<Tab>", 'copilot#Accept("\\<CR>")', {
				expr = true,
				replace_keycodes = false,
			})

			-- Navigate suggestions
			vim.keymap.set("i", "<M-]>", "<Plug>(copilot-next)")
			vim.keymap.set("i", "<M-[>", "<Plug>(copilot-previous)")

			-- Dismiss suggestion
			vim.keymap.set("i", "<C-e>", "<Plug>(copilot-dismiss)")

			-- Manually trigger Copilot
			vim.keymap.set("i", "<M-\\>", "<Plug>(copilot-suggest)")
		end,
	},
}
