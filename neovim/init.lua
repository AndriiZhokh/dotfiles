require 'keymaps'
require 'plugins'
require 'colorscheme'

local os_info = os.getenv('HOME')
print(os_info)

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Displays which-key popup sooner
vim.opt.timeoutlen = 300

local FoldMethods = {
  indent = 'indent',
  syntax = 'syntax',
  marker = 'marker',
  expr = 'expr',
  manual = 'manual',
}

local option = vim.opt

-- :help options
local options = {
  backup = false,                          -- creates a backup file
  clipboard = "unnamedplus",               -- allows neovim to access the system clipboard
  cmdheight = 1,                           -- more space in the neovim command line for displaying messages
  completeopt = { "menuone", "noselect" }, -- mostly just for cmp
  conceallevel = 0,                        -- so that `` is visible in markdown files
  colorcolumn = "140",
  fileencoding = "utf-8",                  -- the encoding written to a file
  hlsearch = true,                         -- highlight all matches on previous search pattern
  ignorecase = true,                       -- ignore case in search patterns
  mouse = "a",                             -- allow the mouse to be used in neovim
  pumheight = 10,                          -- pop up menu height
  showmatch = true,
  showmode = false,                        -- we don't need to see things like -- INSERT -- anymore
  showtabline = 2,                         -- always show tabs
  smartcase = true,                        -- smart case
  smartindent = true,                      -- make indenting smarter again
  splitbelow = true,                       -- force all horizontal splits to go below current window
  splitright = true,                       -- force all vertical splits to go to the right of current window
  swapfile = false,                        -- creates a swapfile
  termguicolors = true,                    -- set term gui colors (most terminals support this)
  timeoutlen = 100,                        -- time to wait for a mapped sequence to complete (in milliseconds)
  undofile = true,                         -- enable persistent undo
  updatetime = 300,                        -- faster completion (4000ms default)
  writebackup = false,                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  expandtab = true,                        -- convert tabs to spaces
  shiftwidth = 2,                          -- the number of spaces inserted for each indentation
  tabstop = 2,                             -- insert 2 spaces for a tab
  cursorline = true,                       -- highlight the current line
  number = true,                           -- set numbered lines
  relativenumber = true,                   -- set relative numbered lines
  numberwidth = 2,                         -- set number column width to 2 {default 4}
  signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                            -- display lines as one long line
  scrolloff = 8,                           -- is one of my fav
  sidescrolloff = 8,
  guifont = "monospace:h17",               -- the font used in graphical neovim applications
  foldenable = true,                       -- enables folding
  foldmethod = FoldMethods.manual,         -- set default folding method
  -- nofoldenable = false                  -- disable folding
  winborder = 'rounded'
}

option.shortmess:append "c"

for k, v in pairs(options) do
  option[k] = v
end

vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldlevel = 99 -- Start with all folds open

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]
vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Custom Markdown folding
-- _G.ManualMarkdownFold = function()
--   local line = vim.api.nvim_buf_get_lines(0, vim.v.lnum - 1, vim.v.lnum, false)[1]
--
--   local match = string.match(line, '^(#+) %S')
--
--   if match then
--     return '>' .. #match
--   else
--     return '='
--   end
-- end

-- _G.PrettyMarkdownFoldText = function()
--   local line = vim.api.nvim_buf_get_lines(0, vim.v.foldstart - 1, vim.v.foldstart, false)[1]
--
--   local clean_line = string.gsub(line, '^#+ ', '')
--
--   local line_count = vim.v.foldend - vim.v.foldstart + 1
--
--   local display_text = string.format('%s ... [%d lines]', line, line_count)
--
--   return display_text
-- end

-- Autocommand to apply our custom folding script ONLY to Markdown files.
-- vim.api.nvim_create_autocmd('FileType', {
--   pattern = 'markdown',
--   desc = 'Use manual regex-based folding for Markdown.',
--   callback = function()
--     vim.opt_local.foldexpr = 'v:lua._G.ManualMarkdownFold()'
--     vim.opt_local.foldtext = 'v:lua._G.PrettyMarkdownFoldText()'
--
--     vim.keymap.set('n', '<Tab>', 'za', { buffer = true, desc = 'Toggle fold' })
--   end,
-- })
