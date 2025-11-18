-- Neovim Color Scheme
-- Save this file as: ~/.config/nvim/colors/compline.lua
-- Then activate with: :colorscheme compline

vim.cmd('highlight clear')
if vim.fn.exists('syntax_on') then
  vim.cmd('syntax reset')
end

vim.g.colors_name = 'compline'
vim.o.termguicolors = true

-- Palette
local colors = {
  -- Background layers
  bg = '#1a1d21',
  bg_alt = '#22262b',
  base0 = '#0f1114',
  base1 = '#171a1e',
  base2 = '#1f2228',
  base3 = '#282c34',
  base4 = '#3d424a',
  base5 = '#515761',
  base6 = '#676d77',
  base7 = '#8b919a',
  base8 = '#e0dcd4',
  
  -- Foreground
  fg = '#f0efeb',
  fg_alt = '#ccc4b4',
  
  -- Semantic colors
  red = '#cdacac',
  orange = '#ccc4b4',
  green = '#b8c4b8',
  yellow = '#d4ccb4',
  blue = '#b4bcc4',
  cyan = '#b4c0c8',
  teal = '#b4c4bc',
  dark_cyan = '#98a4ac',
  
  none = 'NONE'
}

-- Helper function to set highlights
local function hl(group, opts)
  local cmd = 'highlight ' .. group
  if opts.fg then cmd = cmd .. ' guifg=' .. opts.fg end
  if opts.bg then cmd = cmd .. ' guibg=' .. opts.bg end
  if opts.sp then cmd = cmd .. ' guisp=' .. opts.sp end
  if opts.gui then cmd = cmd .. ' gui=' .. opts.gui end
  if opts.link then cmd = 'highlight! link ' .. group .. ' ' .. opts.link end
  vim.cmd(cmd)
end

-- Editor UI
hl('Normal', { fg = colors.fg, bg = colors.bg })
hl('NormalFloat', { fg = colors.fg, bg = colors.bg_alt })
hl('NormalNC', { fg = colors.fg, bg = colors.bg })
hl('SignColumn', { fg = colors.base5, bg = colors.bg })
hl('EndOfBuffer', { fg = colors.base3 })
hl('Cursor', { fg = colors.bg, bg = colors.fg })
hl('CursorLine', { bg = colors.base1 })
hl('CursorColumn', { bg = colors.base1 })
hl('ColorColumn', { bg = colors.base1 })
hl('CursorLineNr', { fg = colors.base8, bg = colors.base1 })
hl('LineNr', { fg = colors.base5 })
hl('Folded', { fg = colors.base6, bg = colors.base2 })
hl('FoldColumn', { fg = colors.base5, bg = colors.bg })
hl('VertSplit', { fg = colors.base4 })
hl('WinSeparator', { fg = colors.base4 })

-- Visual selection
hl('Visual', { bg = colors.base3 })
hl('VisualNOS', { bg = colors.base3 })

-- Search
hl('Search', { fg = colors.bg, bg = colors.yellow })
hl('IncSearch', { fg = colors.bg, bg = colors.orange })
hl('CurSearch', { fg = colors.bg, bg = colors.orange })
hl('Substitute', { fg = colors.bg, bg = colors.green })

-- Messages
hl('ErrorMsg', { fg = colors.red })
hl('WarningMsg', { fg = colors.yellow })
hl('ModeMsg', { fg = colors.fg_alt })
hl('MoreMsg', { fg = colors.green })
hl('Question', { fg = colors.blue })

-- UI elements
hl('Pmenu', { fg = colors.fg, bg = colors.base2 })
hl('PmenuSel', { fg = colors.bg, bg = colors.blue })
hl('PmenuSbar', { bg = colors.base3 })
hl('PmenuThumb', { bg = colors.base5 })
hl('StatusLine', { fg = colors.fg, bg = colors.base2 })
hl('StatusLineNC', { fg = colors.base6, bg = colors.base1 })
hl('TabLine', { fg = colors.base6, bg = colors.base2 })
hl('TabLineFill', { bg = colors.base1 })
hl('TabLineSel', { fg = colors.fg, bg = colors.base3 })
hl('WildMenu', { fg = colors.bg, bg = colors.blue })

-- Diff
hl('DiffAdd', { fg = colors.green, bg = colors.base2 })
hl('DiffChange', { fg = colors.yellow, bg = colors.base2 })
hl('DiffDelete', { fg = colors.red, bg = colors.base2 })
hl('DiffText', { fg = colors.blue, bg = colors.base3 })

-- Git signs
hl('GitSignsAdd', { fg = colors.green })
hl('GitSignsChange', { fg = colors.yellow })
hl('GitSignsDelete', { fg = colors.red })

-- Syntax highlighting
hl('Comment', { fg = colors.base6, gui = 'italic' })
hl('Constant', { fg = colors.orange })
hl('String', { fg = colors.green })
hl('Character', { fg = colors.green })
hl('Number', { fg = colors.orange })
hl('Boolean', { fg = colors.orange })
hl('Float', { fg = colors.orange })

hl('Identifier', { fg = colors.fg })
hl('Function', { fg = colors.blue })

hl('Statement', { fg = colors.cyan })
hl('Conditional', { fg = colors.cyan })
hl('Repeat', { fg = colors.cyan })
hl('Label', { fg = colors.cyan })
hl('Operator', { fg = colors.teal })
hl('Keyword', { fg = colors.cyan })
hl('Exception', { fg = colors.red })

hl('PreProc', { fg = colors.yellow })
hl('Include', { fg = colors.cyan })
hl('Define', { fg = colors.cyan })
hl('Macro', { fg = colors.yellow })
hl('PreCondit', { fg = colors.yellow })

hl('Type', { fg = colors.yellow })
hl('StorageClass', { fg = colors.cyan })
hl('Structure', { fg = colors.yellow })
hl('Typedef', { fg = colors.yellow })

hl('Special', { fg = colors.teal })
hl('SpecialChar', { fg = colors.orange })
hl('Tag', { fg = colors.blue })
hl('Delimiter', { fg = colors.base7 })
hl('SpecialComment', { fg = colors.base7 })
hl('Debug', { fg = colors.red })

hl('Underlined', { gui = 'underline' })
hl('Error', { fg = colors.red })
hl('Todo', { fg = colors.yellow, bg = colors.base2, gui = 'bold' })

-- Treesitter
hl('@variable', { fg = colors.fg })
hl('@variable.builtin', { fg = colors.red })
hl('@variable.parameter', { fg = colors.fg_alt })
hl('@variable.member', { fg = colors.teal })

hl('@constant', { fg = colors.orange })
hl('@constant.builtin', { fg = colors.orange })
hl('@constant.macro', { fg = colors.orange })

hl('@string', { fg = colors.green })
hl('@string.escape', { fg = colors.cyan })
hl('@string.special', { fg = colors.teal })
hl('@character', { fg = colors.green })
hl('@number', { fg = colors.orange })
hl('@boolean', { fg = colors.orange })
hl('@float', { fg = colors.orange })

hl('@function', { fg = colors.blue })
hl('@function.builtin', { fg = colors.blue })
hl('@function.macro', { fg = colors.yellow })
hl('@function.method', { fg = colors.blue })

hl('@constructor', { fg = colors.yellow })
hl('@keyword', { fg = colors.cyan })
hl('@keyword.function', { fg = colors.cyan })
hl('@keyword.operator', { fg = colors.cyan })
hl('@keyword.return', { fg = colors.cyan })

hl('@operator', { fg = colors.teal })
hl('@punctuation.delimiter', { fg = colors.base7 })
hl('@punctuation.bracket', { fg = colors.base7 })
hl('@punctuation.special', { fg = colors.teal })

hl('@type', { fg = colors.yellow })
hl('@type.builtin', { fg = colors.yellow })
hl('@attribute', { fg = colors.cyan })
hl('@property', { fg = colors.teal })

hl('@tag', { fg = colors.cyan })
hl('@tag.attribute', { fg = colors.yellow })
hl('@tag.delimiter', { fg = colors.base7 })

hl('@comment', { fg = colors.base6, gui = 'italic' })

-- LSP
hl('DiagnosticError', { fg = colors.red })
hl('DiagnosticWarn', { fg = colors.yellow })
hl('DiagnosticInfo', { fg = colors.blue })
hl('DiagnosticHint', { fg = colors.cyan })

hl('DiagnosticUnderlineError', { sp = colors.red, gui = 'underline' })
hl('DiagnosticUnderlineWarn', { sp = colors.yellow, gui = 'underline' })
hl('DiagnosticUnderlineInfo', { sp = colors.blue, gui = 'underline' })
hl('DiagnosticUnderlineHint', { sp = colors.cyan, gui = 'underline' })

hl('LspReferenceText', { bg = colors.base3 })
hl('LspReferenceRead', { bg = colors.base3 })
hl('LspReferenceWrite', { bg = colors.base3 })

-- Telescope
hl('TelescopeNormal', { fg = colors.fg, bg = colors.bg_alt })
hl('TelescopeBorder', { fg = colors.base4, bg = colors.bg_alt })
hl('TelescopePromptBorder', { fg = colors.base4, bg = colors.base2 })
hl('TelescopePromptNormal', { fg = colors.fg, bg = colors.base2 })
hl('TelescopePromptPrefix', { fg = colors.cyan })
hl('TelescopeSelection', { fg = colors.fg, bg = colors.base3 })
hl('TelescopeSelectionCaret', { fg = colors.cyan })
hl('TelescopeMatching', { fg = colors.yellow, gui = 'bold' })

-- Neo-tree / nvim-tree
hl('NeoTreeNormal', { fg = colors.fg, bg = colors.bg_alt })
hl('NeoTreeNormalNC', { fg = colors.fg, bg = colors.bg_alt })
hl('NeoTreeDirectoryIcon', { fg = colors.blue })
hl('NeoTreeDirectoryName', { fg = colors.blue })
hl('NeoTreeFileName', { fg = colors.fg })
hl('NeoTreeGitModified', { fg = colors.yellow })
hl('NeoTreeGitAdded', { fg = colors.green })
hl('NeoTreeGitDeleted', { fg = colors.red })

hl('NvimTreeNormal', { fg = colors.fg, bg = colors.bg_alt })
hl('NvimTreeFolderIcon', { fg = colors.blue })
hl('NvimTreeFolderName', { fg = colors.blue })
hl('NvimTreeOpenedFolderName', { fg = colors.blue })
hl('NvimTreeRootFolder', { fg = colors.cyan, gui = 'bold' })

-- WhichKey
hl('WhichKey', { fg = colors.cyan })
hl('WhichKeyGroup', { fg = colors.blue })
hl('WhichKeyDesc', { fg = colors.fg })
hl('WhichKeySeparator', { fg = colors.base5 })
hl('WhichKeyFloat', { bg = colors.bg_alt })

-- Cmp (completion)
hl('CmpItemAbbr', { fg = colors.fg })
hl('CmpItemAbbrMatch', { fg = colors.blue, gui = 'bold' })
hl('CmpItemAbbrMatchFuzzy', { fg = colors.blue })
hl('CmpItemKind', { fg = colors.cyan })
hl('CmpItemMenu', { fg = colors.base6 })

-- Indent blankline
hl('IndentBlanklineChar', { fg = colors.base3 })
hl('IndentBlanklineContextChar', { fg = colors.base5 })

-- Markdown
hl('markdownHeadingDelimiter', { fg = colors.cyan })
hl('markdownH1', { fg = colors.red, gui = 'bold' })
hl('markdownH2', { fg = colors.orange, gui = 'bold' })
hl('markdownH3', { fg = colors.yellow, gui = 'bold' })
hl('markdownH4', { fg = colors.green, gui = 'bold' })
hl('markdownH5', { fg = colors.blue, gui = 'bold' })
hl('markdownH6', { fg = colors.cyan, gui = 'bold' })
hl('markdownCode', { fg = colors.green })
hl('markdownCodeBlock', { fg = colors.green })
hl('markdownCodeDelimiter', { fg = colors.teal })
hl('markdownUrl', { fg = colors.dark_cyan, gui = 'underline' })
hl('markdownLinkText', { fg = colors.blue })
