return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {},
  config = function()
    local fzf_lua = require 'fzf-lua'
    local which_key = require 'which-key'

    which_key.add {
      { '<leader>ff', fzf_lua.files, desc = '[F]ind [F]iles' },
      { '<leader>fg', fzf_lua.live_grep, desc = '[F]ind with Live [G]rep'},
      { '<leader><leader>', fzf_lua.buffers, desc = '[ ] Find existing buffers' },
    }
  end,
}
