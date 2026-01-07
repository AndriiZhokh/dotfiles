return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {},
  config = function()
    local fzf_lua = require 'fzf-lua'
    local which_key = require 'which-key'

    which_key.add {
      { '<leader>sf', fzf_lua.files, desc = '[S]earch [F]iles' },
      { '<leader>sg', fzf_lua.grep, desc = '[S]earch with [G]rep'},
      { '<leader><leader>', fzf_lua.buffers, desc = '[ ] Find existing buffers' },
    }
  end,
}
