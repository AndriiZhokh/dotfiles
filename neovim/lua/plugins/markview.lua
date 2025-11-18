return {
  'OXY2DEV/markview.nvim',
  config = function()
    local markview = require 'markview'
    local which_key = require 'which-key'

    markview.setup {
      markdown = {
        list_items = { enable = false },
      },
    }

    which_key.add {
      { '<leader>nm', '<cmd>Markview<CR>', desc = '[N]otes Toggle markdown preview' },
    }
  end,
}
