return {
  'nvim-mini/mini.nvim',
  version = '*',
  config = function()
    local mini_ai = require 'mini.ai'
    local mini_surround = require 'mini.surround'
    local statusline = require 'mini.statusline'
    local which_key = require 'which-key'

    mini_ai.setup { n_lines = 500 }
    mini_surround.setup()
    statusline.setup { use_icons = flase }

    ---@diagnostic disable-next-line: duplicate-set-field
    statusline.section_location = function()
      return '%2l:%-2v'
    end
  end,
}
