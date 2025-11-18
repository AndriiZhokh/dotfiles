return {
  'nvim-mini/mini.nvim',
  version = '*',
  config = function()
    local mini_ai = require 'mini.ai'
    local mini_surround = require 'mini.surround'
    local MiniFiles = require('mini.files')
    local statusline = require 'mini.statusline'
    local which_key = require 'which-key'

    mini_ai.setup { n_lines = 500 }
    mini_surround.setup()
    MiniFiles.setup()
    statusline.setup { use_icons = flase }

    ---@diagnostic disable-next-line: duplicate-set-field
    statusline.section_location = function()
      return '%2l:%-2v'
    end
    

    local function open_mini_files_current_dir()
      local buf_name = vim.api.nvim_buf_get_name(0)
      local current_dir_path = vim.fn.fnamemodify(buf_name, ':h')

      if current_dir_path == '' then
        current_dir_path = vim.fn.getcwd()
      end

      -- Close mini.files if it's already open (optional, but good for toggling)
      MiniFiles.close()

      MiniFiles.open(current_dir_path)

      -- Optionally, reveal the current working directory in mini.files
      -- This helps if the opened path is different from the true cwd.
      vim.defer_fn(function()
          MiniFiles.reveal_cwd()
      end, 30) -- Defer slightly to ensure mini.files has rendered

    end

    _G.MiniFilesOpenCurrentDir = open_mini_files_current_dir

    which_key.add {
      { '<leader>mf', MiniFiles.open, desc = '[M]ini Files' },
      { '<leader>mc', '<cmd>lua MiniFilesOpenCurrentDir()<CR>', desc = '[M]ini Files (Current Dir)' },
    }
  end,
}
