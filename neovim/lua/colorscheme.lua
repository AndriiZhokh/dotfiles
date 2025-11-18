-- local colorscheme = 'everforest'
-- local colorscheme = 'yugen'
-- local colorscheme = 'nord'
-- local colorscheme = 'terafox'
local colorscheme = 'compline'

local status_ok, _ = pcall(vim.cmd, 'colorscheme ' .. colorscheme)

if not status_ok then
    vim.notify('colorscheme ' .. colorscheme .. ' not found')
    return
end
