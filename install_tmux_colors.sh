#!/bin/bash

# Install ncurses tools if needed
if ! command -v tic &>/dev/null; then
  echo "[+] Installing ncurses-utils..."
  sudo zypper install -y ncurses-utils || sudo zypper install -y ncurses-devel
fi

# Prepare and compile tmux-256color terminfo
echo "[+] Downloading terminfo.src..."
curl -sSLO https://invisible-island.net/datafiles/current/terminfo.src.gz
gzip -f -d terminfo.src.gz

echo "[+] Compiling and installing tmux-256color terminfo..."
mkdir -p ~/.terminfo
tic -xe tmux-256color -o ~/.terminfo terminfo.src

# Clean up source file
rm -f terminfo.src

# Update .bashrc if needed
if ! grep -q 'TERM=tmux-256color' ~/.bashrc; then
  echo '[+] Adding TERM settings to ~/.bashrc'
  cat << 'EOF' >> ~/.bashrc

# Set terminal for tmux + Neovim truecolor support
export TERM=tmux-256color
export COLORTERM=truecolor
EOF
fi

# Reload shell config
echo "[+] Reloading ~/.bashrc..."
source ~/.bashrc

echo "[✓] Setup complete! You can now use tmux + nvim with full colors."
