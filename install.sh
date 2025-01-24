#!/bin/sh

pacman -Syu --disable-sandbox --noconfirm
pacman -Sy --disable-sandbox --noconfirm go
su yay -s /bin/bash -c 'yay -Sy --noconfirm python-epc python-orjson python-sexpdata python-six python-setuptools python-paramiko python-rapidfuzz python-watchdog python-packaging'
pacman -Scc --disable-sandbox --noconfirm

go version
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
cp -f /root/go/bin/* /usr/local/bin/

rm -rf ~/.cache

curl -o /root/.emacs.d/custom/awesome-tab.el https://raw.githubusercontent.com/manateelazycat/awesome-tab/master/awesome-tab.el
git clone --depth=1 https://github.com/manateelazycat/lsp-bridge.git /root/.emacs.d/custom/lsp-bridge

emacs --daemon
emacsclient -e '(kill-emacs)'
