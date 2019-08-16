#!/bin/sh

pacman -Syu --noconfirm

pacman -S --noconfirm go
pacman -S --noconfirm mysql
pacman -S --noconfirm npm
pacman -S --noconfirm flex
pacman -S --noconfirm bison
pacman -S --noconfirm protobuf

pacman -Scc --noconfirm

npm install -g gitbook-cli

go version

cd /mnt
mkdir /mnt/go
cd /mnt/go
export GOPATH=`pwd`

go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v github.com/fatih/gomodifytags
go get -u -v golang.org/x/tools/cmd/godoc
go get -u -v github.com/k0kubun/pp
go get -u -v github.com/motemen/gore/cmd/gore
go get -u -v golang.org/x/tools/cmd/gopls
go get -u -v github.com/golangci/golangci-lint/cmd/golangci-lint

cp bin/* /usr/local/bin/

cd /mnt
rm -rf go
rm -rf ~/.cache

curl -o /root/.emacs.d/custom/awesome-tab.el https://raw.githubusercontent.com/manateelazycat/awesome-tab/master/awesome-tab.el

emacs --daemon
emacsclient -e '(kill-emacs)'
