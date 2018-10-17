#!/bin/sh

pacman -S --noconfirm go
pacman -S --noconfirm mysql-client

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
go get -u -v github.com/motemen/gore
go get -u -v github.com/sourcegraph/go-langserver
go get -u -v github.com/golangci/golangci-lint/cmd/golangci-lint

cp bin/* /usr/local/bin/

cd /mnt
rm -rf go
rm -rf ~/.cache

emacs --daemon
emacsclient -e '(kill-emacs)'
