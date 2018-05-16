#!/bin/sh

apk --no-cache add go
apk --no-cache add ca-certificates

if [ ! -e /etc/nsswitch.conf ]
then
    echo 'hosts: files dns' > /etc/nsswitch.conf
fi

cd /mnt

export GOROOT_BOOTSTRAP="$(go env GOROOT)"
export GOOS="$(go env GOOS)"
export GOARCH="$(go env GOARCH)"
export GOHOSTOS="$(go env GOHOSTOS)"
export GOHOSTARCH="$(go env GOHOSTARCH)"
wget -O go.tar.gz https://dl.google.com/go/go1.10.2.src.tar.gz
tar -C /usr/local -xzf go.tar.gz
rm go.tar.gz
cd /usr/local/go/src
./make.bash
apk del go
ln -s /usr/local/go/bin/* /usr/local/bin/
go version

mkdir /mnt/go
cd /mnt/go
export GOPATH=`pwd`

go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/oracle
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v github.com/alecthomas/gometalinter
./bin/gometalinter --install --update
cp bin/* /usr/local/bin/

cd /mnt
rm -rf go
