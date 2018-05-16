FROM jinwuzhao/emacs-cppdev:latest

ENV TERM=xterm-256color \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_CTYPE=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    TIMEZONE=Asia/Shanghai \
    GOPATH=/root/Documents

WORKDIR /root

VOLUME /root/Documents

COPY ./install.sh /usr/local/bin/install_toolchains.sh
COPY ./.spacemacs .

RUN chmod +x /usr/local/bin/install_toolchains.sh
RUN install_toolchains.sh

ENTRYPOINT ["launch_emacs.sh"]