FROM jinwuzhao/emacs:1.0.7

ENV TERM=xterm-256color \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_CTYPE=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    TIMEZONE=Asia/Shanghai

WORKDIR /root

COPY ./install.sh /usr/local/bin/install_toolchains.sh
COPY ./.emacs .
COPY ./.emacs.d ./.emacs.d

RUN chmod +x /usr/local/bin/install_toolchains.sh
RUN install_toolchains.sh

VOLUME ["/mnt/share/Documents","/mnt/share/configs"]

ENTRYPOINT ["launch_emacs.sh"]
