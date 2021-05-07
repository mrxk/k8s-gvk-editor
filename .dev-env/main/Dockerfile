FROM alpine

LABEL creator=dev-env

RUN apk add curl vim tmux sudo npm make bash

RUN cd /tmp && \
    curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
    gunzip elm.gz && \
    chmod +x elm && \
    cp elm /usr/local/bin/elm

RUN adduser -D elm elm && \
    echo "elm ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/010_elm-nopasswd

USER elm

COPY tmux.conf /home/elm/.tmux.conf

ENTRYPOINT ["tmux"]
