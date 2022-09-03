# FROM docker.io/archlinux:latest as build1
FROM archlinux:latest as build1
RUN pacman -Syyu --noconfirm \
    && pacman -S --noconfirm base-devel git curl wget sqlite openssl openssh

FROM build1 as build2
RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1 \
    && echo "export PATH=/root/.asdf/bin:\$PATH" >> ~/.bashrc \
    && echo ". /root/.asdf/asdf.sh" >> ~/.bashrc \
    && chmod +x ~/.asdf/asdf.sh \
    && chmod +x ~/.asdf/bin/asdf

FROM build2 as build3
RUN pacman -S --noconfirm nodejs-lts-gallium npm \
    && npm add --location=global bash-language-server eslint @babel/eslint-parser prettier typescript typescript-language-server vscode-langservers-extracted
        
FROM build3 as build4
RUN pacman -S --noconfirm rustup fd ripgrep bat bottom nushell \
    && rustup default stable \
    && rustup component add rls clippy rust-analysis

FROM build4 as build5
RUN git clone https://github.com/xevrem/customacs.git ~/.emacs.d
RUN pacman -S --noconfirm emacs \
    && mkdir ~/org \
    && touch ~/org/tasks.org  ~/org/journal.org ~/org/ideas.org \
    && mkdir -p ~/.cache/emacs \
    && emacs -Q --script ~/.emacs.d/docker-init.el

FROM build5 as build6
RUN pacman -S --noconfirm fish
WORKDIR ~/.config
RUN git clone https://github.com/xevrem/fish-config.git ~/.config/fish
RUN fish \
    && set TERM xterm-256color \
    && set COLORTERM truecolor
RUN chsh -s /usr/bin/fish root


CMD fish
