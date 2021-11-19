FROM docker.io/archlinux:latest as build1
RUN pacman -Syyu --noconfirm \
    && pacman -S --noconfirm base-devel git curl wget emacs sqlite openssl openssh

FROM build1 as build2
RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1 \
    && echo "export PATH=/root/.asdf/bin:\$PATH" >> ~/.bashrc \
    && echo ". /root/.asdf/asdf.sh" >> ~/.bashrc \
    && chmod +x ~/.asdf/asdf.sh \
    && chmod +x ~/.asdf/bin/asdf

# FROM build2 as build3
# RUN source ~/.bashrc \
#     && asdf plugin add nodejs \
#     && asdf install nodejs lts \
#     && asdf global nodejs lts \
#     && npm i -g eslint @babel/eslint-parser eslint-plugin-react typescript typescript-language-server vscode-{css,html}-languageserver-bin vscode-json-languageserver stylelint prettier marked eslint-plugin-prettier yaml-language-server vls pyright yarn

# FROM build3 as build4
# RUN source ~/.bashrc \
#     && asdf plugin add rust \
#     && asdf install rust 1.56.1 \
#     && asdf global rust 1.56.1 \
#     && rustup component add rls clippy rust-analysis \
#     && cargo install lsd fd-find ripgrep bat \
#     && asdf reshim rust

# FROM build4 as build5
# RUN git clone https://github.com/xevrem/customacs.git ~/.emacs.d
# RUN mkdir ~/org \
#     && touch ~/org/tasks.org  ~/org/journal.org ~/org/ideas.org

# FROM build5 as build6
# RUN pacman -S --noconfirm fish
# RUN mkdir -p ~/.config/fish \
#     && touch ~/.config/fish/config.fish
# RUN fish \
#     && set TERM xterm-256color \
#     && set COLORTERM truecolor
# RUN chsh -s /usr/bin/fish root

# from build6 as build7
# RUN fish \
#     && curl -sL https://git.io/fisher | source \
#     && fisher install jorgebucaran/fisher \
#     && fisher install jethrokuan/z \
#     && fisher install acomagu/fish-async-prompt \
#     && fisher install pure-fish/pure


# CMD fish
