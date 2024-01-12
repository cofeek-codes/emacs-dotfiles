#!/bin/bash

set -xe

npm i -g @prisma/language-server &&
npm i -g pyright &&
npm i -g typescript-language-server &&
npm i -g intelephense &&
sudo npm i -g livedown
cargo install --git https://github.com/latex-lsp/texlab --tag v5.12.1 &&
cargo install slint-lsp
