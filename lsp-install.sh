#!/bin/bash

set -xe

npm i -g @prisma/language-server &&
npm i -g pyright &&
npm i -g typescript-language-server &&
npm i -g intelephense &&
cargo install --git https://github.com/latex-lsp/texlab --tag v5.12.0
