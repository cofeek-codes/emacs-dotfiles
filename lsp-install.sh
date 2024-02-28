#!/bin/bash

set -xe

# ================== node lsps ==================
echo "================== node lsps =================="

npm i -g @prisma/language-server &&
npm i -g pyright &&
npm i -g typescript-language-server &&
npm i -g intelephense &&
npm i -g livedown

# ================== cargo lsps ==================
echo "================== cargo lsps =================="

cargo install --git https://github.com/latex-lsp/texlab --tag v5.12.1 &&
cargo install slint-lsp

# ================== other stuff ==================
echo "================== other stuff =================="

pip install pygments 
