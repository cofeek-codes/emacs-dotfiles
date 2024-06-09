#!/bin/bash

set -xe

echo "================== node lsps =================="

npm i -g @prisma/language-server &&
npm i -g pyright &&
npm i -g typescript-language-server &&
npm i -g intelephense &&
npm i -g livedown &&
npm install -g perlnavigator-server

echo "================== cargo lsps =================="

cargo install slint-lsp

echo "================== other stuff =================="

pip install pygments 
