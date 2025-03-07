#!/bin/zsh

mode=$1;
echo $mode

rm -rf ./.env
if [ $mode = 'local' ]; then
  cp ./_.env.local ./.env
else
  cp ./_.env.dev ./.env
fi

NODE_ENV=development remix vite:dev