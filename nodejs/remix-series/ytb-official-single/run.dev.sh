#!/bin/zsh

mode=$1;
host=$2;
echo "$mode : $host"

rm -rf ./.env
if [ $mode = 'local' ]; then
  cp ./_.env.local ./.env
else
  cp ./_.env.dev ./.env
fi

NODE_ENV=development remix vite:dev --host=$host