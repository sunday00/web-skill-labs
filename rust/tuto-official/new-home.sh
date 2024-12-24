#!/bin/zsh
project=$1;

mise exec -- cargo new $project;

node new.js $project;
