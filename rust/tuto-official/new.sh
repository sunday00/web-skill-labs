#!/bin/zsh
project=$1;

cargo new $project;

node new.js $project;
