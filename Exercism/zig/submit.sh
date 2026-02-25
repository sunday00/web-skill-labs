#!/bin/zsh

F=${PWD:t}
F=${F//-/_}

echo $F
exercism submit ${F}.zig