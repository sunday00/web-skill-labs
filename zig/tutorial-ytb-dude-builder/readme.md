## test

```shell
zig build test --summary all
```

## build for debug

```shell
zig build -Doptimize=Debug
lldb zig-out/bin/tutorial_ytb_dude_builder

(lldb) b main.zig:19
(lldb) b p07-memory/basic.zig:23

(lldb) run
```