## add dep

```shell
zig fetch --save package {..url} 

# eg

zig fetch --save https://github.com/JakubSzark/zig-string/archive/refs/heads/master.tar.gz
```

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

## build

```shell
zig build -Doptimize=ReleaseSafe # with optimize

zig build run -Doptimize=ReleaseSafe # build and run

# skip safe check
zig build run -Doptimize=ReleaseFast # build using more memory then super fast
zig build run -Doptimize=ReleaseSmall # build a little fast but super small size
```