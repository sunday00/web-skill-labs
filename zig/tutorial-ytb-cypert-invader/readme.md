##wasm build

```shell
zig fetch --save=emsdk git+https://github.com/emscripten-core/emsdk#4.0.9

zig build --build-file build.wasm.zig

# or, small file build

zig build --build-file build.wasm.zig -Doptimize=ReleaseSmall

zig build --build-file build.wasm.zig
```

- this shows error
    ```shell
      failed command: zig-pkg/N-V-... ... ... /invaders.html
    ```
    - but this can be ignored. see zig-out/web folder and run imdex.html  