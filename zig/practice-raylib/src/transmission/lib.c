#include <emscripten.h>

EM_JS(void, _callJs, (const char* ptr), {
    var evt = UTF8ToString(ptr);
    FromWASM.fire(evt).then((r) => {  })
});

void callJs(const char* str) {
    _callJs(str);
}


