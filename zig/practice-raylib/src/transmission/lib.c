#include <emscripten.h>

EM_JS(void, _sendDataToJS, (const char* ptr, int len), {
    var message = UTF8ToString(ptr, len);
//    console.log("C Bridge 수신:", message);
    window.callApi()
});

void sendDataToJS(const char* str, int len) {
    _sendDataToJS(str, len);
}


