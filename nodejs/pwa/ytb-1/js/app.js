if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('/sw.js')
        .then(reg => console.log('serviceWorker registered', reg))
        .catch(e => console.error('failed to register serviceWorker', e))
}