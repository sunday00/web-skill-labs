// console.log('service worker inside sw.js')

const cacheName = 'app-shell-rsrs-v1'
const assets = [
    'index.html', '/',
    'js/app.js',
    'js/common.js',
    'js/materialize.min.js',
    'css/styles.css',
    'css/materialize.min.css',
    'img/pkcontacts.png',
    'img/favicon.svg',
    'https://fonts.googleapis.com/icon?family=Material+Icons',
    'https://fonts.gstatic.com/s/materialicons/v145/flUhRq6tzZclQEJ-Vdg-IuiaDsNcIhQ8tQ.woff2'
]

// caches.open(cacheName).then(cache => {
//     cache.addAll(assets)
// })

self.addEventListener('install', evt => {
    // console.log('serviceWorker installed')

    evt.waitUntil(
        caches.open(cacheName).then(cache => cache.addAll(assets))
    )


})

self.addEventListener('activate', evt => {
    console.log('serviceWorker activated')
})

self.addEventListener('fetch', (evt) => {
    // if (caches.has(evt.request)) {
    evt.respondWith(
        caches.match(evt.request).then(res => {
            if (!res) console.log(evt.request.url)
            return res || fetch(evt.request)
        })
    )
    // }
})