// console.log('service worker inside sw.js')

const cacheName = 'app-shell-rsrs-v2'
const dynamicCacheName = 'v1'
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
    'https://fonts.gstatic.com/s/materialicons/v145/flUhRq6tzZclQEJ-Vdg-IuiaDsNcIhQ8tQ.woff2',
    'pages/default.html'
]

// caches.open(cacheName).then(cache => {
//     cache.addAll(assets)
// })

const limitCacheSize = (name, size) => {
    caches.open(name).then(cache => {
        cache.keys().then(keys => {
            if (keys.length > size) {
                cache.delete(keys[0]).then(limitCacheSize(name, size))
            }
        })
    })
}

self.addEventListener('install', evt => {
    // console.log('serviceWorker installed')

    evt.waitUntil(
        caches.open(cacheName).then(cache => cache.addAll(assets))
    )


})

self.addEventListener('activate', evt => {
    console.log('serviceWorker activated')

    evt.waitUntil(
        caches.keys().then(keys => {
            return Promise.all(
                keys.filter(key => key !== cacheName)
                    .map(key => caches.delete())
            )
        })
    )
})

self.addEventListener('fetch', (evt) => {
    // if (caches.has(evt.request)) {
    evt.respondWith(
        caches.match(evt.request).then(res => {
            if (!res) console.log(evt.request.url)
            return res || fetch(evt.request).then(fRes => {
                return caches.open(dynamicCacheName).then(cache => {
                    cache.put(evt.request.url, fRes.clone())
                    limitCacheSize(dynamicCacheName, 5)
                    return fRes
                })
            })
        }).catch(e => {
            if (evt.request.url.indexOf('.html') > -1) {
                return caches.match('page/default.html')
            }
        })
    )
    // }
})