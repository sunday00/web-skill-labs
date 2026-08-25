// console.log('service worker inside sw.js')

const cacheName = 'app-shell-rsrs-v2'
const dynamicCacheName = 'v1'
const assets = [
    'index.html', '/',
    'js/app.js',
    'js/db.js',
    'js/common.js',
    'js/materialize.min.js',
    'css/styles.css',
    'css/materialize.min.css',
    'img/pkcontacts.png',
    'img/favicon.svg',
    'https://fonts.googleapis.com/icon?family=Material+Icons',
    'https://fonts.gstatic.com/s/materialicons/v145/flUhRq6tzZclQEJ-Vdg-IuiaDsNcIhQ8tQ.woff2',
    'pages/default.html',
    // 'https://www.gstatic.com/firebasejs/12.18.0/firebase-app.js',
    // "https://www.gstatic.com/firebasejs/12.18.0/firebase-firestore.js",
    // "https://www.gstatic.com/firebasejs/12.18.0/firebase-analytics.js",
    // "https://firebase.googleapis.com/v1alpha/projects/-/apps/1:388225997919:web:d3ef3b10b13ec396bfcfea/webConfig"
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

self.addEventListener('push', (evt) => {
    // payload 파싱 (문자열 or JSON 둘 다 대응)
    let data = {title: '기본 알림', body: ''}
    if (evt.data) {
        try {
            data = evt.data.json()
        } catch {
            data = {title: '알림', body: evt.data.text()}
        }
    }

    evt.waitUntil(
        self.registration.showNotification(data.title, {
            body: data.body,
            icon: '/img/icons/icon-192.png',
            badge: '/img/icons/icon-96.png',
            data: data.data,        // 클릭 처리에서 사용
            actions: [              // 알림에 버튼 추가
                {action: 'open', title: '와우~ 함 보까?'},
                {action: 'dismiss', title: '아 꺼져'}
            ]
        })
    )
})

// 알림 클릭 처리
self.addEventListener('notificationclick', (evt) => {
    evt.notification.close()

    if (evt.action === 'dismiss') return

    evt.waitUntil(
        clients.matchAll({type: 'window'}).then(cs => {
            // 이미 열린 창 있으면 포커스
            for (const c of cs) {
                if ('focus' in c) return c.focus()
            }
            // 없으면 새로 열기
            if (clients.openWindow) {
                return clients.openWindow(evt.notification.data?.url || '/')
            }
        })
    )
})