// console.log('service worker inside sw.js')

const cacheName = 'app-shell-rsrs'
const assets = [
    'index.html', '/',
    'js/app.js',
    'js/common.js',
    'js/materialize.min.js',
    'css/styles.css',
    'css/materialize.min.css',
    'img/pkcontacts.png',
    'img/favicon.svg',
    'https://fonts.googleapis.com/icon?family=Material+Icons'
]

caches.open(cacheName).then(cache => {
    cache.addAll(assets)
})

self.addEventListener('install', evt => {
    console.log('serviceWorker installed')
})

self.addEventListener('activate', evt => {
    console.log('serviceWorker activated')
})

self.addEventListener('fetch', (evt) => {
    console.log(evt)
})