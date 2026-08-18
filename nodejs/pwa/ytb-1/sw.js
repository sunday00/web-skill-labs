// console.log('service worker inside sw.js')

self.addEventListener('install', evt => {
    console.log('serviceWorker installed')
})

self.addEventListener('activate', evt => {
    console.log('serviceWorker activated')
})

self.addEventListener('fetch', (evt) => {
    console.log(evt)
})