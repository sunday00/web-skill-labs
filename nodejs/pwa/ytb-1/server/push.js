import push from "web-push"

if (Array.from(process.argv).includes('-g')) {
    console.log(push.generateVAPIDKeys())
    process.exit()
}

let vapidKeys = {
    publicKey: 'BDjFSd46MiRJLmGOPZRCUHywBfD158bML3sYaVbJzwi80dAYlXUOkEQ6OdcR2vT7LOu1bUkOk8RoZJrWK1ipdOE',
    privateKey: 'Tq2hv0TZ83gC951PtfzKlG9RbSQo-69oYh5P3_tGHt0'
}

push.setVapidDetails('mailto:grayfield00@naver.com', vapidKeys.publicKey, vapidKeys.privateKey)

let sub = {
    "endpoint": "https://fcm.googleapis.com/fcm/send/cImzFiLzH2c:APA91bG47-oeL7k-92Izda6s3yVeovvywcWJlYNlf2GAuSr-FbQU30KqFRHMdYCQhBcMzr6IpvkQjJCudlfdgdRm3d8DCblcqx1H_Gwn2dRYzuNR6WGIfud_JaJMOiLsP2UTFPwospmE",
    "expirationTime": null,
    "keys": {
        "p256dh": "BAIwxTMpKmb_dVKEpLbIi2BGDMh8syJFMZZivkNaATWNW05IvkkUPw3ZilztWu6HPX-bhHcN5N0Yk-zTDHMhtgo",
        "auth": "mn_xmmtGn8rpYxXuK6pbqg"
    }
}
push.sendNotification(sub, 'this is test from server')