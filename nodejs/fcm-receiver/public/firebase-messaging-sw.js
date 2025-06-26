// /public/firebase-messaging-sw.js
importScripts(
  "https://www.gstatic.com/firebasejs/11.8.1/firebase-app-compat.js"
);
importScripts(
  "https://www.gstatic.com/firebasejs/11.8.1/firebase-messaging-compat.js"
);

self.addEventListener("install", function (e) {
  self.skipWaiting();
});

self.addEventListener("activate", function (e) {
  console.log("fcm service worker가 실행되었습니다.");
});

// self.addEventListener("push", (event) => {
//   console.log(event)
//   const payload = JSON.parse(event.data.text());
//   event.waitUntil(
//       registration.showNotification(payload.title, {
//         body: payload.body,
//         data: { link: payload.link },
//       })
//   );
// });

// const firebaseConfig = {
//   apiKey: "AIzaSyAYLae0yPW5PLeIlcCpGu_tVB5PrDoDs3I",
//   authDomain: "iron-common.firebaseapp.com",
//   projectId: "iron-common",
//   storageBucket: "iron-common.firebasestorage.app",
//   messagingSenderId: "51367295915",
//   appId: "1:51367295915:web:94446d8310c6bdafb5b0ee"
// };

const firebaseConfig = {
  apiKey: "AIzaSyCM7zTa6BrXUA6S4EjhMg68hSYAOIIYns4",
  authDomain: "everybody-proam-9d4ac.firebaseapp.com",
  projectId: "everybody-proam-9d4ac",
  storageBucket: "everybody-proam-9d4ac.firebasestorage.app",
  messagingSenderId: "262426257724",
  appId: "1:262426257724:web:054d8eab20c7507b181639",
  measurementId: "G-J3XDCH0W1H"
};

// const app = initializeApp(firebaseConfig);
firebase.initializeApp(firebaseConfig);

// const messaging = getMessaging(app);
const messaging = firebase.messaging();

messaging.onBackgroundMessage((payload) => {
  console.log('back!')
  const notificationTitle = payload.title;
  const notificationOptions = {
    body: payload.body,
    // icon: payload.icon
  };
  self.registration.showNotification(notificationTitle, notificationOptions);
});
