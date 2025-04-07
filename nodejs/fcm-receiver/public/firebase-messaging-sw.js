// /public/firebase-messaging-sw.js
importScripts(
  "https://www.gstatic.com/firebasejs/10.8.0/firebase-app-compat.js"
);
importScripts(
  "https://www.gstatic.com/firebasejs/10.8.0/firebase-messaging-compat.js"
);

self.addEventListener("install", function (e) {
  self.skipWaiting();
});

self.addEventListener("activate", function (e) {
  console.log("fcm service worker가 실행되었습니다.");
});

const firebaseConfig = {
  apiKey: "AIzaSyCrY-f_n-Grw4BIiJtJ94LDTdEpznY7Adw",
  authDomain: "kakaovx-test-grey.firebaseapp.com",
  projectId: "kakaovx-test-grey",
  storageBucket: "kakaovx-test-grey.appspot.com",
  messagingSenderId: "395626435379",
  appId: "1:395626435379:web:033b14433cc5d19bea6446",
};

// const app = initializeApp(firebaseConfig);
firebase.initializeApp(firebaseConfig);

// const messaging = getMessaging(app);
const messaging = firebase.messaging();

// messaging.onBackgroundMessage((payload) => {
//   const notificationTitle = payload.title;
//   const notificationOptions = {
//     body: payload.body,
//     // icon: payload.icon
//   };
//   self.registration.showNotification(notificationTitle, notificationOptions);
// });
