import { useEffect } from "react";
import { initializeApp } from "firebase/app";
import { getToken, getMessaging, onMessage } from "firebase/messaging";

async function handleAllowNotification() {
  if ("serviceWorker" in navigator) {
    window.addEventListener("load", function () {
      navigator.serviceWorker
        .register("firebase-messaging-sw.js")
        .then(function (registration) {
          console.log(
            "Service Worker가 scope에 등록되었습니다.:",
            registration.scope
          );
        })
        .catch(function (err) {
          console.log("Service Worker 등록 실패:", err);
        });
    });
  }

  const firebaseConfig = {
    apiKey: "AIzaSyCrY-f_n-Grw4BIiJtJ94LDTdEpznY7Adw",
    authDomain: "kakaovx-test-grey.firebaseapp.com",
    projectId: "kakaovx-test-grey",
    storageBucket: "kakaovx-test-grey.appspot.com",
    messagingSenderId: "395626435379",
    appId: "1:395626435379:web:033b14433cc5d19bea6446",
  };

  const app = initializeApp(firebaseConfig);
  const messaging = getMessaging(app);

  onMessage(messaging, (payload) => {
    console.log({ payload: payload.notification });
  });

  const token = await getToken(messaging, {
    vapidKey:
      "BNZmmd72BCqCtjhPqyM4aWZNd7Nzbx9VTZQ08QiGPSSfg0ylYtCVn4xIVGGyKmcSRFd2K1i3bN_wyaMvMbg--L4",
  });

  console.log(token);

  return messaging;
}

function App() {
  useEffect(() => {
    handleAllowNotification();
  }, []);

  return (
    <>
      <div></div>
    </>
  );
}

export default App;
