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

  const app = initializeApp(firebaseConfig);
  const messaging = getMessaging(app);

  onMessage(messaging, (payload) => {
    console.log({ payload: payload });
  });

  const token = await getToken(messaging, {
    vapidKey:
      // "BM_cB08_xpXfzGrkUcJr79AncK4oQ8T6Jl9y6oCuMD6rIInsZiIGYeVB7jyI5blidjF32VkQLTg4_MapgZxQwlE",
      "BItpeVl0shaaXrFUDa5WwyrQEAQg5tQQlPdKoP8ObPiwGp4HFuTl4HrSRfFpldql2YFP0q86SgcJjLXyvFgMD00",
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
