import { Injectable } from '@nestjs/common';
import {
  AppStoreServerAPI,
  Environment,
  decodeRenewalInfo,
  decodeTransaction,
  decodeTransactions,
} from 'app-store-server-api';
import * as jwt from 'jsonwebtoken';
import axios from 'axios';

@Injectable()
export class AppStoreService {
  async reqTestNoti() {
    //     const KEY = `-----BEGIN PRIVATE KEY-----
    // MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgtlpRdIFqlm5hrDpl
    // XtA99fV6k2QIoSSeIRnHSQnUC9CgCgYIKoZIzj0DAQehRANCAARDtkzFhN5VTgqx
    // EBhy9VgGD24m1PfNUkF8fNjpjC9YC/vX43olVX2bDldaPsnK2DoNuWlUR62tmWOy
    // 8Ym8OrLu
    // -----END PRIVATE KEY-----`;
    //
    //     const KEY_ID = 'K8H7442FKY';
    //     const ISSUER_ID = 'b8e57981-d723-459a-9981-077966c79dc7';
    //     const APP_BUNDLE_ID = 'com.compositionstudiokr.notekit.ios';
    //
    //     const api = new AppStoreServerAPI(
    //       KEY,
    //       KEY_ID,
    //       ISSUER_ID,
    //       APP_BUNDLE_ID,
    //       Environment.Sandbox,
    //     );
    //
    //     const response = await api.requestTestNotification();
    //
    //     const testNotiToken = response.testNotificationToken;
    //
    //     const response2 = await api.getTestNotificationStatus(testNotiToken);

    // console.log(response2);

    await axios
      .put(
        `http://api.storekit-sandbox.itunes.apple.com/inApps/v1/transactions/consumption/${1}`,
        {
          accountTenure: 0,
          appAccountToken: '',
          consumptionStatus: 1,
          customerConsented: true,
          deliveryStatus: 5,
          lifetimeDollarsPurchased: 2,
          lifetimeDollarsRefunded: 1,
          platform: 1,
          playTime: 0,
          sampleContentProvided: false,
          userStatus: 0,
        },
        {
          headers: {
            Authorization: `Bearer eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6Iks4SDc0NDJGS1kifQ.eyJpc3MiOiJiOGU1Nzk4MS1kNzIzLTQ1OWEtOTk4MS0wNzc5NjZjNzlkYzciLCJleHAiOjE2OTQyNTEyMTUsImF1ZCI6ImFwcHN0b3JlY29ubmVjdC12MSIsImlhdCI6MTY5NDI1MDAxNX0.PAhb2f7klb9L1S4x9PQbyeI0uwiiX9aOKmagMHk0Zg0nxbp-U3N6PzxfEhVf61maKXUnloPsQ6Mj6w3I3M_kMg`,
          },
        },
      )
      .then((res) => {
        console.log(res.data);
      });
  }

  login() {
    // You get privateKey, apiKeyId and issuerId from your Apple App Store Connect account
    const privateKey = `-----BEGIN PRIVATE KEY-----
MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgtlpRdIFqlm5hrDpl
XtA99fV6k2QIoSSeIRnHSQnUC9CgCgYIKoZIzj0DAQehRANCAARDtkzFhN5VTgqx
EBhy9VgGD24m1PfNUkF8fNjpjC9YC/vX43olVX2bDldaPsnK2DoNuWlUR62tmWOy
8Ym8OrLu
-----END PRIVATE KEY-----`;

    const apiKeyId = 'K8H7442FKY';
    const issuerId = 'b8e57981-d723-459a-9981-077966c79dc7';
    const now = Math.round(new Date().getTime() / 1000); // Notice the /1000
    const nowPlus20 = now + 1199; // 1200 === 20 minutes

    const payload = {
      iss: issuerId,
      exp: nowPlus20,
      aud: 'appstoreconnect-v1',
    };

    const token = jwt.sign(
      {
        iss: issuerId,
        exp: nowPlus20,
        aud: 'appstoreconnect-v1',
      },
      privateKey,
      {
        algorithm: 'ES256', // you must use this algorythm, not jsonwebtoken's default
        header: {
          alg: 'ES256',
          kid: apiKeyId,
          typ: 'JWT',
        },
      },
    );
    console.log('@token: ', token);
  }
}
