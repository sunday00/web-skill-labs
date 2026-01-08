import { Injectable } from '@nestjs/common'
import FB, { ServiceAccount } from 'firebase-admin'
import account from 'pem/firebase-sdk.json'

@Injectable()
export class FirebaseService {
  public readonly conf: FB.remoteConfig.RemoteConfig
  private app: FB.app.App

  constructor() {
    this.app = FB.initializeApp({
      credential: FB.credential.cert(account as ServiceAccount),
    })

    this.conf = this.app.remoteConfig()
  }

  public async get(key?: string) {
    const temp = await this.conf.getTemplate()

    // console.log(temp.etag)

    if (!key) return { ...temp.parameters, ...temp.parameterGroups }

    if (!key.includes('.')) {
      const directValue = temp.parameters[key]

      if (directValue) return directValue

      return temp.parameterGroups[key]?.parameters
    }

    const keys = key.split('.')
    let v: any = temp.parameterGroups
    for (const k of keys) {
      v = 'parameters' in v[k] ? v[k].parameters : v[k].defaultValue.value
    }

    return v
  }

  public async get2(key?: string) {
    const temp = await this.conf.getServerTemplate()

    // await temp.load()

    // console.log(temp.toJSON())

    return temp
  }

  public async get3(key: string) {
    const temp = await this.conf.getTemplate()

    return temp.parameters[key].defaultValue as { value: string }
  }
}
