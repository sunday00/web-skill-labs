import { initializeApp } from 'firebase/app'
import {
  activate,
  fetchAndActivate,
  getRemoteConfig,
  onConfigUpdate,
  setCustomSignals,
} from 'firebase/remote-config'
import * as cfg from '@/utils/firebase/pem.json'

const app = initializeApp(cfg)
const remoteConfig = getRemoteConfig(app)

// remoteConfig.settings.minimumFetchIntervalMillis = 3600000
remoteConfig.settings.minimumFetchIntervalMillis = 1000 * 10

onConfigUpdate(remoteConfig, {
  next: (configUpdate) => {
    console.log('Updated keys:', configUpdate.getUpdatedKeys())

    // if (configUpdate.getUpdatedKeys().has('welcome_message')) {
    activate(remoteConfig).then(() => {})
    // }
  },
  error: (error) => {
    console.log('Config update error:', error)
  },
  complete: () => {
    console.log('Listening stopped.')
  },
})

fetchAndActivate(remoteConfig)
  .then(() => {
    setCustomSignals(remoteConfig, { kkk: 'vvv' })
      .then(() => {})
      .catch((e) => {
        console.error(e)
      })
  })
  .catch((e) => {
    console.error(e)
  })

export { remoteConfig }
