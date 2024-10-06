import Vue from 'vue'
import App from './App.vue'
import { router } from './routes'
import { library } from '@fortawesome/fontawesome-svg-core'
import * as fas from '@fortawesome/free-solid-svg-icons'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

library.add(
  ...Object.keys(fas).map((k) => {
    return fas[k]
  }),
)

Vue.component('font-awesome-icon', FontAwesomeIcon)

new Vue({
  router,
  render: (h) => h(App),
}).$mount('#app')
