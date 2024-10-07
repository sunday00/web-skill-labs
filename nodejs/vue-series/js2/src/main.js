import Vue from 'vue'
import App from './App.vue'
import { router } from './routes'
import store from './store.js'
import vuetify from './vuetify.js'

export const eventBus = new Vue()

new Vue({
  router,
  store,
  vuetify,
  render: (h) => h(App),
}).$mount('#app')
