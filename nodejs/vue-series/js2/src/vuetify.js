import Vue from 'vue'
import Vuetify from 'vuetify'
import VueCompositionApi from '@vue/composition-api'
import 'vuetify/dist/vuetify.min.css'

Vue.use(Vuetify)
Vue.use(VueCompositionApi)

const opts = {}

export default new Vuetify(opts)
