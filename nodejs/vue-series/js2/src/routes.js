import Vue from 'vue'
import VueRouter from 'vue-router'
import NikeThing from './domains/ytb1/nike-thing/NikeThing.vue'
import VuexExample from './domains/ytb2/vuex-tuto/VuexExample.vue'
import PracticeVuetify from './domains/practice/vuetify/PracticeVuetify.vue'

Vue.use(VueRouter)

export const routes = [
  {
    path: '/ytb1/nike-thing',
    name: 'ytb1-nike',
    component: NikeThing,
  },
  {
    path: '/ytb2/vuex-tuto',
    name: 'vuex-tuto',
    component: VuexExample,
  },
  {
    path: '/sample/vuetify',
    name: 'practice-vuetify',
    component: PracticeVuetify,
  },
]

export const router = new VueRouter({
  mode: 'history',
  base: 'localhost:5173',
  routes,
})
