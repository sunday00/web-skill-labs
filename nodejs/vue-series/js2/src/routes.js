import Vue from 'vue'
import VueRouter from 'vue-router'
import NikeThing from './domains/ytb1/nike-thing/NikeThing.vue'

Vue.use(VueRouter)

export const routes = [
  {
    path: '/ytb1/nike-thing',
    name: 'ytb1-nike',
    component: NikeThing,
  },
]

export const router = new VueRouter({
  mode: 'history',
  base: 'localhost:5173',
  routes,
})
