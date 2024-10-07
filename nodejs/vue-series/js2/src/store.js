import Vue from 'vue'
import Vuex from 'vuex'
import { appendUser, getFooBar } from './apis/foobar.js'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    showLogo: true,
    practiceUsers: [],
  },
  getters: {
    showLogo: (state) => state.showLogo,
    practiceUsers: (state) => state.practiceUsers,
  },
  mutations: {
    mutateLogoStateToggle(state) {
      state.showLogo = !state.showLogo
    },
    async mutatePracticeUsers(state) {
      state.practiceUsers = await getFooBar()
    },
    async appendUser(state, payload) {
      const result = await appendUser([...state.practiceUsers], payload)
      if (result <= 299) {
        state.practiceUsers = [...state.practiceUsers, payload]
      }
    },
  },
  actions: {
    toggleShowLogo(context) {
      context.commit('mutateLogoStateToggle')
    },
    fetchPracticeUsers(context) {
      context.commit('mutatePracticeUsers')
    },
    async appendPracticeUsers(context, payload) {
      context.commit('appendUser', payload)
    },
  },
})
