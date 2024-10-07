import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    showLogo: true,
  },
  getters: {
    showLogo: (state) => state.showLogo,
  },
  mutations: {
    mutateLogoStateToggle(state) {
      state.showLogo = !state.showLogo
    },
  },
  actions: {
    toggleShowLogo(context) {
      context.commit('mutateLogoStateToggle')
    },
  },
})
