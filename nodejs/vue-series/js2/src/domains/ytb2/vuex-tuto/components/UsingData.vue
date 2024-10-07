<template>
  <div class="using-data-container">
    <button class="outline" role="button" @mousedown="toggleMe">Toggle</button>

    <img :src="vueLogo" alt="vue logo" class="vue-logo" v-if="showLogo" />
  </div>
</template>

<script>
import vueLogo from '../../../../assets/vue.svg'

export default {
  name: 'UsingData',
  data() {
    return {
      vueLogo,
      showLogo: true,
    }
  },
  methods: {
    toggleMe() {
      this.showLogo = !this.showLogo
    },
  },
}
</script>

<style lang="scss" scoped>
div.using-data-container {
  .vue-logo {
    width: 20em;
  }
}
</style>
