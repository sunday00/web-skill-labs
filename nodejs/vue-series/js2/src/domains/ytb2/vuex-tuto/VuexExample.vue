<template>
  <div class="pico-pico">
    <div class="container">
      <h2>vuex</h2>

      <section class="image-toggle-set">
        <using-data />
        <using-vuex />
      </section>
    </div>
  </div>
</template>

<script>
import UsingData from './components/UsingData.vue'
import UsingVuex from './components/UsingVuex.vue'

export default {
  name: 'VuexExample',
  components: { UsingVuex, UsingData },
  data() {
    return {}
  },

  mounted() {
    document.querySelector('html').dataset.theme = 'dark'
  },
}
</script>

<style lang="scss" scoped>
.container {
  .image-toggle-set {
    display: flex;
    gap: 1em;

    & > div {
      display: flex;
      flex-direction: column;
      flex: 1;
      justify-content: center;
      align-items: center;
    }
  }
}
</style>
