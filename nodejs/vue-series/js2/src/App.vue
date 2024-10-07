<script>
import '/node_modules/@fortawesome/fontawesome-free/css/all.css'
import Gnb from './components/Gnb.vue'

export default {
  name: 'App',
  components: { Gnb },
}
</script>

<template>
  <div>
    <gnb />
    <router-view />
  </div>
</template>

<style lang="scss">
@use '@picocss/pico/scss/pico' with (
  $parent-selector: '.pico-pico'
);
</style>
