<template>
  <nav>
    <ul>
      <li v-for="item in routes">
        <router-link :to="item.path" class="contrast">{{
          item.name
        }}</router-link>
      </li>
    </ul>
  </nav>
</template>

<script>
import { routes } from '../routes.js'

export default {
  name: 'Gnb',
  data() {
    return {
      routes,
    }
  },
}
</script>

<style lang="scss" scoped>
@use '@picocss/pico/scss/pico.scss';

nav {
  padding: 1em;

  ul {
    display: flex;
    gap: 1em;
  }
}
</style>
