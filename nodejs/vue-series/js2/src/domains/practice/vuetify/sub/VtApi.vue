<template>
  <div>
    <vt-api-input />

    <v-spacer />

    <v-list>
      <v-list-item v-for="(user, idx) in practiceUsers" :key="user.id">
        <v-list-item-content
          :class="[idx % 2 ? '' : 'grey lighten-3', 'list-item']"
        >
          {{ user.name }}
        </v-list-item-content>
      </v-list-item>
    </v-list>
  </div>
</template>

<script>
import VtApiInput from '../components/VtApiInput.vue'

export default {
  name: 'VtApi',
  components: { VtApiInput },
  data() {
    return {}
  },
  computed: {
    practiceUsers() {
      return this.$store.getters.practiceUsers
    },
  },
  mounted() {
    this.$store.dispatch('fetchPracticeUsers')
  },
}
</script>

<style lang="scss" scoped>
.list-item {
  padding: 1rem;
}
</style>
