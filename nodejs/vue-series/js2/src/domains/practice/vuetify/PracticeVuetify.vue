<template>
  <v-app id="app">
    <v-main>
      <v-container>
        <v-tabs dark v-model="tab">
          <v-tab>Modal</v-tab>
          <v-tab>Item Two</v-tab>
          <v-tab>Item Three</v-tab>
        </v-tabs>

        <v-tabs-items v-model="tab">
          <v-tab-item> <vt-modal /> </v-tab-item>
        </v-tabs-items>
      </v-container>
    </v-main>
  </v-app>
</template>

<script>
import VtModal from './sub/VtModal.vue'

export default {
  name: 'PracticeVuetify',
  components: { VtModal },
  data() {
    return {
      tab: null,
    }
  },
}
</script>
