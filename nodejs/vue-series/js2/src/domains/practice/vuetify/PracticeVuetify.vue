<template>
  <v-app id="app">
    <v-main>
      <v-container>
        <v-tabs dark v-model="tab">
          <v-tab>Modal</v-tab>
          <v-tab>Api</v-tab>
          <v-tab>Item Three</v-tab>
        </v-tabs>

        <v-tabs-items v-model="tab">
          <v-tab-item> <vt-modal /> </v-tab-item>
          <v-tab-item> <vt-api /> </v-tab-item>
        </v-tabs-items>
      </v-container>
    </v-main>
  </v-app>
</template>

<script>
import VtModal from './sub/VtModal.vue'
import VtApi from './sub/VtApi.vue'

export default {
  name: 'PracticeVuetify',
  components: { VtModal, VtApi },
  data() {
    return {
      tab: null,
    }
  },
}
</script>
