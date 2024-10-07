<template>
  <div class="vtModal container">
    <h2>Modal</h2>

    <v-dialog v-model="dialog" width="600px">
      <template v-slot:activator="{ on, attrs }">
        <v-btn color="primary" dark v-bind="attrs" v-on="on">
          Open Dialog
        </v-btn>
      </template>

      <div>
        <v-card>
          <v-card-title>
            <p>modal example...</p>
          </v-card-title>

          <v-card-text>OK bye...</v-card-text>

          <v-card-actions>
            <v-spacer />
            <v-btn color="green darken-1" text @click="dialog = false">
              close
            </v-btn>
          </v-card-actions>
        </v-card>
      </div>
    </v-dialog>
  </div>
</template>

<script>
export default {
  name: 'VtModal',
  data() {
    return {
      dialog: false,
    }
  },
}
</script>

<style lang="scss" scoped></style>
