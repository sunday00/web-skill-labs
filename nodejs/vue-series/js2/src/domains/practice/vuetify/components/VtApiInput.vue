<template>
  <div class="vt-api-input">
    <v-form ref="form" v-model="valid" lazy-validation>
      <v-text-field
        v-model="name"
        :counter="10"
        :rules="nameRules"
        label="Name"
        required
      ></v-text-field>

      <v-btn :disabled="!valid" color="success" class="mr-4" @click="submit">
        Validate
      </v-btn>
    </v-form>
  </div>
</template>

<script>
export default {
  name: 'VtApiInput',
  data() {
    return {
      valid: false,
      name: '',
      nameRules: [
        (v) => !!v || 'Name is required',
        (v) => (v && v.length <= 10) || 'Name must be less than 10 characters',
      ],
    }
  },
  methods: {
    async submit() {
      this.$refs.form.validate()

      if (this.valid) {
        const user = {
          id:
            (this.$store.getters.practiceUsers[
              this.$store.getters.practiceUsers.length - 1
            ]?.id ?? 0) + 1,
          name: this.name,
        }

        await this.$store.dispatch('appendPracticeUsers', user)

        this.reset()
        this.resetValidation()
        this.name = ''
      }
    },
    reset() {
      this.$refs.form.reset()
    },
    resetValidation() {
      this.$refs.form.resetValidation()
    },
  },
}
</script>

<style lang="scss" scoped>
.vt-api-input {
  margin-bottom: 3rem;
}
</style>
