<template>
  <section class="review-form">
    <form @submit.prevent="handleSubmit">
      <div class="errorMessage" v-if="errors.length">
        <h3>Please correct the following error(s):</h3>
        <ul>
          <li v-for="(error, index) in errors" :key="index">{{ error }}</li>
        </ul>
      </div>
      <div class="row">
        <label for="name">Name:</label>
        <input type="text" id="name" v-model="name" />
      </div>
      <div class="row">
        <label for="review">Review:</label>
        <textarea id="review" v-model="review"></textarea>
      </div>
      <div class="row">
        <label for="rating">Rating:</label>
        <select id="rating" v-model.number="rating">
          <option>5</option>
          <option>4</option>
          <option>3</option>
          <option>2</option>
          <option>1</option>
        </select>
      </div>
      <div class="row">
        <button type="submit">Submit</button>
      </div>
    </form>
  </section>
</template>

<script>
import { eventBus } from '../../../../main.js'

export default {
  data() {
    return {
      name: null,
      review: null,
      rating: null,
      errors: [],
    }
  },
  methods: {
    handleSubmit() {
      this.errors = []

      if (!this.name) {
        this.errors.push('name is required')
      }

      if (!this.review) {
        this.errors.push('review is required')
      }

      if (!this.rating) {
        this.errors.push('rating is required')
      }

      if (this.errors.length > 0) {
        return
      }

      const review = {
        name: this.name,
        review: this.review,
        rating: this.rating,
      }

      // this.$emit('submit', review)
      eventBus.$emit('addReview', review)

      this.name = null
      this.review = null
      this.rating = null
    },
  },
}
</script>
