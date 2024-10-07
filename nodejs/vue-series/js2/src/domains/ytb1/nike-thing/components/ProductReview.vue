<template>
  <section class="product-review">
    <div class="tabs">
      <div class="tabLinks">
        <span
          v-for="(tab, index) in tabs"
          :key="index"
          @click="() => updateTab(tab)"
          class="tab"
          :class="{ activeTab: selectedTabs === tab }"
        >
          {{ tab }}
        </span>
      </div>
      <div class="tabContent">
        <div v-show="selectedTabs === 'ReviewList'">
          <reviews-list :review-list="reviews" />
        </div>
        <div v-show="selectedTabs === 'ReviewForm'">
          <!--          <review-form @submit="addReview" />-->
          <review-form />
        </div>
      </div>
    </div>
  </section>
</template>

<script>
import ReviewForm from './ReviewForm.vue'
import ReviewsList from './ReviewsList.vue'
import { eventBus } from '../../../../main.js'

export default {
  name: 'ProductReview',
  components: { ReviewsList, ReviewForm },
  data() {
    return {
      reviews: [],
      tabs: ['ReviewList', 'ReviewForm'],
      selectedTabs: 'ReviewList',
    }
  },
  methods: {
    // addReview(review) {
    //   this.reviews.push(review)
    //   this.selectedTabs = 'ReviewList'
    // },
    updateTab(tab) {
      this.selectedTabs = tab
    },
  },
  mounted() {
    eventBus.$on('addReview', (review) => {
      this.reviews.push(review)
      this.selectedTabs = 'ReviewList'
    })
  },
}
</script>
