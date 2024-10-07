<template>
  <section class="reviews">
    <h2>Reviews</h2>
    <p v-if="!reviewList.length">There is no reviews yet.</p>
    <ul>
      <li v-for="review in reviewList">
        <p>{{ review.review }}</p>
        <span>
          - {{ review.name }}
          <strong>Rating: {{ review.rating }}</strong>
        </span>
      </li>
    </ul>
  </section>
</template>

<script>
export default {
  name: 'ReviewsList',
  props: {
    reviewList: {
      required: true,
      type: Array,
    },
  },
}
</script>
