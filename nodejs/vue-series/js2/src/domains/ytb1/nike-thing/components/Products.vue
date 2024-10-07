<template>
  <section class="container">
    <div class="product">
      <div class="image">
        <img :src="selected.img" :alt="product" />
      </div>
      <div class="content">
        <h1>
          {{ title }}
        </h1>
        <div class="stockInfo">
          <span class="green" v-if="selected.qty > 10">In stock</span>
          <span class="amber" v-else-if="selected.qty <= 10 && selected.qty > 0"
            >few left</span
          >
          <span class="red" v-else>Out of stock</span>
        </div>
        <div class="features">
          <ul>
            <li v-for="(feature, idx) in features" :key="idx">
              {{ feature }}
            </li>
          </ul>
        </div>
        <div class="variants">
          <span
            v-for="(variant, idx) of variants"
            :key="variant.id"
            @mouseover="updateVariant(idx)"
            class="colorBox"
            :style="{ backgroundColor: variant.color }"
          ></span>
        </div>
        <div class="addCart">
          <button
            @click="addToCart"
            :disabled="selected.qty <= 0"
            :class="{ disabledState: selected.qty <= 0 }"
          >
            add to cart
          </button>
        </div>
      </div>
    </div>
    <product-review />
  </section>
</template>

<script>
import nikeRed from '../assets/nike-red.jpg'
import nikeWhite from '../assets/nike-white.jpg'
import nikeBlack from '../assets/nike-black.jpg'
import ProductReview from './ProductReview.vue'

export default {
  name: 'Product',
  components: { ProductReview },
  data() {
    return {
      brand: 'Nike',
      product: 'Nike Air Force',
      selectedIdx: 0,
      features: ['durable', 'secure', 'padded'],
      variants: [
        { id: 1, color: 'red', img: nikeRed, qty: 9 },
        { id: 2, color: 'black', img: nikeBlack, qty: 13 },
        { id: 3, color: 'white', img: nikeWhite, qty: 0 },
      ],
      cart: 0,
    }
  },
  computed: {
    title() {
      return `${this.brand} ${this.product}`
    },
    selected() {
      return this.variants[this.selectedIdx]
    },
  },
  methods: {
    addToCart() {
      this.$emit('addToCart', this.selected.id)
      this.selected.qty -= 1
    },
    updateVariant(idx) {
      this.selectedIdx = idx
    },
  },
}
</script>
