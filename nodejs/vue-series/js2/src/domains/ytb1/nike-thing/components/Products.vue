<template>
  <section class="container">
    <header class="header">
      <div class="container">
        <div class="cart">
          <p>
            <font-awesome-icon
              icon="fa-solid fa-shopping-cart"
            ></font-awesome-icon>
            {{ cart }}
          </p>
        </div>
      </div>
    </header>
    <div class="product">
      <div class="image">
        <img :src="productImage" :alt="product" />
      </div>
      <div class="content">
        <h1>
          {{ product }}
        </h1>
        <div class="stockInfo">
          <span class="green" v-if="inventory > 10">In stock</span>
          <span class="amber" v-else-if="inventory <= 10 && inventory > 0"
            >few left</span
          >
          <span class="red" v-else>Out of stock</span>
        </div>
        <div class="features">
          <ul>
            <li v-for="(feature, idx) in features" :key="idx">{{ feature }}</li>
          </ul>
        </div>
        <div class="variants">
          <span v-for="variant of variants" :key="variant.id">{{}}</span>
        </div>
      </div>
    </div>
  </section>
</template>

<script>
import nikeRed from '../assets/nike-red.jpg'
import { faShoppingCart } from '@fortawesome/free-solid-svg-icons'

export default {
  name: 'Product',
  methods: {
    faShoppingCart() {
      return faShoppingCart
    },
  },
  data() {
    return {
      product: 'Nike Air Force',
      productImage: nikeRed,
      inventory: 9,
      features: ['durable', 'secure', 'padded'],
      variants: [
        { id: 1, color: 'red' },
        { id: 2, color: 'black' },
        { id: 3, color: 'white' },
      ],
      cart: 0,
    }
  },
}
</script>
