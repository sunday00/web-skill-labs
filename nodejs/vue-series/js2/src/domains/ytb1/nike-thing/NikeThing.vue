<template>
  <div style="height: 100vh; overflow-y: scroll">
    <header id="header">
      <div class="container">
        <div class="cart">
          <p><i class="fas fa-shopping-cart"></i> {{ cart.length }}</p>
        </div>
      </div>
    </header>
    <product @addToCart="addToCart" />
  </div>
</template>

<script>
import Product from './components/Products.vue'
import './assets/main.css'

export default {
  name: 'NikeThing',
  components: {
    Product,
  },
  data() {
    return {
      cart: [],
    }
  },
  methods: {
    addToCart(id) {
      this.cart.push(id)
    },
  },
}
</script>
