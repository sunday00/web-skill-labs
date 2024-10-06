<template>
  <section>
    <header id="header">
      <div class="container">
        <div class="cart">
          <p><i class="fas fa-shopping-cart"></i> {{ cart }}</p>
        </div>
      </div>
    </header>
    <section class="container">
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
              <li v-for="(feature, idx) in features" :key="idx">
                {{ feature }}
              </li>
            </ul>
          </div>
          <div class="variants">
            <span
              v-for="variant of variants"
              :key="variant.id"
              @mouseover="updateImage(variant.img)"
              class="colorBox"
              :style="{ backgroundColor: variant.color }"
            ></span>
          </div>
          <div class="addCart">
            <button
              @click="addToCart"
              :disabled="inventory <= 0"
              :class="{ disabledState: inventory <= 0 }"
            >
              add to cart
            </button>
          </div>
        </div>
      </div>
    </section>
  </section>
</template>

<script>
import nikeRed from '../assets/nike-red.jpg'
import nikeWhite from '../assets/nike-white.jpg'
import nikeBlack from '../assets/nike-black.jpg'

export default {
  name: 'Product',
  data() {
    return {
      product: 'Nike Air Force',
      productImage: nikeRed,
      inventory: 12,
      features: ['durable', 'secure', 'padded'],
      variants: [
        { id: 1, color: 'red', img: nikeRed },
        { id: 2, color: 'black', img: nikeBlack },
        { id: 3, color: 'white', img: nikeWhite },
      ],
      cart: 0,
    }
  },
  methods: {
    addToCart() {
      this.cart += 1
      this.inventory -= 1
    },
    updateImage(img) {
      this.productImage = img
    },
  },
}
</script>
