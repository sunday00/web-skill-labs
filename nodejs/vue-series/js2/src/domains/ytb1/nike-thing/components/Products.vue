<template>
    <section class="container">
        <div class="product">
            <div class="image">
                <img :src="productImage" />
            </div>
            <div class="content">
                <h1>
                    {{ product }}
                </h1>
            </div>
        </div> 
    </section>
</template>

<script>
import nikeRed from "../assets/nike-red.jpg"

export default {
    name: 'Product',
    data() {
        return {
            product: 'Nike Air Force',
            productImage: nikeRed
        }
    }
}
</script>
