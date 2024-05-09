const express = require('express')
const { RPCObserver, RPCRequest } = require('./rpc')
const { customerMock } = require('./mock/customer')
const PORT = 9001

const app = express()
app.use(express.json())

RPCObserver('CUSTOMER_RPC', customerMock)

app.get('/wishlist', async (req, res) => {
  try {
    const resData = await RPCRequest('PRODUCT_RPC', {
      productId: 'CqZ0m6OhXWBx+HZU',
      customerId: customerMock._id,
    })

    return res.status(200).json(resData)
  } catch (err) {
    console.log(err)
    return res.status(500).json(err)
  }
})

app.get('/', (req, res) => {
  return res.json('customer service')
})

app.listen(PORT, () => {
  console.log(`Customer service listening on port ${PORT}`)
  console.clear()
})
