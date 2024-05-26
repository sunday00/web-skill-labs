const express = require('express')
const { RPCObserver, RPCRequest } = require('./rpc')
const { wishlistMock } = require('./mock/customer')
const PORT = 9002

const app = express()
app.use(express.json())

RPCObserver('PRODUCT_RPC', wishlistMock)

app.get('/customer', async (req, res) => {
  try {
    const resData = await RPCRequest('CUSTOMER_RPC', {
      productId: wishlistMock._id,
      customerId: 'MMZsklO/hmMQR3ue',
    })

    return res.status(200).json(resData)
  } catch (err) {
    console.log(err)
    return res.status(500).json(err)
  }
})

app.get('/', (req, res) => {
  return res.json('product service')
})

app.listen(PORT, () => {
  console.log(`Product service listening on port ${PORT}`)
  console.clear()
})
