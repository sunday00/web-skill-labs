const express = require('express')
const PORT = 9002

const app = express()
app.use(express.json())

app.get('/products', (req, res) => {
  return res.json('product service')
})

app.get('/', (req, res) => {
  return res.json('product service')
})

app.listen(PORT, () => {
  console.log(`Product service listening on port ${PORT}`)
  console.clear()
})
