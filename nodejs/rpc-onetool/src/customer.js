const express = require('express')
const PORT = 9001

const app = express()
app.use(express.json())

app.get('/profile', (req, res) => {
  return res.json('customer service')
})

app.get('/', (req, res) => {
  return res.json('customer service')
})

app.listen(PORT, () => {
  console.log(`Customer service listening on port ${PORT}`)
  console.clear()
})
