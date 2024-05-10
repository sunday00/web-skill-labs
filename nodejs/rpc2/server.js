const express = require('express');
const morgan = require('morgan')
const actions = require('./action')

const app = express()

app.use(morgan('dev'));
app.use(express.json());
app.use(express.static('public'));

app.get('/', (req, res) => {
  res.send('index');
})

app.post('/rpc', (req, res) => {
  const result = actions[req.body.type](req.body.payload)

  res.send(result)
})

app.listen(4444, () => console.log('Listening on port 4444'));