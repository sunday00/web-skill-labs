import express from 'express'
import bodyParser from 'body-parser'
import dotenv from 'dotenv'
import controller from './src/controller.js'
import KafkaConfig from './src/config/kafka.config.js'

dotenv.config({ path: './.env' })
const port = Number(process.env.APP_PORT || '8080')

const main = async () => {
  const app = express()
  const bp = bodyParser.json()
  app.use(bp)

  app.post('/api/send', controller.sendMessage)

  const kfkc = new KafkaConfig()
  await kfkc.consume('my-topic', (value) => {
    console.log(value)
  })

  app.listen(port, () => {})
}

main().then(() => {
  console.log(`server is running at http://localhost:${port}`)
})
