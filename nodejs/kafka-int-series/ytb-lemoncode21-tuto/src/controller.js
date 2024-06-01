import KafkaConfig from './config/kafka.config.js'

const sendMessage = async (req, res) => {
  try {
    const { message } = req.body
    const kafkaConfig = new KafkaConfig()

    const messages = [{ key: 'key1', value: message }]

    await kafkaConfig.produce('my-topic', messages)

    res.status(200).json({
      status: 'ok',
      message: 'msg sent',
    })
  } catch (err) {
    console.error(err)
  }
}

export default { sendMessage }
