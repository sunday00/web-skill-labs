import { Kafka } from 'kafkajs'
import { ENV } from './env-loader.js'

export default class KafkaConfig {
  constructor() {
    this.kafka = new Kafka({
      clientId: ENV.kafka.clientId,
      brokers: ENV.kafka.brokers,
    })

    this.producer = this.kafka.producer()
    this.consumer = this.kafka.consumer({ groupId: ENV.kafka.consumerGroup })
  }

  async produce(topic, message) {
    try {
      await this.producer.connect()
      await this.producer.send({
        topic,
        message,
      })
    } catch (e) {
      console.error(e)
    } finally {
      await this.producer.disconnect()
    }
  }

  async consume(topic, callback) {
    try {
      await this.consumer.connect()
      await this.consumer.subscribe({
        topic,
        fromBeginning: true,
      })
      await this.consumer.run({
        eachMessage: async ({ topic, partition, message }) => {
          callback(message.value.toString())
        },
      })
    } catch (e) {
      console.error(e)
    } finally {
      await this.consumer.disconnect()
    }
  }
}
