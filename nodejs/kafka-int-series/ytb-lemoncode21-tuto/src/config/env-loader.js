import dotenv from 'dotenv'

dotenv.config({ path: './.env' })

export const ENV = {
  kafka: {
    clientId: process.env.KAFKA_CLIENT_ID,
    brokers: [`${process.env.KAFKA_HOST}:${process.env.KAFKA_PORT}`],
    consumerGroup: process.env.KAFKA_CONSUMER_GROUP,
  },
}
