const grpc = require('@grpc/grpc-js')
const blogServiceImpl = require('../blog/blog.service_impl')
const { BlogService } = require('../blog/proto/blog_grpc_pb')
const {MongoClient} = require('mongodb')
const {DB_NAME} = require('../utils/constants')


const addr = '0.0.0.0:50051'
const mongoClient = new MongoClient('mongodb://root:example@localhost:27017')

global.collection = undefined

const cleanup = async (server) => {
  console.log('clean up')

  if (server) {
    await mongoClient.close()
    server.forceShutdown()
  }
}

const main = async () => {
  const server = new grpc.Server()
  let creds = grpc.ServerCredentials.createInsecure()

  process.on('SIGINT', () => {
    console.log('Caught interrupt signal')
    cleanup(server)
  })

  await mongoClient.connect()
  const database = mongoClient.db(DB_NAME)
  collection = database.collection('blog')

  server.addService(BlogService, blogServiceImpl)

  server.bindAsync(addr, creds, (err, _) => {
    if (err) {
      return cleanup(server)
    }
  })

  console.log(`Listening on ${addr}`)
}

main()
  .catch(cleanup)
