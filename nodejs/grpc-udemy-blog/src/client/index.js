const fs = require('fs')
const grpc = require('@grpc/grpc-js')
const { BlogServiceClient } = require('../blog/proto/blog_grpc_pb')
const { BlogRequest } = require('../blog/proto/blog_pb')

// const doGreet = (client) => {
//   console.log('doGreet was invoked')
//
//   const req = new GreetRequest().setFirstName('Clement')
//
//   client.greet(req, (err, res) => {
//     if (err) {
//       return console.error(err)
//     }
//
//     console.log(`Greet: ${res.getResult()}`)
//   })
// }
//
// const doGreetMany = (client) => {
//   console.log('doGreetMany was invoked')
//
//   const req = new GreetRequest().setFirstName('Clement')
//
//   const call = client.greetMany(req)
//
//   call.on('data', (res) => {
//     console.log(`GreetMany: ${res.getResult()}`)
//   })
// }
//
// const doLongGreet = (client) => {
//   console.log('doLongGreet was invoked')
//
//   const names = ['Clement', 'Maria', 'Tom']
//
//   const call = client.longGreet((err, res) => {
//     if (err) return console.error(err)
//
//     console.log(`LongGreet: ${res.getResult()}`)
//   })
//
//   names
//     .map((name) => {
//       return new GreetRequest().setFirstName(name)
//     })
//     .forEach((req) => {
//       call.write(req)
//     })
//   call.end()
// }
//
// const doGreetEveryone = (client) => {
//   console.log('doGreetEveryone was invoked')
//
//   const names = ['Clement', 'Maria', 'Tom']
//
//   const call = client.greetEveryone()
//
//   call.on('data', (res, err) => {
//     if (err) return console.error(err)
//
//     console.log(`GreetEveryone: ${res.getResult()}`)
//   })
//
//   names
//     .map((name) => {
//       return new GreetRequest().setFirstName(name)
//     })
//     .forEach((req) => {
//       call.write(req)
//     })
//   call.end()
// }
//
// const doGreetWithDeadLine = (client, ms) => {
//   console.log('doGreetWithDeadLine was invoked')
//
//   const req = new GreetRequest().setFirstName('Clement')
//
//   client.greetWithDeadline(
//     req,
//     { deadline: new Date(Date.now() + ms) },
//     (err, res) => {
//       if (err) return console.error(err)
//
//       console.log(`GreetWithDeadline: ${res.getResult()}`)
//     },
//   )
// }

const main = () => {
  let creds = grpc.ChannelCredentials.createInsecure()

  const client = new BlogServiceClient('127.0.0.1:50051', creds)

  // doGreet(client)


  client.close()
}

main()
