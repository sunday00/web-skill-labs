const grpc = require('@grpc/grpc-js')
const pb = require('../blog/proto/blog_pb')
const { sleep } = require('../utils/time')

const blogToDocument = (blog) => {
  return {
    author_id: blog.getAuthorId(),
    title: blog.getTitle(),
    content: blog.getContent(),
  }
}

const internal = (err, callback) => {
  return callback({
    code: grpc.status.INTERNAL,
    message: err.toString(),
  })
}

const checkNotAcknowledged = (res, callback) => {
  if (!res.acknowledged) {
    callback({
      code: grpc.status.INTERNAL,
      message: `Operation wasn't acknowledged`,
    })
  }
}

exports.createBlog = async (call, callback) => {
  const data = blogToDocument(call.request)

  await collection
    .insertOne(data)
    .then((res) => {
      checkNotAcknowledged(res, callback)

      const id = res.insertId.toString()
      const blogId = new pb.BlogId().setId(id)

      callback(null, blogId)
    })
    .catch((err) => internal(err, callback))
}

// exports.greetMany = (call) => {
//   console.log('greetMany was invoked')
//
//   for (let i = 0; i < 10; i++) {
//     const res = new pb.GreetResponse()
//     res.setResult(`Hello ${call.request.getFirstName()} - no ${i}`)
//     call.write(res)
//   }
//
//   call.end()
// }
//
// exports.longGreet = (call, callback) => {
//   console.log('longGreet was invoked')
//
//   let greet = ''
//
//   call.on('data', (req) => {
//     greet += `Hello ${req.getFirstName()}\n`
//   })
//
//   call.on('end', () => {
//     const res = new pb.GreetResponse().setResult(greet)
//     callback(null, res)
//   })
// }
//
// exports.greetEveryone = (call) => {
//   console.log('greetEveryone is invoked')
//
//   call.on('data', (req) => {
//     console.log(`received ${req}`)
//
//     const res = new pb.GreetResponse().setResult(`Hello ${req.getFirstName()}`)
//
//     console.log(`sending response ${res}`)
//     call.write(res)
//   })
//
//   call.on('end', () => {
//     call.end()
//   })
// }
//
// exports.greetWithDeadline = async (call, callback) => {
//   console.log('greetWithDeadLine was invoked')
//
//   for (let i = 0; i < 3; i++) {
//     if (call.cancelled) {
//       return console.log('The client cancelled the request')
//     }
//
//     await sleep(3000)
//   }
//
//   const res = new pb.GreetResponse().setResult(
//     `Hello ${call.request.getFirstName()}`,
//   )
//   callback(null, res)
// }
