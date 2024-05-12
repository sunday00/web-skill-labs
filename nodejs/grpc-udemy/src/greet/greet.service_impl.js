const pb = require('../greet/proto/greet_pb')

exports.greet = (call, callback) => {
  console.log('greet was invoked')

  const res = new pb.GreetResponse().setResult(
    `Hello ${call.request.getFirstName()}`,
  )

  callback(null, res)
}

exports.greetMany = (call, _) => {
  console.log('greetMany was invoked')

  const res = new pb.GreetResponse()

  let i = 0
  const intv = setInterval(() => {
    res.setResult(`Hello ${call.request.getFirstName()} - no ${i}`)
    call.write(res)
    i++

    if (i > 10) {
      call.end()
      clearInterval(intv)
    }
  }, 1000)
}
