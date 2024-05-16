const pb = require('../greet/proto/greet_pb')

exports.greet = (call, callback) => {
  console.log('greet was invoked')

  const res = new pb.GreetResponse().setResult(
    `Hello ${call.request.getFirstName()}`,
  )

  callback(null, res)
}

exports.greetMany = (call) => {
  console.log('greetMany was invoked')

  for (let i = 0; i < 10; i++) {
    const res = new pb.GreetResponse()
    res.setResult(`Hello ${call.request.getFirstName()} - no ${i}`)
    call.write(res)
  }

  call.end()
}

exports.longGreet = (call, callback) => {
  console.log('longGreet was invoked')

  let greet = ''

  call.on('data', (req) => {
    greet += `Hello ${req.getFirstName()}\n`
  })

  call.on('end', () => {
    const res = new pb.GreetResponse().setResult(greet)
    callback(null, res)
  })
}

exports.greetEveryone = (call) => {
  console.log('greetEveryone is invoked')

  call.on('data', (req) => {
    console.log(`received ${req}`)

    const res = new pb.GreetResponse().setResult(`Hello ${req.getFirstName()}`)

    console.log(`sending response ${res}`)
    call.write(res)
  })

  call.on('end', () => {
    call.end()
  })
}
