const pb1 = require('../greet/proto/greet_pb')
const pb2 = require('../calculator/proto/sum_pb')

exports.greet = (call, callback) => {
  console.log('greet was invoked')

  const res = new pb1.GreetResponse()
    .setResult(`Hello ${call.request.getFirstName()}`)

  callback(null, res)
}

exports.sum = (call, callback) => {
  console.log('sum was invoked')

  const res = new pb2.SumResponse()
    .setResult(call.request.getFirstNumber() + call.request.getSecondNumber())

  callback(null, res)
}