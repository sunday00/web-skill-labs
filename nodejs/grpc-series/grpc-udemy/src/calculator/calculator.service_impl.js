const { SimpleResponse } = require('../calculator/proto/simple_pb')
const { SumResponse } = require('../calculator/proto/sum_pb')
const { PrimeResponse } = require('../calculator/proto/prime_pb')
const { AvgResponse } = require('../calculator/proto/avg_pb')
const { MaxResponse } = require('../calculator/proto/max_pb')
const { SqrtResponse } = require('../calculator/proto/sqrt_pb')
const grpc = require('@grpc/grpc-js')

exports.simple = (call) => {
  let res = new SimpleResponse()
  res.setResult(1)
  call.write(res)

  res = new SimpleResponse()
  res.setResult(2)
  call.write(res)

  res = new SimpleResponse()
  res.setResult(3)
  call.write(res)

  res = new SimpleResponse()
  res.setResult(4)
  call.write(res)

  res = new SimpleResponse()
  res.setResult(5)
  call.write(res)

  call.end()
}

exports.sum = (call, callback) => {
  console.log('sum was invoked')

  const res = new SumResponse().setResult(
    call.request.getFirstNumber() + call.request.getSecondNumber(),
  )

  callback(null, res)
}

exports.prime = (call) => {
  console.log('prime was invoked')

  let number = call.request.getNumber()
  let divisor = 2
  const res = new PrimeResponse()

  while (number > 1) {
    if (number % divisor === 0) {
      res.setResult(divisor)

      call.write(res)

      number /= divisor
    } else {
      ++divisor
    }
  }

  call.end()
}

exports.avg = (call, callback) => {
  console.log('avg was invoked')

  let count = 0.0
  let total = 0.0

  call.on('data', (req) => {
    total += req.getNumber()
    ++count
  })

  call.on('end', () => {
    const res = new AvgResponse().setResult(total / count)

    callback(null, res)
  })
}

exports.max = (call) => {
  console.log('max was invoked')

  let max = 0

  call.on('data', (req) => {
    max = Math.max(max, req.getNumber())
    const res = new MaxResponse().setResult(max)
    call.write(res)
  })

  call.on('end', () => {
    call.end()
  })
}

exports.sqrt = (call, callback) => {
  console.log('sqrt was invoked')

  const number = call.request.getNumber()

  if (number < 0) {
    callback({
      code: grpc.status.INVALID_ARGUMENT,
      message: `Bad request number: ${number}. Try bigger than 0.`,
    })
  }

  const res = new SqrtResponse().setResult(Math.sqrt(number))
  callback(null, res)
}
