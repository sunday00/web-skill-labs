const { SimpleResponse } = require('../calculator/proto/simple_pb')
const { SumResponse } = require('../calculator/proto/sum_pb')
const { PrimeResponse } = require('../calculator/proto/prime_pb')
const { AvgResponse } = require('../calculator/proto/avg_pb')

exports.simple = (call, _) => {
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

exports.prime = (call, _) => {
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
