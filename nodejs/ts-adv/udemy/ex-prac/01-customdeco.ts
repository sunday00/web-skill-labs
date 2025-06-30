function logger(arg: string) {
  return function (
    target: any,
    propertyKey: string,
    descriptor: PropertyDescriptor,
  ): void {
    // console.log({ arg, target, propertyKey, descriptor })
    // target.sayHello(arg)
    const fn = descriptor.value

    descriptor.value = function (...args: any[]) {
      fn.apply(this, args)
    }
  }
}

class Sample {
  @logger('hoho')
  sayHello(kk: string): string {
    console.log('hello', kk)

    return 'hello'
  }
}

const sample = new Sample()
sample.sayHello('sun')
