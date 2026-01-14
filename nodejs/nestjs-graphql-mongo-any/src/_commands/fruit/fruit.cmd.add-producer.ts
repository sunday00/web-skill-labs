import { Command, CommandRunner } from 'nest-commander'
import { SeederOptions } from '@/_commands/_common/seeder.options'
import { FruitService } from '@/fruit/fruit.service'
import { Fruit } from '@/fruit/struct/fruit.entity'
import { randFullName } from '@ngneat/falso'

@Command({
  name: 'fruit.add.producer',
  description: 'seeding add producer to fruits',
})
export class FruitCmdAddProducer extends CommandRunner {
  constructor(private readonly service: FruitService) {
    super()
  }

  async run(passedParams: string[], options?: SeederOptions): Promise<void> {
    const i = this.addProducerTo10Fruits()

    let iNext: IteratorResult<string[]> = {
      done: false,
      value: [],
    }

    while (!iNext.done) {
      if (iNext.value.length > 0) console.log(iNext.value)
      iNext = await i.next()
    }
  }

  async *addProducerTo10Fruits() {
    let page = 0
    const size = 20

    while (true) {
      const items: Fruit[] = await this.service.findMany(++page, size)

      if (items.length < 1) return null // <-- iterator to be done.

      items.forEach((item: Fruit) => (item.producer = randFullName()))

      await this.service.updateMany(items)

      yield items.map((item: Fruit) => item.id) // <-- iterator shows nexted value.
    }
  }
}
