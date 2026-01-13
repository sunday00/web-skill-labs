import { Command, CommandRunner } from 'nest-commander';
import { SeederOptions } from '@/_commands/_common/seeder.options';
import { FruitService } from '@/fruit/fruit.service';
import { Fruit } from '@/fruit/struct/fruit.entity';
import { randFullName } from '@ngneat/falso';

@Command({
  name: 'fruit.add.producer',
  description: 'seeding add producer to fruits',
})
export class FruitCmdAddProducer extends CommandRunner {
  constructor(private readonly service: FruitService) {
    super()
  }

  async run(passedParams: string[], options?: SeederOptions): Promise<void> {
    async function* get10Fruits(service: FruitService) {
      let page = 1
      const size = 20

      while (true) {
        const items: Fruit[] = await service.findMany(page, size)

        const m = items.map((item: Fruit) => {
          item.producer = randFullName()

          return item
        })

        await service.updateMany(m)

        page++

        if (items.length < 1) return null

        yield items.map((item: Fruit) => item.id)
      }
    }

    const i = get10Fruits(this.service)
    let iNext = await i.next()
    while (!iNext.done) {
      console.log(iNext.value)
      iNext = await i.next()
    }
  }
}
