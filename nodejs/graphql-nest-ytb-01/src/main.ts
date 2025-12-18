import { NestFactory } from '@nestjs/core'
import { AppModule } from './app.module'

async function bootstrap() {
  const configs = {
    port: 3000,
  }

  const app = await NestFactory.create(AppModule)
  await app.listen(configs.port)

  return configs
}
bootstrap()
  .then((con) => {
    console.log(con.port)
  })
  .catch((err) => {
    console.error(err)
  })
