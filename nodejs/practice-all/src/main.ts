import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import swaggerConfig from './common/config/swagger.config';
import helmet from 'helmet';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  app.use(helmet());

  swaggerConfig(app);

  await app.listen(3033);
}
bootstrap().then(() => {
  console.log({ port: 3033 });
});
