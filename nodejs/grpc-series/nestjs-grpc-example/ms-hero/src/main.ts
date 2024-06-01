import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { MicroserviceOptions, Transport } from '@nestjs/microservices';
import * as path from 'path';

async function bootstrap() {
  const app = await NestFactory.createMicroservice<MicroserviceOptions>(
    AppModule,
    {
      transport: Transport.GRPC,
      options: {
        package: 'hero',
        protoPath: path.join(__dirname, 'hero/hero.proto'),
        url: '0.0.0.0:5003',
      },
    },
  );

  await app.listen();
}

bootstrap().then(() => {});
