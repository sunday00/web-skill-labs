import { INestApplication } from '@nestjs/common';
import {
  DocumentBuilder,
  SwaggerCustomOptions,
  SwaggerModule,
} from '@nestjs/swagger';

export default (app: INestApplication) => {
  const config = new DocumentBuilder()
    .setTitle('test')
    .addServer(`http://localhost:3033`)
    .build();

  const customOptions: SwaggerCustomOptions = {
    swaggerOptions: {
      persistAuthorization: true,
    },
  };
  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup('docs', app, document, customOptions);
};
