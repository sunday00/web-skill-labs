import {
  ArgumentMetadata,
  Injectable,
  PipeTransform,
  StandardSchemaValidationPipe,
} from '@nestjs/common'
import { plainToInstance } from 'class-transformer'

@Injectable()
export class ValidationPipe
  extends StandardSchemaValidationPipe
  implements PipeTransform
{
  async transform(value: any, metadata: ArgumentMetadata) {
    const r = await super.transform(value, metadata)

    if (metadata.metatype) {
      return plainToInstance(metadata.metatype, r)
    }

    return r
  }
}
