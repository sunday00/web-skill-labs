import {
  ArgumentMetadata,
  Injectable,
  PipeTransform,
  StandardSchemaValidationPipe,
} from '@nestjs/common'
import { plainToInstance } from 'class-transformer'

const NATIVE_TYPES: any[] = [String, Boolean, Number, Array, Object, Buffer]

@Injectable()
export class ValidationPipe
  extends StandardSchemaValidationPipe
  implements PipeTransform
{
  async transform(value: any, metadata: ArgumentMetadata) {
    const r = await super.transform(value, metadata)

    // console.log(metadata.schema)

    // 1) @Req(), @Res(), @SseSignal() 같은 커스텀 데코레이터는 변환 안 함
    if (metadata.type === 'custom') return r

    // 2) metatype 없음 or 원시/네이티브면 변환 안 함
    if (!metadata.metatype || NATIVE_TYPES.includes(metadata.metatype)) {
      return r
    }

    if (metadata.metatype) {
      return plainToInstance(metadata.metatype, r)
    }

    return r
  }
}
