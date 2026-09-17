import {
  CallHandler,
  ExecutionContext,
  Injectable,
  NestInterceptor,
} from '@nestjs/common'
import { map, Observable } from 'rxjs'
import { Response } from 'express'

@Injectable()
export class ContentLengthInterceptor implements NestInterceptor {
  intercept(ctx: ExecutionContext, next: CallHandler): Observable<any> {
    const res = ctx.switchToHttp().getResponse<Response>()

    return next.handle().pipe(
      map((body) => {
        const raw = Buffer.byteLength(
          typeof body === 'string' ? body : JSON.stringify(body ?? ''),
          'utf8',
        )
        res.setHeader('X-Content-Length-Raw', raw.toString())
        return body
      }),
    )
  }
}
