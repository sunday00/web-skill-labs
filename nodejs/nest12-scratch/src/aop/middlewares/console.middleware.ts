import { Injectable, NestMiddleware } from '@nestjs/common'
import { NextFunction } from 'express'
import { Time } from '../../utils/time.util.js'

@Injectable()
export class ConsoleMiddleware implements NestMiddleware {
  use(req: any, res: any, next: (error?: any) => void) {
    process.stdout.write(`logged from middleware -----------------------\n`)

    return next()
  }
}

export const ConsoleFunctionMiddleware = (
  // not nees to register on module provider.

  req: Request,
  res: any,
  next: NextFunction,
) => {
  process.stdout.write(
    `logged from Functional middleware -----------------------\n`,
  )

  return next()
}

export const UselessFunctionMiddlewareFactory = (name: string) => {
  return (req: Request, res: any, next: NextFunction) => {
    console.log(`logged from useless ${name} function middleware`)

    return next()
  }
}

// global middleware on main.ts app.use()
// class style can't be that way.
// should using app.module.apply() and path/* method/*
export const CheckPerformAll = (
  req: any,
  res: any,
  next: (error?: any) => void,
) => {
  const st = new Time()

  next()

  const et = new Time()
  console.log(` ==== ${et.diff(st.T)} ms`)
}
