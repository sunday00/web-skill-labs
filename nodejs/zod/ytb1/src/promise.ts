import { z } from 'zod'

const PromiseSchema = z.promise(z.string())
const p = Promise.resolve(1)
PromiseSchema.parseAsync(p)
