import { z } from 'zod'

const BrandEmailSchema = z.string().refine(
  (val) => {
    return val.endsWith('@naver.com')
  },
  { error: 'error oops' },
)
const brandEmailRaw: z.infer<typeof BrandEmailSchema> = 'abc@naver.com'
const brandEmailRaw2: z.infer<typeof BrandEmailSchema> = 'abc@naver2.com'
const brandEmail = BrandEmailSchema.parse(brandEmailRaw)
// const brandEmail2 = BrandEmailSchema.parse(brandEmailRaw2)
console.log(brandEmail)

const PasswordSchema = z.string().superRefine((val, ctx) => {
  if (val.length < 8) {
    ctx.addIssue({
      code: 'too_small', // <--- this is constant enum
      origin: 'string', // mandatory
      // continue: false,
      // input: undefined,
      // inst: undefined,
      // params: {},
      // path: [],
      minimum: 8, // <--- maybe mandatory when code is too_small
      // type: 'string',
      // inclusive: true,
      message: 'min >= 8',
    })
  }
  if (!/[A-Z]/.test(val)) {
    ctx.addIssue({ code: 'custom', message: '대문자를 포함해야 합니다' })
  }
  if (!/[0-9]/.test(val)) {
    ctx.addIssue({ code: 'custom', message: '숫자를 포함해야 합니다' })
  }
  if (!/[!@#$%^&*]/.test(val)) {
    ctx.addIssue({ code: 'custom', message: '특수문자를 포함해야 합니다' })
  }
})

const r = PasswordSchema.safeParse('abc')
console.log(r.error?.issues)
