import { z } from 'zod'
import { fromZodError } from 'zod-validation-error'

const UserSchema = z.object({
  username: z.string().min(3, 'min len 3'),
})

const user = {
  username: '1',
}

const r = UserSchema.safeParse(user)

if (!r.success) {
  console.log('\n=========')
  console.log(fromZodError(r.error).message)
  console.log('=========')
}
