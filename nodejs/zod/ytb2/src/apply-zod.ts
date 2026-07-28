import { z } from 'zod'

// prettier-ignore
export const userDataSchema = z.object({
  id: z.coerce // <--------------------------------------coerce--------------------------------------------------+ // coerce : numeric to number
    .number() //                                                                                                 |
    .min(2) //                                                                                             |
    .max(100, 'do not exceed 100'), //                                                             |
  name: z.string().min(2, 'too short name').max(100, 'too long name'), //     |
  email: z.email(), //                                                                                           |
  age: z.coerce.number().positive().max(150), // <---------------------------------------------------------+
  preferences: z.object({ //                                                                                     |
    theme: z.enum(['light', 'dark'], { //                                                                        |
      error: () => ({ message: 'please select an item' }), //                                                    |
    }), //                                                                                                       |
    // .default('dark'), //                                                                                      |
    notifications: z.boolean().default(false), //                                                           |
  }), //                                                                                                         |
  // .optional(), //                                                                                             |
  //                                                                                                             |
  // hobbies: z //                                                                                               |
  //   .array(z.string().min(2, 'what is this hobby? should 2 or more letters please.')) //                      |
  //   .min(1) //                                                                                                |
  //   .max(5), //                                                                                               |
}) //                                                                                                            |
//                                                                                                               |
type UserData = z.infer<typeof userDataSchema> //                                                                |
//                                                                                                               |
// prettier-ignore
export function processUserData(userData: UserData): string { //                                                 |
  const result = userDataSchema.safeParse(userData) //                                                           |
  //                                                                                                             |
  if (!result.success) console.warn(result.error) //                                                             |
  const userSummary = `
    User: ${userData.name} (ID: ${userData.id})
    Email: ${userData.email}
    Age: ${userData.age}
    Theme: ${userData.preferences?.theme || 'default'}
    Notifications: ${userData.preferences?.notifications ? 'Enabled' : 'Disabled'} 
  ` //
  //                                                                                                             |
  return userSummary.trim() //                                                                                   |
} //                                                                                                             |
//                                                                                                               |
const userDataFromApi: UserData = {
  id: '7', // <---- this is not run time error occur ------------------------------------------------------------+
  name: 'Jane doe', //                                                                                           |
  email: 'invalid-email', //                                                                                     |
  age: '10', // <---- this is not run time error occur ----------------------------------------------------------+
}

const r = processUserData(userDataFromApi)

console.log(r)
