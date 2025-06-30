/**
 * Define the AuthHeaders type to ensure the `Token` property
 * starts with 'Bearer ' followed by a valid JWT token.
 *
 * Note: JWT tokens are composed of 3 parts, separated by dots.
 *
 * 💡 A conditional type should not be required.
 */

export type AuthHeaders = { Token: `Bearer ${string}.${string}.${string}` }

const example1: AuthHeaders = {
  // ✅ This is a valid token header:
  Token:
    'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtIjoiWW91J3JlIGEgbmVyZCA7KSJ9.gfB7ECp1ePeIB4Mh_3Ypci4y7jFjMH9w_BB4rZcMvQM',
}
//
// const example2: AuthHeaders = {
//   //❌ Token should start with 'Bearer'
//   Token: 'abc.def.ghi',
// }
//
// const example3: AuthHeaders = {
//   //❌ Token should start with 'Bearer'
//   Token: 'hello.world.token',
// }
//
// const example4: AuthHeaders = {
//   //❌ Token is invalid, only 1 part.
//   Token: 'Bearer kdjfl',
// }
//
// const example5: AuthHeaders = {
//   //❌ Token is invalid, only 2 parts.
//   Token: 'Bearer ksdjhf.123',
// }
