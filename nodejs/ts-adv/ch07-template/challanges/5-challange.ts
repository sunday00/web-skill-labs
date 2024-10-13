import { Equal, Expect } from '..'

declare function goTo<Path extends string>(path: Path, params: ExtractParams<Path>): void

type ExtractParams<Url> = Url extends `${infer S}/${infer R}`
  ? ExtractParams<S> & ExtractParams<R>
  : Url extends `:${infer P}`
    ? { [K in P]: string }
    : {}

// Test cases
type testCase1 = ExtractParams<'user/:userId'>
type test1 = Expect<Equal<testCase1, { userId: string }>>

type testCase2 = ExtractParams<'user/:userId/dashboard'>
type test2 = Expect<Equal<testCase2, { userId: string }>>

type testCase3 = ExtractParams<'user/:userId/dashboard/:dashboardId'>
type test3 = Expect<Equal<testCase3, { userId: string } & { dashboardId: string }>>

// Function usage examples
goTo('user/:userId', { userId: '2' }) // ✅

goTo('user/:userId/dashboard', { userId: '2' }) // ✅
//
// // ❌ Missing `userId`
// goTo('user/:userId/dashboard/:dashboardId', { dashboardId: '2' })
//
// // ❌ Extra parameter `oops`
// goTo('user/:userId/dashboard/:dashboardId', {
//   userId: '2',
//   dashboardId: '2',
//   oops: ':(',
// })
