import { Equal, Expect } from "..";

declare function goTo<Path extends string>(
  path: Path,
  params: ExtractParams<Path>
): void;

type ExtractParams<Url> =
  // 1. Split the string using `/`:
  Url extends `${infer Start}/${infer Rest}`
    ? // 2. Recurse on the two sides of the slash,
      //    and merge the objects with &:
      ExtractParams<Start> & ExtractParams<Rest>
    : // 3. If the url is a param, create a
    //    `{ [Param]: string }` object:
    Url extends `:${infer Param}`
    ? { [K in Param]: string }
    : // 4. Otherwise, return an empty object:
      {};

// Test cases
type testCase1 = ExtractParams<"user/:userId">;
type test1 = Expect<Equal<testCase1, { userId: string }>>;

type testCase2 = ExtractParams<"user/:userId/dashboard">;
type test2 = Expect<Equal<testCase2, { userId: string }>>;

type testCase3 = ExtractParams<"user/:userId/dashboard/:dashboardId">;
type test3 = Expect<
  Equal<testCase3, { userId: string } & { dashboardId: string }>
>;

// Function usage examples
goTo("user/:userId", { userId: "2" }); // ✅

goTo("user/:userId/dashboard", { userId: "2" }); // ✅

// ❌ Missing `userId`
goTo("user/:userId/dashboard/:dashboardId", { dashboardId: "2" });

// ❌ Extra parameter `oops`
goTo("user/:userId/dashboard/:dashboardId", {
  userId: "2",
  dashboardId: "2",
  oops: ":(",
});
