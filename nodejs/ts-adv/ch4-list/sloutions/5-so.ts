/**
 * A utility type that enforces a non-empty array.
 */
type NonEmptyList<T> = [T, ...T[]];

/**
 * Function to simulate sending email to a list of email addresses.
 * @param addresses - A non-empty array of strings representing addresses.
 */
function sendEmail(addresses: NonEmptyList<string>) {
  // Simulate sending email
  console.log("Sending email to:", addresses.join(", "));
}

// Valid examples
sendEmail(["info@codelicks.info"]); // ✅
sendEmail(["blabla@codelicks.info", "hello@codelicks.info"]); // ✅

// Invalid example
// @ts-expect-error
sendEmail([]);
//       ^ ❌ `[]` isn't assignable to `NonEmptyList<string>`
