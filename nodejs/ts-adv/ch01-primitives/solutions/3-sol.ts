export let throttledFunction: Function & { stop: Function };
// OR
// let throttledFunction: (() => void) & { stop: () => void };

throttledFunction = Object.assign(() => {}, { stop: () => {} });

// ✅
throttledFunction();

// ✅
throttledFunction.stop();

// ❌ `nonExistentMethod` does not exist on `throttledFunction`.
// @ts-expect-error
throttledFunction.nonExistentMethod();

// ❌ can't assign a string to `throttledFunction`.
// @ts-expect-error: ❌
throttledFunction = "Hello";
