import { Equal, Expect } from "..";

type RequestStatus = "loading" | "success" | "error";

type ConvertToStates<S> = S extends unknown
  ? /*                 👆 Distribute `S`

                   Compute properties 👇              */
    {
      status: S;
      isLoading: S extends "loading" ? true : false;
      data: S extends "success" ? string : undefined;
      error: S extends "error" ? Error : null;
    }
  : never;

// Test helpers, should not be used in the solution!

type LoadingState = {
  status: "loading";
  isLoading: true;
  data: undefined;
  error: null;
};

type SuccessState = {
  status: "success";
  isLoading: false;
  data: string;
  error: null;
};

type FailureState = {
  status: "error";
  isLoading: false;
  data: undefined;
  error: Error;
};

type result1 = ConvertToStates<"error">;
type test1 = Expect<Equal<result1, FailureState>>;

type result2 = ConvertToStates<"loading" | "success">;
type test2 = Expect<Equal<result2, LoadingState | SuccessState>>;

type result3 = ConvertToStates<"success" | "error">;
type test3 = Expect<Equal<result3, SuccessState | FailureState>>;

type result4 = ConvertToStates<"loading" | "success" | "error">;
type test4 = Expect<Equal<result4, LoadingState | SuccessState | FailureState>>;
