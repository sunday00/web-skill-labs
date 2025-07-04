const STATUS = {
  PENDING: "Request is pending.",
  FAILED: "Request failed.",
  SUCCESS: "Request was successful",
} as const;

type StatusWithResult = (typeof STATUS)["FAILED" | "SUCCESS"];

export {};
