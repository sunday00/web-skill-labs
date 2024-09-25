export type RunOption = {
  service?: string;
  size?: number;
  task?: string;
  [k: string]: unknown;
};

export type RunMethod = {
  //@ts-ignore
  [k: any]: (options: RunOption) => void;
};
