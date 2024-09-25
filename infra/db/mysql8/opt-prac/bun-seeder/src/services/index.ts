import { RunOption, RunMethod } from "../share/types";
import { DBClient } from "../utils/mysql";
import { User1 } from "./user1";

export interface IServiceConstructor {
  connect(db: DBClient): void;
}

export type IService = IServiceConstructor & RunMethod;

export const services: {
  [k: string]: { new (): IService };
} = {
  user1: User1,
};
