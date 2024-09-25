import { randFullName, randNumber } from "@ngneat/falso";
import { IService } from ".";
import { RunOption } from "../share/types";
import { DBClient } from "../utils/mysql";

export class User1 implements IService {
  public db?: DBClient;

  constructor() {}

  connect(db: DBClient) {
    this.db = db;
  }

  async createMany(options: RunOption) {
    const sql = `
        INSERT INTO users (name, age) VALUES ?;
    `;
    const values = [];

    const isAgeIn2030 = randNumber({ min: 0, max: 100 }) > 20;

    for (let i = 0; i < (options.size ?? 10); i++) {
      values.push([
        randFullName(),
        isAgeIn2030
          ? randNumber({ min: 20, max: 30 })
          : randNumber({ min: 18, max: 110 }),
      ]);
    }

    await this.db?.conn.query(sql, [values]);
  }
}
