import mysql from "mysql2/promise";

export interface DBClient {
  conn: any;
  connect(): Promise<DBClient> | DBClient;
}

export class MysqlClient implements DBClient {
  public conn: mysql.Connection | undefined = undefined;

  async test() {
    console.log("test import");
  }

  async connect(): Promise<DBClient> {
    this.conn = await mysql.createConnection({
      host: "localhost",
      port: 3310,
      user: "gray",
      password: "p+$$word",
      database: "optprac",
    });

    return this;
  }
}
