import { db } from '../../../common/db/mysql.client'
import { UserType } from '../type/user.type'

export class User {
  public data: UserType[] = []

  public async index(page: string) {
    const sql = `
        SELECT * FROM users 
        ORDER BY id DESC
        LIMIT ?, 10;
    `

    const [rows, _fields] = (await db.query(
      sql,
      (Number(page) - 1) * 10,
    )) as unknown as [UserType[], unknown[]]
    this.data = rows
  }

  public async store(user: Omit<UserType, 'id'>) {
    const sql = `
        INSERT INTO users (name, age) VALUES (
            ?, ?
        )
    `

    const [result, _fields] = await db.execute({
      sql,
      values: [user.name, user.age],
    })

    //@ts-ignore
    return result.affectedRows
  }
}
