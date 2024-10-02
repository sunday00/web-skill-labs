import mysql from 'mysql2/promise'

const db = await mysql.createConnection({
  host: 'localhost',
  user: 'gray',
  database: 'optprac',
  password: 'p+$$word',
  port: 3310,
})

export { db }
