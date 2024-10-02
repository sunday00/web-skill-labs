DROP TABLE IF EXISTS users;

CREATE TABLE users (
   id INT AUTO_INCREMENT PRIMARY KEY,
   name VARCHAR(100),
   accessCode varchar(100),
   age INT
);

CREATE INDEX idx_age ON users(age)

EXPLAIN SELECT * FROM users WHERE age BETWEEN 10 and 20;


CREATE INDEX idx_access_code ON users(accessCode)

EXPLAIN SELECT * FROM users WHERE accessCode = '_-gazbp6cLlImEKr6b8KW';
