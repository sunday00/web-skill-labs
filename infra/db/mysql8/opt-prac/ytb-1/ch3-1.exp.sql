DROP TABLE IF EXISTS users;

CREATE TABLE users (
   id INT AUTO_INCREMENT PRIMARY KEY,
   name VARCHAR(100),
   accessCode varchar(100),
   age INT
);

SELECT * FROM users
WHERE age = 23;

EXPLAIN SELECT * FROM users
WHERE age = 23;

EXPLAIN ANALYZE SELECT * FROM users
    WHERE age = 23;

-- -> Filter: (users.age = 23)  (cost=1.25 rows=1) (actual time=0.0375..0.0448 rows=2 loops=1)
   -- -> Table scan on users  (cost=1.25 rows=10) (actual time=0.0358..0.042 rows=10 loops=1)

CREATE INDEX idx_name ON users (name);

EXPLAIN SELECT * FROM users
ORDER BY name LIMIT 10;

EXPLAIN ANALYZE SELECT * FROM users
        ORDER BY name LIMIT 10;

CREATE TABLE users (
   id INT AUTO_INCREMENT PRIMARY KEY,
   name VARCHAR(100),
   accessCode varchar(100) UNIQUE ,
   age INT
);

EXPLAIN SELECT * FROM users where id = 3;
EXPLAIN SELECT * FROM users where name = 'Heike Han';
EXPLAIN SELECT * FROM users where accessCode = 'IJTSQ08TZjApAEqu9WfQR';

