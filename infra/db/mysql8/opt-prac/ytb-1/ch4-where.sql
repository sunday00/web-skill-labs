DROP TABLE IF EXISTS users;

CREATE TABLE users (
   id INT AUTO_INCREMENT PRIMARY KEY,
   name VARCHAR(100),
   accessCode varchar(100),
   department varchar(50),
   age INT,
   createdAt DATETIME
);

SELECT * FROM users WHERE department = 'sports'
AND createdAt >= DATE_SUB(NOW(), INTERVAL 3 DAY );

EXPLAIN ANALYZE SELECT * FROM users WHERE department = 'sports'
AND createdAt >= DATE_SUB(NOW(), INTERVAL 3 DAY );

CREATE INDEX idx_created_at ON users (createdAt);

ALTER TABLE users DROP INDEX idx_created_at;

CREATE INDEX idx_department ON users (department);

ALTER TABLE users DROP INDEX idx_department;

CREATE INDEX idx_created_at_department ON users (createdAt,department);

SHOW INDEX FROM users;