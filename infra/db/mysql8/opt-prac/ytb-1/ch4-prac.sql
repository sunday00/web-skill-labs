DROP TABLE IF EXISTS users;

CREATE TABLE users (
   id INT AUTO_INCREMENT PRIMARY KEY,
   name VARCHAR(100),
   accessCode varchar(100),
   department varchar(50),
   age INT,
   createdAt DATETIME
);

SELECT * FROM users LIMIT 10000;

SELECT * FROM users
WHERE createdAt >= DATE_SUB(NOW(), INTERVAL 3 DAY)

CREATE INDEX idx_created_at ON users (createdAt)