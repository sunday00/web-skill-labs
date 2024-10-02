DROP table IF EXISTS users;

CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100),
    accessCode varchar(100) UNIQUE ,
    age INT
)

SHOW INDEX FROM users;