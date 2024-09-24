SET SESSION cte_max_recursion_depth=1000000;

INSERT INTO users(name, age)
WITH RECURSIVE cte(n) AS
    (
        SELECT 1
        UNION ALL
        SELECT n+1 FROM cte WHERE n < 1000000
    )
SELECT
    CONCAT('User', LPAD(n, 7, '0')),
    FLOOR(1+RAND()  * 1000) AS age
FROM cte;

-- practice

SELECT * FROM users
WHERE age = 23;

CREATE INDEX idx_age ON users (age);
SHOW INDEX FROM users;
