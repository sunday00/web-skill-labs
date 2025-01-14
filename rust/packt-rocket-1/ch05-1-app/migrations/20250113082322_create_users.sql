-- Add migration script here
CREATE TABLE IF NOT EXISTS users
(
    uuid   UUID default ( lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-' || substr('89ab',abs(random()) % 4 + 1, 1) || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6))) ) PRIMARY KEY,
    name   VARCHAR NOT NULL,
    age    SMALLINT NOT NULL DEFAULT 0,
    grade  SMALLINT NOT NULL DEFAULT 0,
    active BOOL NOT NULL DEFAULT TRUE
);
CREATE INDEX name_active_idx ON users(name, active);