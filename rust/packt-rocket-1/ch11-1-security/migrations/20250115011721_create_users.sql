-- Add migration script here

CREATE TABLE IF NOT EXISTS users
(
    uuid          VARCHAR     NOT NULL default (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' ||
                                                substr(lower(hex(randomblob(2))), 2) || '-' ||
                                                substr('89ab', abs(random()) % 4 + 1, 1) ||
                                                substr(lower(hex(randomblob(2))), 2) || '-' ||
                                                lower(hex(randomblob(6)))) PRIMARY KEY,
    username      VARCHAR     NOT NULL UNIQUE,
    email         VARCHAR     NOT NULL UNIQUE,
    password_hash VARCHAR     NOT NULL,
    description   TEXT,
    status        INTEGER     NOT NULL DEFAULT 0,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);