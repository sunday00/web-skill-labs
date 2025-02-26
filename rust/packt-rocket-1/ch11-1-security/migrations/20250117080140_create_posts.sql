-- Add migration script here
CREATE TABLE IF NOT EXISTS posts
(
    uuid       VARCHAR     NOT NULL default (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' ||
                                             substr(lower(hex(randomblob(2))), 2) || '-' ||
                                             substr('89ab', abs(random()) % 4 + 1, 1) ||
                                             substr(lower(hex(randomblob(2))), 2) || '-' ||
                                             lower(hex(randomblob(6)))) PRIMARY KEY,
    user_uuid  VARCHAR     NOT NULL,
    post_type  INTEGER     NOT NULL DEFAULT 0,
    content    TEXT        NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (user_uuid) REFERENCES "users" (uuid)
);