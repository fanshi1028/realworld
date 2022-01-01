CREATE TABLE IF NOT EXISTS accounts (
 username TEXT PRIMARY KEY NOT NULL,
 email TEXT UNIQUE NOT NULL,
 password TEXT NOT NULL,
 image TEXT NOT NULL DEFAULT '',
 bio TEXT NOT NULL DEFAULT '',
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 updated_at TIMESTAMP NOT NULL DEFAULT current_timestamp
);
