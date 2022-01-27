CREATE TABLE IF NOT EXISTS articles (
 -- slug TEXT PRIMARY KEY GENERATED AlWAYS AS (array_to_string(regexp_split_to_array(title, '\s+'), '-')) STORED,
 -- ERROR: generation expression is not immutable (SQLSTATE 42P17)
 slug TEXT PRIMARY KEY NOT NULL,
 title TEXT NOT NULL,
 description TEXT NOT NULL DEFAULT '',
 -- tag_list :: [Tag], -- ["dragons", "training"],
 body TEXT NOT NULL DEFAULT '',
 -- favorites_count INTEGER NOT NULL, -- 0,
 image TEXT NOT NULL DEFAULT '',
 author TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 updated_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 CONSTRAINT article_author FOREIGN KEY (author) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
);
