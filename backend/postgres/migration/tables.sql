-- * To check existing table
-- SELECT * FROM pg_catalog.pg_tables
-- WHERE schemaname != 'pg_catalog'
-- AND schemaname != 'information_schema'

-- * To migrate
CREATE TABLE IF NOT EXISTS accounts (
 username TEXT PRIMARY KEY NOT NULL,
 email TEXT UNIQUE NOT NULL,
 password TEXT NOT NULL,
 image TEXT NOT NULL,
 bio TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 updated_at TIMESTAMP NOT NULL DEFAULT current_timestamp
)

CREATE TABLE IF NOT EXISTS articles (
 -- slug TEXT PRIMARY KEY GENERATED AlWAYS AS (array_to_string(regexp_split_to_array(title, '\s+'), '-')) STORED,
 -- ERROR: generation expression is not immutable (SQLSTATE 42P17)
 slug TEXT PRIMARY KEY NOT NULL,
 title TEXT NOT NULL,
 description TEXT NOT NULL,
 -- tag_list :: [Tag], -- ["dragons", "training"],
 body TEXT NOT NULL,
 -- favorites_count INTEGER NOT NULL, -- 0,
 image TEXT NOT NULL DEFAULT '',
 author TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 updated_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 CONSTRAINT article_author FOREIGN KEY (author) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
)

CREATE TABLE IF NOT EXISTS comments (
 id UUID PRIMARY KEY NOT NULL,
 body TEXT,
 author TEXT NOT NULL,
 article TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 updated_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 CONSTRAINT comment_author FOREIGN KEY (author) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT comment_article FOREIGN KEY (article) REFERENCES articles (slug) ON DELETE CASCADE ON UPDATE CASCADE
)

CREATE TABLE IF NOT EXISTS tags (
 tag TEXT PRIMARY KEY NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp
)

CREATE TABLE IF NOT EXISTS article_has_tag (
 article TEXT NOT NULL,
 tag TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (article, tag),
 CONSTRAINT article_has_tag_article FOREIGN KEY (article) REFERENCES articles (slug) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT article_has_tag_tag FOREIGN KEY (tag) REFERENCES tags (tag) ON DELETE CASCADE ON UPDATE CASCADE
)

CREATE TABLE IF NOT EXISTS user_follow_user (
 following TEXT NOT NULL,
 followed_by TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (following, followed_By),
 CONSTRAINT user_follow_user_following FOREIGN KEY (following) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT user_follow_user_followed_by FOREIGN KEY (followed_by) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
)

CREATE TABLE IF NOT EXISTS user_favorite_article (
 favoriting TEXT NOT NULL,
 favorited_by TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (favoriting, favorited_by),
 CONSTRAINT user_favorite_article_favoriting FOREIGN KEY (favoriting) REFERENCES articles (slug) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT user_favorite_article_favorited_by FOREIGN KEY (favorited_by) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
)
