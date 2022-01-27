CREATE TABLE IF NOT EXISTS user_favorite_article (
 favoriting TEXT NOT NULL,
 favorited_by TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (favoriting, favorited_by),
 CONSTRAINT user_favorite_article_favoriting FOREIGN KEY (favoriting) REFERENCES articles (slug) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT user_favorite_article_favorited_by FOREIGN KEY (favorited_by) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
);
