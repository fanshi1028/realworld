CREATE TABLE IF NOT EXISTS article_has_tag (
 article TEXT NOT NULL,
 tag TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (article, tag),
 CONSTRAINT article_has_tag_article FOREIGN KEY (article) REFERENCES articles (slug) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT article_has_tag_tag FOREIGN KEY (tag) REFERENCES tags (tag) ON DELETE CASCADE ON UPDATE CASCADE
);
