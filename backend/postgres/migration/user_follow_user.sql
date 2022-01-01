CREATE TABLE IF NOT EXISTS user_follow_user (
 following TEXT NOT NULL,
 followed_by TEXT NOT NULL,
 created_at TIMESTAMP NOT NULL DEFAULT current_timestamp,
 PRIMARY KEY (following, followed_By),
 CONSTRAINT user_follow_user_following FOREIGN KEY (following) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT user_follow_user_followed_by FOREIGN KEY (followed_by) REFERENCES accounts (username) ON DELETE CASCADE ON UPDATE CASCADE
);
