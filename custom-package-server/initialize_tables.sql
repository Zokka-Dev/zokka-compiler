CREATE TABLE packages (
    id INTEGER PRIMARY KEY NOT NULL,
    project TEXT NOT NULL,
    author TEXT NOT NULL,
    version TEXT NOT NULL,
    hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    elm_json BLOB NOT NULL,
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_packages_project ON packages(project);
CREATE INDEX idx_packages_author ON packages(author);
CREATE INDEX idx_packages_repository_id ON packages(repository_id);

CREATE TABLE repositories (
    id INTEGER PRIMARY KEY NOT NULL,
    human_readable_name TEXT NOT NULL,
    url_safe_name TEXT NOT NULL UNIQUE,
    owner_user_id INTEGER NOT NULL,
    FOREIGN KEY (owner_user_id) REFERENCES users(id)
);

CREATE INDEX idx_repositories_owner_user_id ON repositories(owner_user_id);

CREATE TABLE users (
    id INTEGER PRIMARY KEY NOT NULL,
    username TEXT NOT NULL,
    password_hash TEXT NOT NULL
);

CREATE TABLE auth_tokens (
    id INTEGER PRIMARY KEY NOT NULL,
    token_value TEXT NOT NULL,
    user_id INTEGER NOT NULL,
    permission_id INTEGER NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (permission_id) REFERENCES permissions(id),
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_auth_tokens_user_id ON auth_tokens(user_id);
CREATE INDEX idx_auth_tokens_permission_id ON auth_tokens(permission_id);
CREATE INDEX idx_auth_tokens_repository_id ON auth_tokens(repository_id);

CREATE TABLE permissions (
    id INTEGER PRIMARY KEY NOT NULL,
    level TEXT NOT NULL
);

-- See https://sqlite.org/sqlar.html
-- If sz is equal to size of data then no compression is performed
CREATE TABLE sqlar(
  name TEXT PRIMARY KEY,  -- name of the file
  mode INT,               -- access permissions
  mtime INT,              -- last modification time
  sz INT,                 -- original file size
  data BLOB               -- compressed content
);

INSERT INTO users (id, username, password_hash) VALUES (0, 'testuser0', 'somepasswordhash');
INSERT INTO repositories (id, human_readable_name, url_safe_name, owner_user_id) VALUES (0, 'Fun Repository', 'fun-repository', 0);
