CREATE TABLE packages (
    id INTEGER PRIMARY KEY NOT NULL,
    project TEXT NOT NULL,
    author TEXT NOT NULL,
    version TEXT NOT NULL,
    location TEXT NOT NULL,
    hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_packages_project ON packages(project);
CREATE INDEX idx_packages_author ON packages(author);
CREATE INDEX idx_packages_repository_id ON packages(repository_id);

CREATE TABLE repositories (
    id INTEGER PRIMARY KEY NOT NULL,
    human_readable_name TEXT NOT NULL,
    url_safe_name TEXT NOT NULL
);

CREATE TABLE users (
    id INTEGER PRIMARY KEY NOT NULL,
    username TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_users_repository_id ON users(repository_id);

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
