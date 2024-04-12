CREATE TABLE Packages (
    id INTEGER PRIMARY KEY NOT NULL,
    project TEXT NOT NULL,
    author TEXT NOT NULL,
    version TEXT NOT NULL,
    location TEXT NOT NULL,
    hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (repository_id) REFERENCES Repositories(id)
);

CREATE INDEX idx_packages_project ON Packages(project);
CREATE INDEX idx_packages_author ON Packages(author);
CREATE INDEX idx_packages_repository_id ON Packages(repository_id);

CREATE TABLE Repositories (
    id INTEGER PRIMARY KEY NOT NULL,
    human_readable_name TEXT NOT NULL,
    url_safe_name TEXT NOT NULL
);

CREATE TABLE Users (
    id INTEGER PRIMARY KEY NOT NULL,
    username TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (repository_id) REFERENCES Repositories(id)
);

CREATE INDEX idx_users_repository_id ON Users(repository_id);

CREATE TABLE Auth_tokens (
    id INTEGER PRIMARY KEY NOT NULL,
    token_value TEXT NOT NULL,
    user_id INTEGER NOT NULL,
    permission_id INTEGER NOT NULL,
    repository_id INTEGER NOT NULL,
    FOREIGN KEY (user_id) REFERENCES Users(id),
    FOREIGN KEY (permission_id) REFERENCES Permissions(id),
    FOREIGN KEY (repository_id) REFERENCES Repositories(id)
);

CREATE INDEX idx_auth_tokens_user_id ON Auth_tokens(user_id);
CREATE INDEX idx_auth_tokens_permission_id ON Auth_tokens(permission_id);
CREATE INDEX idx_auth_tokens_repository_id ON Auth_tokens(repository_id);

CREATE TABLE Permissions (
    id INTEGER PRIMARY KEY NOT NULL,
    level TEXT NOT NULL
);
