CREATE TABLE packages (
    id INTEGER PRIMARY KEY NOT NULL,
    project TEXT NOT NULL,
    author TEXT NOT NULL,
    version TEXT NOT NULL,
    hash TEXT NOT NULL,
    repository_id INTEGER NOT NULL,
    elm_json BLOB NOT NULL,
    created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_packages_project ON packages(project);
CREATE INDEX idx_packages_author ON packages(author);
CREATE INDEX idx_packages_version ON packages(version);
CREATE INDEX idx_packages_repository_id ON packages(repository_id);
CREATE UNIQUE INDEX idx_packages_repository_id_project_author_version ON packages(repository_id, project, author, version);

CREATE TABLE repositories (
    id INTEGER PRIMARY KEY NOT NULL,
    human_readable_name TEXT NOT NULL,
    url_safe_name TEXT NOT NULL UNIQUE,
    owner_user_id INTEGER NOT NULL,
    created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (owner_user_id) REFERENCES users(id)
);

CREATE INDEX idx_repositories_owner_user_id ON repositories(owner_user_id);

CREATE TABLE users (
    id INTEGER PRIMARY KEY NOT NULL,
    username TEXT NOT NULL UNIQUE,
    password_hash BLOB NOT NULL,
    password_salt BLOB NOT NULL,
    created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX idx_users_username ON users(username);

CREATE TABLE auth_tokens (
    id INTEGER PRIMARY KEY NOT NULL,
    token_value_hash TEXT NOT NULL,
    -- No salt because token values are supposed to be random to begin with so
    -- we're not vulnerable to dictionary attacks
    --
    -- We also include a fragment of the token value for UI purposes to help
    -- users identify which token is which
    token_value_fragment TEXT NOT NULL,
    user_id INTEGER NOT NULL,
    permission_id INTEGER NOT NULL,
    repository_id INTEGER NOT NULL,
    created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (permission_id) REFERENCES permissions(id),
    FOREIGN KEY (repository_id) REFERENCES repositories(id)
);

CREATE INDEX idx_auth_tokens_user_id ON auth_tokens(user_id);
CREATE UNIQUE INDEX idx_auth_tokens_token_value_hash ON auth_tokens(token_value_hash);
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

-- Even if we switch to a provider such as Auth0 for authentication, if we want
-- the ability to provide logouts, we still need to store login session data.
CREATE TABLE login_sessions(
    id INTEGER PRIMARY KEY NOT NULL,
    -- Store this as text rather than just a binary blob because this will
    -- transmitted to the user and stored on their end with an expectation that
    -- it will be sent back. Binary data can often run into weird encoding and
    -- decoding descrepancies.
    session_token_value_hash TEXT NOT NULL,
    -- No salt because token values are supposed to be random to begin with so
    -- we're not vulnerable to dictionary attacks
    user_id INTEGER NOT NULL,
    created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE UNIQUE INDEX idx_login_sessions_session_token_value_hash ON login_sessions(session_token_value_hash);

PRAGMA journal_mode=WAL;

INSERT INTO permissions (id, level) VALUES (0, 'read');
INSERT INTO permissions (id, level) VALUES (1, 'write');
