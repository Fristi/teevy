CREATE TABLE users_facebook (
    facebook_id varchar(32) not null
) INHERITS (users);

CREATE TABLE users_teevy (
    email character varying(128) NOT NULL,
    password character varying(60),
    old_password character varying(128),
    old_salt character varying(32)
) INHERITS(users);

alter table subscriptions drop constraint user_show_subscriptions_user_id_fkey;

CREATE TABLE users_temp (
    id integer not null,
    nickname character varying(128) not null,
    email character varying(128) NOT NULL,
    password character varying(60),
    old_password character varying(128),
    old_salt character varying(32)
);

INSERT INTO users_temp (id, email, nickname, password, old_password, old_salt)
SELECT id, email, nickname, password, old_password, old_salt FROM users;


ALTER TABLE users DROP COLUMN email;
ALTER TABLE users DROP COLUMN password;
ALTER TABLE users DROP COLUMN old_password;
ALTER TABLE users DROP COLUMN old_salt;


TRUNCATE TABLE users;

INSERT INTO users_teevy (id, email, nickname, password, old_password, old_salt)
SELECT id, email, nickname, password, old_password, old_salt FROM users_temp;

DROP TABLE users_temp;

CREATE INDEX idx_users_facebook_facebook_user_id ON users_facebook USING btree (facebook_id);

