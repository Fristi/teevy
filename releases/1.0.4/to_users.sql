CREATE TABLE users_temp (
    id integer not null primary key,
    nickname character varying(128) not null,
    email character varying(128) not null,
    password character varying(60),
    old_password character varying(128),
    old_salt character varying(32),
    facebook_user_id character varying(64)
);

CREATE SEQUENCE users_id_seq START WITH 464 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER TABLE public.users_id_seq OWNER TO teevy;
ALTER SEQUENCE users_id_seq OWNED BY users.id;
ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);

INSERT INTO users_temp (id, email, nickname, password, old_password, old_salt)
SELECT id, email, nickname, password, old_password, old_salt FROM users_teevy;

DROP TABLE users_teevy;
DROP TABLE users_facebook;
DROP TABLE users;

ALTER TABLE users_temp RENAME TO users;

