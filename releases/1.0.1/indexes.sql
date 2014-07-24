CREATE INDEX idx_user_show_subscriptions_user_id ON user_show_subscriptions USING btree (user_id);
CREATE INDEX idx_user_show_subscriptions_show_id ON user_show_subscriptions USING btree (show_id);

ALTER TABLE users ALTER COLUMN id TYPE integer;
ALTER TABLE user_sessions ALTER COLUMN user_id TYPE integer;
ALTER TABLE user_show_subscriptions ALTER COLUMN user_id TYPE integer;

ALTER TABLE shows ALTER COLUMN id TYPE integer;
ALTER TABLE user_show_subscriptions ALTER COLUMN show_id TYPE integer;

CREATE TYPE episode_pointer AS (
    season_nr integer,
    episode_nr integer,
    show_id integer
);

ALTER TABLE user_show_subscriptions ADD COLUMN pointer episode_pointer;
ALTER TABLE episodes ADD COLUMN pointer episode_pointer;

UPDATE user_show_subscriptions SET pointer.season_nr = (episode_id).season_nr, pointer.episode_nr = (episode_id).episode_nr, pointer.show_id = (episode_id).show_id;
UPDATE episodes SET pointer.season_nr = (id).season_nr, pointer.episode_nr = (id).episode_nr, pointer.show_id = (id).show_id;

ALTER TABLE user_show_subscriptions DROP CONSTRAINT user_show_subscriptions_episode_id_fkey;
ALTER TABLE user_show_subscriptions DROP COLUMN episode_id;
ALTER TABLE user_show_subscriptions RENAME COLUMN pointer TO episode_id;

ALTER TABLE episodes DROP CONSTRAINT episodes_pkey;
DROP VIEW next_episodes;
ALTER TABLE episodes DROP COLUMN id;
ALTER TABLE episodes RENAME COLUMN pointer TO id;
ALTER TABLE episodes ADD PRIMARY KEY (id);

ALTER TABLE user_show_subscriptions ADD CONSTRAINT user_show_subscriptions_episode_id_fkey FOREIGN KEY(episode_id) REFERENCES episodes(id);

CREATE VIEW next_episodes AS SELECT episodes.id, lag(episodes.id) OVER (PARTITION BY (episodes.id).show_id ORDER BY (episodes.id).season_nr DESC, (episodes.id).episode_nr DESC) AS next_id, episodes.release_date FROM episodes ORDER BY (episodes.id).season_nr, (episodes.id).episode_nr;

DROP TYPE episode_id;
ALTER TYPE episode_pointer RENAME TO episode_id;