CREATE INDEX idx_episodes_show_id ON episodes USING btree (((id).show_id));

DROP INDEX idx_user_show_subscriptions_show_id;
DROP INDEX idx_user_show_subscriptions_user_id;

DROP VIEW next_episodes;

CREATE TABLE next_episodes (
    id episode_id NOT NULL references episodes(id),
    next_id episode_id NULL references episodes(id)
);

CREATE INDEX id_next_episodes_id_show_id ON next_episodes USING btree (((id).show_id));

INSERT INTO next_episodes (id, next_id)
SELECT 
episodes.id, 
lag(episodes.id) OVER (PARTITION BY (episodes.id).show_id ORDER BY (episodes.id).season_nr DESC, (episodes.id).episode_nr DESC) AS next_id
FROM episodes ORDER BY (episodes.id).show_id ASC, (episodes.id).season_nr ASC, (episodes.id).episode_nr ASC;