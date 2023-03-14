--06 Diet day
--EXPLAIN ANALYZE
WITH client_diet_by_date AS (
  SELECT
    notes.client_id,
    notes.date AS date_diet,
    COUNT(DISTINCT notes.id) AS note_counts,
    COUNT(note_assets.id) FILTER(WHERE note_assets.url IS NOT NULL) AS pic_counts,
    COUNT(note_assets.id) FILTER(WHERE note_assets.url IS NULL) AS essay_count,
    COUNT(note_assets.id) FILTER(WHERE note_assets.data->>'light' = 'green') AS light_green_count,
    COUNT(note_assets.id) FILTER(WHERE note_assets.data->>'light' = 'yellow') AS light_yellow_count,
    COUNT(note_assets.id) FILTER(WHERE note_assets.data->>'light' = 'red') AS light_red_count,
    SUM((note_assets.data->>'carbohydrate')::DECIMAL) AS carbohydrate,
    SUM((note_assets.data->>'protein')::DECIMAL) AS protein,
    SUM((note_assets.data->>'fat')::DECIMAL) AS fat,
    (SUM((note_assets.data->>'carbohydrate')::DECIMAL) * 4 +
     SUM((note_assets.data->>'protein')::DECIMAL) * 4 +
     SUM((note_assets.data->>'fat')::DECIMAL) * 9) AS calorie
  FROM notes
  LEFT JOIN note_assets ON note_assets.note_id = notes.id
--  TEMP
  -- WHERE notes.date > '2023-02-20'
  GROUP BY notes.client_id, notes.date
)
SELECT DISTINCT ON (client_diet_by_date.client_id, client_diet_by_date.date_diet)
  client_diet_by_date.*, client_targets.begin_date, client_targets.end_date, client_targets.calorie AS calorie_target, client_targets.updated_at AS target_updated_at
FROM client_diet_by_date
LEFT JOIN LATERAL (
  SELECT * FROM consulting_client_targets
  ORDER BY updated_at DESC
) AS client_targets
  ON client_targets.client_id = client_diet_by_date.client_id
 AND client_diet_by_date.date_diet BETWEEN client_targets.begin_date AND client_targets.end_date
