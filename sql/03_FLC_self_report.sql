-- EXPLAIN ANALYZE
WITH
  group_classes_of_flc_program_join_users AS (
    SELECT
      group_classes.name AS class_name,
      programs.name AS program_name,
      CONCAT(users.first_name, users.last_name) AS user_name,
      group_classes.id AS group_class_id,
      group_classes.started_at,
      group_classes.finished_at
    FROM group_classes
    INNER JOIN programs ON group_classes.program_id = programs.id
    INNER JOIN users ON group_classes.user_id = users.id
    WHERE programs.org_id = 3
      AND group_classes.name NOT LIKE '%初日%' AND group_classes.name NOT LIKE '%宋醫師進階%' AND group_classes.name NOT LIKE '%診所進階%'
--    Temp
      AND group_classes.created_at BETWEEN '2023-03-01' AND '2023-04-01'
  ),
  group_classes_of_flc_program_join_users_and_clients AS (
    SELECT 
      group_classes_of_flc_program_join_users.*,
      group_class_orders.client_id,
      group_class_orders.group_class_id AS g_id,
      clients.birthday,
      clients.gender,
      clients.current
    FROM group_class_orders
    INNER JOIN clients ON group_class_orders.client_id = clients.id
    INNER JOIN group_classes_of_flc_program_join_users ON group_class_orders.group_class_id = group_classes_of_flc_program_join_users.group_class_id
  ),
  notes_of_flc_courses AS (
    SELECT notes.*
    FROM notes
    INNER JOIN group_classes_of_flc_program_join_users_and_clients ON notes.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
           AND notes.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
    --    Temp
      WHERE notes.created_at BETWEEN '2023-03-01' AND '2023-04-01'
  ),
  note_assets_of_flc_courses AS (
    SELECT
      note_assets.id, note_assets.data, notes_of_flc_courses.client_id,
      note_assets.data->>'light' AS light,
      note_assets.date
    FROM note_assets
    INNER JOIN notes_of_flc_courses
    ON note_assets.note_id = notes_of_flc_courses.id
    WHERE note_assets.url IS NOT NULL
  ),
  notes_aggregation_of_flc_courses AS (
    SELECT
      notes.client_id,
      SUM((notes.data->>'carbohydrate')::NUMERIC) OVER() AS carbohydrate,
      (notes.data->>'protein')::NUMERIC AS protein,
      (notes.data->>'fat')::NUMERIC AS fat,
      COUNT(notes.date) OVER(PARTITION BY notes.client_id) AS notes_count,
      notes.date,
      MAX(notes.date) OVER(PARTITION BY notes.client_id) AS max_notes_date
    FROM notes_of_flc_courses AS notes
    GROUP BY notes.client_id, notes.date, notes.data
  ),
  note_assets_count_of_flc_courses AS (
    SELECT
      client_id,
      COUNT(id) OVER (PARTITION BY client_id) AS note_assets_count,
      note_assets_of_flc_courses.date AS date,
      COUNT(id) FILTER (WHERE light = 'green') OVER(PARTITION BY client_id) AS note_assets_g_light_count,
      COUNT(id) FILTER (WHERE light = 'yellow') OVER(PARTITION BY client_id) AS note_assets_y_light_count,
      COUNT(id) FILTER (WHERE light = 'red') OVER(PARTITION BY client_id) AS note_assets_r_light_count
    FROM note_assets_of_flc_courses
    GROUP BY id, client_id, note_assets_of_flc_courses.date, light
  ),
  consulting_client_summaries_before AS (
    SELECT DISTINCT ON (consulting_client_summaries.client_id) consulting_client_summaries.*
    FROM consulting_client_summaries
    INNER JOIN group_classes_of_flc_program_join_users_and_clients ON consulting_client_summaries.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
    WHERE date < group_classes_of_flc_program_join_users_and_clients.started_at
    ORDER BY client_id DESC, date DESC
  ),
  consulting_client_summaries_after AS (
    SELECT DISTINCT ON (consulting_client_summaries.client_id) consulting_client_summaries.*
    FROM consulting_client_summaries
    INNER JOIN group_classes_of_flc_program_join_users_and_clients ON consulting_client_summaries.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
    WHERE date > group_classes_of_flc_program_join_users_and_clients.started_at
    ORDER BY client_id DESC, date ASC
  )


SELECT 
  group_classes_of_flc_program_join_users_and_clients.g_id AS class_id,
  group_classes_of_flc_program_join_users_and_clients.class_name AS class_name,
  group_classes_of_flc_program_join_users_and_clients.program_name AS program,
  group_classes_of_flc_program_join_users_and_clients.started_at AS date_flc_T0,
  group_classes_of_flc_program_join_users_and_clients.finished_at AS date_flc_T1,
  group_classes_of_flc_program_join_users_and_clients.user_name AS nutritionist_online,
  group_classes_of_flc_program_join_users_and_clients.client_id,
  notes_aggregation_of_flc_courses.max_notes_date AS date_latest_update,
  group_classes_of_flc_program_join_users_and_clients.birthday AS btd,
  AGE(CURRENT_DATE, group_classes_of_flc_program_join_users_and_clients.birthday),
--  (CURRENT_DATE - group_classes_of_flc_program_join_users_and_clients.birthday) / 365 AS age,
  group_classes_of_flc_program_join_users_and_clients.gender,
  group_classes_of_flc_program_join_users_and_clients.current->>'height' AS height,
  notes_aggregation_of_flc_courses.notes_count AS day_count,
  note_assets_count_of_flc_courses.note_assets_count AS pic_count,
  note_assets_count_of_flc_courses.note_assets_g_light_count, 0 AS light_G_count,
  (SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_g_light_count, 0))) / NULLIF(SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_G_p,
  (SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_y_light_count, 0))) / NULLIF(SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_Y_p,
  (SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_r_light_count, 0))) / NULLIF(SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(note_assets_count_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_R_p,
  SUM(notes_aggregation_of_flc_courses.carbohydrate) AS carbohydrate,
  SUM(notes_aggregation_of_flc_courses.protein) AS protein,
  SUM(notes_aggregation_of_flc_courses.fat) AS fat,
  consulting_client_summaries_before.weight AS weight_before,
  consulting_client_summaries_after.weight AS weight_after,
  consulting_client_summaries_after.weight - consulting_client_summaries_before.weight AS weight_delta,
  (consulting_client_summaries_after.weight - consulting_client_summaries_before.weight) / NULLIF(consulting_client_summaries_before.weight, 0) AS weight_delta_p,
  consulting_client_summaries_before.bmi AS bmi_before,
  consulting_client_summaries_after.bmi AS bmi_after,
  consulting_client_summaries_after.bmi - consulting_client_summaries_before.bmi AS bmi_delta,
  (consulting_client_summaries_after.bmi - consulting_client_summaries_before.bmi) / NULLIF(consulting_client_summaries_before.bmi, 0) AS bmi_delta_p,
  consulting_client_summaries_before.body_fat_mass AS fat_before,
  consulting_client_summaries_after.body_fat_mass AS fat_after,
  consulting_client_summaries_after.body_fat_mass - consulting_client_summaries_before.body_fat_mass AS fat_delta,
  (consulting_client_summaries_after.body_fat_mass - consulting_client_summaries_before.body_fat_mass) / NULLIF(consulting_client_summaries_before.body_fat_mass, 0) AS fat_delta_p,
  consulting_client_summaries_before.waist_circumference AS wc_before,
  consulting_client_summaries_after.waist_circumference AS wc_after,
  consulting_client_summaries_after.waist_circumference - consulting_client_summaries_before.waist_circumference AS wc_delta,
  (consulting_client_summaries_after.waist_circumference - consulting_client_summaries_before.waist_circumference) / NULLIF(consulting_client_summaries_before.waist_circumference, 0) AS wc_delta_p
FROM group_classes_of_flc_program_join_users_and_clients
LEFT JOIN LATERAL (
  SELECT * FROM notes_aggregation_of_flc_courses
) AS notes_aggregation_of_flc_courses
  ON notes_aggregation_of_flc_courses.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
  AND notes_aggregation_of_flc_courses.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
LEFT JOIN LATERAL (
  SELECT * FROM note_assets_count_of_flc_courses
) AS note_assets_count_of_flc_courses
  ON note_assets_count_of_flc_courses.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
  AND note_assets_count_of_flc_courses.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
LEFT JOIN consulting_client_summaries_before ON group_classes_of_flc_program_join_users_and_clients.client_id = consulting_client_summaries_before.client_id
LEFT JOIN consulting_client_summaries_after ON group_classes_of_flc_program_join_users_and_clients.client_id = consulting_client_summaries_after.client_id
GROUP BY
  group_classes_of_flc_program_join_users_and_clients.g_id,
  group_classes_of_flc_program_join_users_and_clients.class_name,
  group_classes_of_flc_program_join_users_and_clients.program_name,
  group_classes_of_flc_program_join_users_and_clients.started_at,
  group_classes_of_flc_program_join_users_and_clients.finished_at,
  group_classes_of_flc_program_join_users_and_clients.user_name,
  group_classes_of_flc_program_join_users_and_clients.client_id,
  notes_aggregation_of_flc_courses.max_notes_date,
  group_classes_of_flc_program_join_users_and_clients.birthday,
  group_classes_of_flc_program_join_users_and_clients.gender,
  group_classes_of_flc_program_join_users_and_clients.current->>'height',
  notes_aggregation_of_flc_courses.notes_count,
  note_assets_count_of_flc_courses.note_assets_count,
  note_assets_count_of_flc_courses.note_assets_g_light_count,
  consulting_client_summaries_before.weight,
  consulting_client_summaries_after.weight,
  consulting_client_summaries_before.bmi,
  consulting_client_summaries_after.bmi,
  consulting_client_summaries_before.body_fat_mass,
  consulting_client_summaries_after.body_fat_mass,
  consulting_client_summaries_before.waist_circumference,
  consulting_client_summaries_after.waist_circumference



