--EXPLAIN ANALYZE
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
    WHERE programs.name ILIKE '%FLC%' AND programs.org_id = 3
--    Temp
      --AND group_classes.created_at BETWEEN '2022-12-01' AND '2023-02-01'
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
  ),
  note_assets_of_flc_courses AS (
    SELECT
      note_assets.id, note_assets.data, notes_of_flc_courses.client_id,
      CASE note_assets.data->>'light'
        WHEN 'green' THEN true
        ELSE false
      END AS is_g_light
    FROM note_assets
    INNER JOIN notes_of_flc_courses
    ON note_assets.note_id = notes_of_flc_courses.id
    WHERE note_assets.url IS NOT NULL
  ),
  notes_count_of_flc_courses AS (
    SELECT
      notes.client_id,
      COUNT(notes.id) AS notes_count,
      MAX(notes.date) AS max_notes_date
    FROM notes_of_flc_courses AS notes
    GROUP BY notes.client_id
  ),
  note_assets_count_of_flc_courses AS (
    SELECT
      client_id,
      COUNT(id) AS note_assets_count,
      COUNT(id) FILTER (WHERE is_g_light IS TRUE) AS note_assets_g_light_count
    FROM note_assets_of_flc_courses
    GROUP BY client_id
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
  notes_count_of_flc_courses.max_notes_date AS date_latest_update,
  group_classes_of_flc_program_join_users_and_clients.birthday AS btd,
  AGE(CURRENT_DATE, group_classes_of_flc_program_join_users_and_clients.birthday),
--  (CURRENT_DATE - group_classes_of_flc_program_join_users_and_clients.birthday) / 365 AS age,
  group_classes_of_flc_program_join_users_and_clients.gender,
  group_classes_of_flc_program_join_users_and_clients.current->>'height' AS height,
  COALESCE(notes_count_of_flc_courses.notes_count, 0) AS dat_count,
  COALESCE(note_assets_count_of_flc_courses.note_assets_count, 0) AS pic_count,
  COALESCE(note_assets_count_of_flc_courses.note_assets_g_light_count) AS light_G_count,
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
LEFT JOIN notes_count_of_flc_courses ON group_classes_of_flc_program_join_users_and_clients.client_id = notes_count_of_flc_courses.client_id
LEFT JOIN note_assets_count_of_flc_courses ON group_classes_of_flc_program_join_users_and_clients.client_id = note_assets_count_of_flc_courses.client_id
LEFT JOIN consulting_client_summaries_before ON group_classes_of_flc_program_join_users_and_clients.client_id = consulting_client_summaries_before.client_id
LEFT JOIN consulting_client_summaries_after ON group_classes_of_flc_program_join_users_and_clients.client_id = consulting_client_summaries_after.client_id
