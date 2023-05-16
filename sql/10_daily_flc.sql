-- 10.daily_flc
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
        AND group_classes.id NOT IN (8, 24, 28, 34)
  --       AND group_classes.name NOT LIKE '%初日%' AND group_classes.name NOT LIKE '%宋醫師進階%' AND group_classes.name NOT LIKE '%診所進階%'
  --    Temp
  --      AND group_classes.created_at > '2023-04-01'
    ),
    group_classes_of_flc_program_join_users_and_clients AS (
      SELECT DISTINCT ON (clients.id, group_class_orders.group_class_id)
        group_classes_of_flc_program_join_users.*,
        group_class_orders.client_id,
        group_class_orders.group_class_id AS class_id,
        clients.birthday,
        clients.gender,
        clients.current,
        clients.mobile
      FROM group_class_orders
      INNER JOIN clients ON group_class_orders.client_id = clients.id
      INNER JOIN group_classes_of_flc_program_join_users ON group_class_orders.group_class_id = group_classes_of_flc_program_join_users.group_class_id
      --    Temp
    ),
    notes_of_flc_courses AS (
      SELECT notes.id, notes.date, notes.data, group_classes_of_flc_program_join_users_and_clients.*
      FROM notes
      INNER JOIN group_classes_of_flc_program_join_users_and_clients
        ON group_classes_of_flc_program_join_users_and_clients.client_id = notes.client_id
        AND notes.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
      --    Temp
      --  WHERE notes.created_at BETWEEN '2023-04-01' AND '2023-05-01'
    ),
    note_assets_of_flc_courses AS (
      SELECT
        note_assets.id, note_assets.data, notes_of_flc_courses.client_id,
        note_assets.data->>'light' AS light,
        note_assets.date,
        note_assets.note_id
      FROM note_assets
      INNER JOIN notes_of_flc_courses
      ON notes_of_flc_courses.id = note_assets.note_id
      WHERE note_assets.url IS NOT NULL
      -- and  note_assets.created_at BETWEEN '2023-04-01' AND '2023-05-01'
    ),
    notes_aggregation_of_flc_courses AS (
      SELECT
        notes.*,
        SUM((notes.data->>'carbohydrate')::NUMERIC) AS carbohydrate,
        SUM((notes.data->>'protein')::NUMERIC) AS protein,
        SUM((notes.data->>'fat')::NUMERIC) AS fat,
        COUNT(notes.client_id) AS note_assets_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'green') AS note_assets_g_light_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'yellow') AS note_assets_y_light_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'red')  AS note_assets_r_light_count
      FROM notes_of_flc_courses AS notes
      LEFT JOIN note_assets_of_flc_courses AS note_assets ON note_assets.note_id = notes.id
      GROUP BY notes.id, notes.id, notes.client_id, notes.date, notes.class_id, notes.data, notes.class_name, notes.program_name, notes.user_name, notes.group_class_id, notes.started_at, notes.finished_at, notes.birthday, notes.gender, notes.mobile, notes.current
    )
  SELECT
    notes_aggregation_of_flc_courses.class_id,
    notes_aggregation_of_flc_courses.class_name AS class_name,
    notes_aggregation_of_flc_courses.program_name AS program,
    notes_aggregation_of_flc_courses.started_at AS date_flc_T0,
    notes_aggregation_of_flc_courses.finished_at AS date_flc_T1,
    notes_aggregation_of_flc_courses.user_name AS nutritionist_online,
    notes_aggregation_of_flc_courses.client_id,
    notes_aggregation_of_flc_courses.mobile,
    notes_aggregation_of_flc_courses.date AS date,
    notes_aggregation_of_flc_courses.birthday AS btd,
    AGE(CURRENT_DATE, notes_aggregation_of_flc_courses.birthday),
    notes_aggregation_of_flc_courses.gender,
    notes_aggregation_of_flc_courses.current->>'height' AS height,
    SUM(notes_aggregation_of_flc_courses.note_assets_count) AS pic_count,
    SUM(notes_aggregation_of_flc_courses.note_assets_g_light_count) AS light_G_count,
    SUM(notes_aggregation_of_flc_courses.note_assets_y_light_count) AS light_Y_count,
    SUM(notes_aggregation_of_flc_courses.note_assets_r_light_count) AS light_R_count,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_G_p,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_Y_p,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_R_p,
    SUM(notes_aggregation_of_flc_courses.carbohydrate) AS carbohydrate,
    SUM(notes_aggregation_of_flc_courses.protein) AS protein,
    SUM(notes_aggregation_of_flc_courses.fat) AS fat
  FROM notes_aggregation_of_flc_courses 
  GROUP BY
    notes_aggregation_of_flc_courses.class_id,
    notes_aggregation_of_flc_courses.class_name,
    notes_aggregation_of_flc_courses.program_name,
    notes_aggregation_of_flc_courses.started_at,
    notes_aggregation_of_flc_courses.finished_at,
    notes_aggregation_of_flc_courses.user_name,
    notes_aggregation_of_flc_courses.client_id,
    notes_aggregation_of_flc_courses.mobile,
    notes_aggregation_of_flc_courses.birthday,
    notes_aggregation_of_flc_courses.gender,
    notes_aggregation_of_flc_courses.current->>'height',
    notes_aggregation_of_flc_courses.date
