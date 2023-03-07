-- 看起來 date 可能沒有處理 timezone 的問題

SELECT DISTINCT ON (consulting_client_summaries.client_id)
  consulting_client_summaries.client_id,
  (TIMEZONE('UTC', consulting_client_summaries.created_at) AT TIME ZONE 'Asia/Taipei')::DATE AS date_free_version,
  TIMEZONE('UTC', consulting_client_summaries.created_at) AT TIME ZONE 'Asia/Taipei' AS datetime,
  consulting_client_summaries.weight,
  consulting_client_summaries.bmi,
  consulting_client_summaries.body_fat_mass AS fat,
  consulting_client_summaries.waist_circumference AS wc
FROM consulting_client_summaries
INNER JOIN group_class_orders ON consulting_client_summaries.client_id = group_class_orders.client_id
INNER JOIN group_classes ON group_class_orders.group_class_id = group_classes.id
INNER JOIN programs ON group_classes.program_id = programs.id
WHERE programs.name NOT LIKE 'FLC'
ORDER BY client_id DESC, date DESC
