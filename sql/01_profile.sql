SELECT clients.id, clients.name, clients.gender, clients.birthday AS btd, clients.mobile, programs.name AS program_name, orgs.name AS org_name, group_classes.started_at AS date_T0, group_classes.finished_at AS date_T1
FROM clients
INNER JOIN group_class_orders
ON group_class_orders.client_id = clients.id
INNER JOIN group_classes
ON group_classes.id = group_class_orders.group_class_id
INNER JOIN programs
ON programs.id = group_classes.program_id
LEFT JOIN orgs
ON orgs.id = group_classes.org_id
WHERE programs.org_id IN (3, 21, 24, 26)
