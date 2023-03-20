
# Q6_stat_table_1st <- stat_table_1st %>% filter(client_type != 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline))
Q6_stat_table_1st <- stat_table_1st_ob %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 

plot_M <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 2, layout = TRUE)
plot_m <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 1, layout = TRUE)





