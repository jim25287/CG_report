
# Q6_stat_table_1st <- stat_table_1st %>% filter(client_type != 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline))
Q6_stat_table_1st <- stat_table_1st_ob %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 

plot_M <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 2, layout = TRUE)
plot_m <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 1, layout = TRUE)





Q6_stat_table_1st  <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), pattern = 2)


#baseline, ∆%
Q6_stat_table_1st %>% 
  filter(!is.na(I)) %>% 
  select(I, gender, `∆weight%`, age_gp) %>% rename(value = `∆weight%`) %>% 
  mutate(value_adj = value %>% multiply_by(-1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "I", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "I"),
            label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
            position = position_dodge(0.5), 
            xlab = "", ylab = "∆Weight Loss(%)", title = "減重成效",
            legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
  ) 

Q6_stat_table_1st %>% 
  filter(!is.na(I)) %>% 
  select(I, gender, `weight_baseline`, age_gp) %>% rename(value = `weight_baseline`) %>% 
  mutate(value_adj = value %>% multiply_by(1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "I", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "I"),
            label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
            position = position_dodge(0.5), 
            xlab = "", ylab = "∆Weight Loss(%)", title = "減重成效",
            legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
  ) 

Q6_stat_table_1st %>% 
  filter(!is.na(I)) %>% 
  select(I, gender, `pbf_baseline`, age_gp) %>% rename(value = `pbf_baseline`) %>% 
  mutate(value_adj = value %>% multiply_by(1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "I", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "I"),
            label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
            position = position_dodge(0.5), 
            xlab = "", ylab = "∆Weight Loss(%)", title = "減重成效",
            legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
  ) 



df05_biochem <- df05_biochem %>% lin_DM_diagnosis(c("hba1c", "glucose_ac", "glucose_pc_1hr","glucose_pc_2hr"))
df05_biochem <- lin_insulin_rsp_pattern(df05_biochem, c("insulin", "insulin_pc_1hr", "insulin_pc_2hr"), pattern = 2)


#[2DO]: 包含介入前/後資料, [Next: 篩選介入前data]!!
table(df05_biochem$DM, df05_biochem$I, exclude = "Unclassified", useNA = "no") %>% addmargins()
table(df05_biochem$DM, df05_biochem$I, exclude = "Unclassified", useNA = "no") %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
#DM View
table(df05_biochem$DM, df05_biochem$I, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins() %>% round(2)
#Insulin Pattern  View
table(df05_biochem$DM, df05_biochem$I, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins() %>% round(2)


Q6_stat_table_1st$DM_baseline <- Q6_stat_table_1st$DM_baseline %>% factor(levels = c("Normal", "Pre-DM", "DM"))
#DM & GIRC Pattern Cross_table
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$I) %>% addmargins()
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$I) %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$I) %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins() %>% round(2)


