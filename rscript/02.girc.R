
# Q6_stat_table_1st <- stat_table_1st %>% filter(client_type != 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline))
Q6_stat_table_1st <- stat_table_1st_ob %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 

plot_M <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 2, layout = TRUE)
plot_m <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 1, layout = TRUE)




#baseline, ∆%
Q6_stat_table_1st %>% 
  filter(!is.na(Pattern_major_baseline)) %>% 
  select(Pattern_major_baseline, gender, `∆weight%`) %>% rename(value = `∆weight%`) %>% 
  mutate(value_adj = value %>% multiply_by(-1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
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
  filter(!is.na(Pattern_major_baseline)) %>% 
  select(Pattern_major_baseline, gender, `weight_baseline`) %>% rename(value = `weight_baseline`) %>% 
  mutate(value_adj = value %>% multiply_by(1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
            label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
            position = position_dodge(0.5), 
            xlab = "", ylab = "Weight(Kg)", title = "減重成效",
            legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
  ) 

Q6_stat_table_1st %>% 
  filter(!is.na(Pattern_major_baseline)) %>% 
  select(Pattern_major_baseline, gender, `pbf_baseline`) %>% rename(value = `pbf_baseline`) %>% 
  mutate(value_adj = value %>% multiply_by(1)) %>% 
  ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
            add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
            label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
            position = position_dodge(0.5), 
            xlab = "", ylab = "PBF(%)", title = "減重成效",
            legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
  ) 



#篩選介入前data
a <- df05_biochem %>% distinct(id, .keep_all = TRUE) %>% view()
table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no") %>% addmargins()
table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no") %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
#DM View
table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins() %>% round(2)
#Insulin Pattern  View
table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins() %>% round(2)


Q6_stat_table_1st$DM_baseline <- Q6_stat_table_1st$DM_baseline %>% factor(levels = c("Normal", "Pre-DM", "DM"))
#DM & GIRC Pattern Cross_table in OB. program
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no") %>% addmargins()
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no") %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins() %>% round(2)
table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no") %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins() %>% round(2)


#GIRC improvement path?
  ##Pool improvement - fragment alignment/mapping: ncol:3(id, origin, aftermath)
df05_biochem

##T0,T1,∆ plot

