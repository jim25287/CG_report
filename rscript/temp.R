#身體維度資料 e.g., wc 改用 3D data ##**缺date 欄位



source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")


# dashboard --------------------------------------------------------------


# Pool data from cliets w/ blood data
# - weight loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - fat loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - improvement from BMI-classification
# - 減重成效，各組百分比 (**single treatment)




#1. med_id_pool
med_id_pool <- dplyr::intersect(clinic_inbody_data_ori %>% select(id) %>% unique() %>% pull(),
                                clinic_blood_data_ori %>% select(id) %>% unique() %>% pull() )
#2. produce dashborad dataset
clinic_inbody_data_dashboard <- clinic_inbody_data_ori %>% filter(id %in% med_id_pool)
clinic_blood_data_dashboard <- clinic_blood_data_ori %>% filter(id %in% med_id_pool)
rm(med_id_pool)



for (i in unique(clinic_inbody_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_inbody_data_dashboard$id),1)) {
    clinic_inbody_data_dashboard <- clinic_inbody_data_dashboard[with(clinic_inbody_data_dashboard, order(id, date_inbody)),] #order by date
    dashboard_table <- clinic_inbody_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) - clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) %>% head(1) %>% as.vector()
  #collect data
  dashboard_table <- rbind(dashboard_table, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_inbody_data_dashboard$id),1)) {
    colnames(dashboard_table) <- names(clinic_inbody_data_dashboard[0,c(-1,-2)])
    dashboard_table <- cbind(unique(clinic_inbody_data_dashboard$id), dashboard_table)
    colnames(dashboard_table)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

for (i in unique(clinic_blood_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_blood_data_dashboard$id),1)) {
    clinic_blood_data_dashboard <- clinic_blood_data_dashboard[with(clinic_blood_data_dashboard, order(id, date_blood)),] #order by date
    dashboard_table_blood <- clinic_blood_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) - clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) %>% head(1) %>% as.vector()
  
  #collect data: min
  dashboard_table_blood <- rbind(dashboard_table_blood, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_blood_data_dashboard$id),1)) {
    colnames(dashboard_table_blood) <- names(clinic_blood_data_dashboard[0,c(-1,-2)])
    dashboard_table_blood <- cbind(unique(clinic_blood_data_dashboard$id), dashboard_table_blood)
    colnames(dashboard_table_blood)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

dashboard_table <- lin_mapping(dashboard_table, client_type, id, clinical_list, client_type, id)
dashboard_table_blood <- lin_mapping(dashboard_table_blood, client_type, id, clinical_list, client_type, id)

#exclude DM duration < 30 days
dashboard_table <- dashboard_table %>% filter(client_type != 1 | is.na(client_type))
dashboard_table <- dashboard_table %>% filter((bf < -2) & (weight < -1))

dashboard_table_blood <- dashboard_table_blood %>% filter(id %in% dashboard_table$id)





#Layout
  #total client
  dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  
  #total accomplishment
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) 
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE)
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE)
  ##血生化
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE)
  
  #avg
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##血生化 不適用, 嘗試換算成∆%, baseline important
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()

 

  

# test --------------------------------------------------------------------

  line_plot_df %>% 
    ggplot( aes(x = pre_post, group = 1)) + 
    geom_point(aes(y = mean), size = 1.5, color = "red3",) +
    geom_line(aes(y = mean), size = 0.3, color = "red3") +
    geom_text(data = . %>% filter(pre_post == "After"),
              aes(y = mean, label = paste0(round(mean, 1),"%")), 
              nudge_x = -0.5, size = 3,
              show.legend = FALSE) +
    labs(x = "", y = "成效(%)", title = "")+
    xlim("Before", "After") +
    scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) +
    facet_wrap(vars(variable), scales = "free") +
    theme_linedraw() +
    theme(
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      axis.title.y = element_text(face = "bold", size = 15)
    ) 
  
  
  
  
  
  
  
  
  
  
  