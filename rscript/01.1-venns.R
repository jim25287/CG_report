library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)



#Venn unscreened
x <- list(
  Inbody = clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique(),
  Blood_Test = clinic_blood_data_ori %>% select(id) %>% pull() %>% unique(),
  Clinic_clients = clinical_list %>% select(id) %>% pull() %>% unique()
)

plot_1 <- 
ggvenn(
  x, 
  fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
  stroke_size = 0.5, set_name_size = 4
) +
  labs(title = "Whole Datasets")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  )

rm(x)

#output Whole client id (unscreened)
# cat("Whole client List_Cofit_Genesis",
#     union(clinic_inbody_data_ori %>% select(id) %>% pull(),
#           clinic_blood_data_ori %>% select(id) %>% pull()) %>% unique(),
#     sep = "\n", file = "all_client_list.csv")

#output inbody_blood_not_in_Genesis_list
# cat("List not in Genesis",
#     setdiff(intersect(clinic_blood_data_ori %>% select(id) %>% pull() %>% unique(), clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique()), clinical_list %>% select(id) %>% pull() %>% unique()),
#     sep = "\n", file = "list_not_in_Genesis.csv")


#output Excluded List
# cat("Inbody Excluded List",
#     setdiff(clinic_inbody_data_ori$id %>% unique(),
#             clinic_inbody_data_ori %>% filter(id %in% unique(clinical_list$id)) %>% select(id) %>% pull() %>% unique()),
#     sep = "\n", file = "excluded_1.csv")
# cat("Blood Excluded List",
#     setdiff(clinic_blood_data_ori$id %>% unique(),
#             clinic_blood_data_ori %>% filter(id %in% unique(clinical_list$id)) %>% select(id) %>% pull() %>% unique()),
#     sep = "\n", file = "excluded_1.csv", append = TRUE)
# cat("Inbody_Blood_not_in_Genesis List",
#     setdiff(intersect(clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique(), clinic_blood_data_ori %>% select(id) %>% pull() %>% unique()), 
#             clinical_list %>% select(id) %>% pull() %>% unique()),
#     sep = "\n", file = "excluded_1.csv", append = TRUE)



#Clinical Client exclusion
# df <- createWorkbook()
# addWorksheet(df, sheetName = "Clients_no_inbody_blood_data", gridLines = FALSE)
# writeDataTable(df, sheet = 1, 
#                clinical_list[which(clinical_list$id %in% setdiff(clinical_list %>% select(id) %>% pull() %>% unique(),  union(clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique(), clinic_blood_data_ori %>% select(id) %>% pull() %>% unique()))), ]) 
# addWorksheet(df, sheetName = "Clients_no_blood_data", gridLines = FALSE)
# writeDataTable(df, sheet = 2, 
#                clinical_list[which(clinical_list$id %in% setdiff(intersect(clinical_list %>% select(id) %>% pull() %>% unique(), clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique()), clinic_blood_data_ori %>% select(id) %>% pull() %>% unique())), ]) 
# addWorksheet(df, sheetName = "Clients_no_inbody_data", gridLines = FALSE)
# writeDataTable(df, sheet = 3, 
#                clinical_list[which(clinical_list$id %in% setdiff(intersect(clinical_list %>% select(id) %>% pull() %>% unique(), clinic_blood_data_ori %>% select(id) %>% pull() %>% unique()), clinic_inbody_data_ori %>% select(id) %>% pull() %>% unique())), ])
# saveWorkbook(df, "excluded_2.xlsx", overwrite = TRUE)
# rm(df)


# #inclusion
# clinic_inbody_data_ori %<>% filter(id %in% unique(clinical_list$id))
# clinic_blood_data_ori %<>% filter(id %in% unique(clinical_list$id))

#Venn
x <- list(
  Inbody = clinic_inbody_data_ori %>% filter(id %in% unique(clinical_list$id)) %>% select(id) %>% pull() %>% unique(),
  Blood_Test = clinic_blood_data_ori %>% filter(id %in% unique(clinical_list$id)) %>% select(id) %>% pull() %>% unique()
)

plot_2 <- 
ggvenn(
  x, 
  fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
  stroke_size = 0.5, set_name_size = 4
)+
  labs(title = "Clinic Datasets")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  )

rm(x)




x <- list(
  All_clients = before_screening_data %>% select(id) %>% pull() %>% unique(),
  Fit_criteria = stat_table_1st %>% filter(client_type != 1) %>% select(id) %>% pull() %>% unique()
)

plot_3 <- 
  ggvenn(
    x, 
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 4
  ) +
  labs(title = "Data Screening")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  )
rm(x)
rm(list = c("before_screening_data"))

