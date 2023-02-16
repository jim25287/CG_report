# main page stat. ---------------------------------------------------------
  ##  台北目前累積病人數? Total/OB(finished/ongoing)/DM(finished/ongoing)
    ### - googleVis::gvisAnnotationChart: client_monthly_stat_report
  ##  初日減重門診，已經幫助了多少人成功減重? 
    ### - [目前分析樣本]: `r success_df_freq[["Total"]] %>% max()` 
    ### - [成功減重人數(%)]: 
        # 總共: `r success_df_freq[success_df_freq$gender == "Total", "success"]` ; `r success_df_pct[success_df_freq$gender == "Total", "success"]`
        # 女性: `r success_df_freq[success_df_freq$gender == "female", "success"]` ; `r success_df_pct[success_df_freq$gender == "female", "success"]`
        # 男性: `r success_df_freq[success_df_freq$gender == "male", "success"]` ; `r success_df_pct[success_df_freq$gender == "male", "success"]`
    ### - success_df_pct
    ### - success_df_freq





# 1. read google sheet -------------------------------------------------------


library(googlesheets4)
clinical_list <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', col_types = "iilccciDccDDcccdddcccc")
#clinical_list %>% glimpse()

#df clean
clinical_list <- clinical_list[-1:-5,]
clinical_list <- clinical_list[which(!is.na(clinical_list$serial_id)),]

clinical_list %<>% select(c("serial_id","id","client_type","name","date","class_date_1","class_date_2","medication","doctor","nutritionist_major","nutritionist_online","program","history"))

clinical_list$medication_note <- NA
#Trulicity
clinical_list[grep("(licity){1}",clinical_list$medication),"medication_note"] <- "Trulicity"
#Ozempic
clinical_list[grep("(mpic){1}",clinical_list$medication),"medication_note"] <- "Ozempic"
#Rybelsus, rebylsus(wrong name) search list
clinical_list[grep("(sus){1}",clinical_list$medication),"medication_note"] <- "Rybelsus"

clinical_list %<>% relocate(medication_note, .before = medication)



#clean client_type
#DM
clinical_list[(is.na(clinical_list$client_type)) & (clinical_list$program %>% stringr::str_detect("糖尿")), "client_type"] <- 1
#OB
clinical_list[(is.na(clinical_list$client_type)) & (clinical_list$program %>% stringr::str_detect("糖尿", negate = TRUE)), "client_type"] <- 2

#Genesis
clinical_stat <- clinical_list$client_type %>% table() %>% addmargins()
#北秀: 462人





# 2. Linechart_x_Time --------------------------------------------------------


#台北目前累積病人數? Total/OB(finished/ongoing)/DM(finished/ongoing)大概多少? (by month)
## temp: from clinical list. future: from PostgreSQL
## build OB/DM note_col
#Establish category
clinical_stat_category <- factor(levels = (c("Total", "OB", "OB(ongoing)", "DM", "DM(ongoing)")))
#Establish dashboard df
accum_client_df <- data.frame(date = rep(clinical_list[["class_date_1"]] %>% lubridate::floor_date(unit = "month") %>% unique() %>% na.exclude(), each = levels(clinical_stat_category) %>% length()), 
                              category = rep(levels(clinical_stat_category), clinical_list[["class_date_1"]] %>% lubridate::floor_date(unit = "month") %>% unique() %>% na.exclude() %>% length()),
                              value = rep(NA, (clinical_list[["class_date_1"]] %>% lubridate::floor_date(unit = "month") %>% unique() %>% na.exclude() %>% length())*(levels(clinical_stat_category) %>% length())),
                              anno_title = rep(NA, (clinical_list[["class_date_1"]] %>% lubridate::floor_date(unit = "month") %>% unique() %>% na.exclude() %>% length())*(levels(clinical_stat_category) %>% length())),
                              anno_text = rep(NA, (clinical_list[["class_date_1"]] %>% lubridate::floor_date(unit = "month") %>% unique() %>% na.exclude() %>% length())*(levels(clinical_stat_category) %>% length())))
#floor date for monthly calculation
clinical_list <- clinical_list %>% mutate(date_cate = class_date_1 %>% lubridate::floor_date(unit = "month"))
clinical_list <- clinical_list %>% mutate(date_finish = class_date_2 %>% lubridate::floor_date(unit = "month"))
#clean client_type as factor
clinical_list$client_type <- factor(clinical_list$client_type, levels = c("1", "2"))

#thr summarise to integrate finish/ongoing df, group by date, client_type, exclude NA col(missing value: class_date)
client_stat_df <- 
  left_join(clinical_list %>% 
              group_by(date_cate, client_type, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_cate),
            clinical_list %>% 
              group_by(date_finish, client_type, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_finish),
            by = c("date", "client_type"),
  ) %>% lin_exclude_NA_col(., variables = c("date", "client_type"))

client_stat_df <- client_stat_df %>% dplyr::rename(class_buy = n.x, class_finish = n.y)

#Establish auxiliary df
client_stat_df$class_buy_cumsum_all <- client_stat_df$class_buy %>% cumsum()
client_stat_df[is.na(client_stat_df$class_finish), "class_finish"] <- 0
client_stat_df$class_finish_cumsum_all <- client_stat_df$class_finish %>% cumsum()
client_stat_df <- client_stat_df %>% mutate(class_ongoing_all = class_buy_cumsum_all - class_finish_cumsum_all)

client_stat_df$class_buy_cumsum_sub <- c(rbind(client_stat_df %>% filter(client_type == 1) %>% select(class_buy) %>% pull() %>% cumsum(), 
                                               client_stat_df %>% filter(client_type == 2) %>% select(class_buy) %>% pull() %>% cumsum()))

client_stat_df$class_finish_cumsum_sub <- c(rbind(client_stat_df %>% filter(client_type == 1) %>% select(class_finish) %>% pull() %>% cumsum(), 
                                                  client_stat_df %>% filter(client_type == 2) %>% select(class_finish) %>% pull() %>% cumsum()))
client_stat_df <- client_stat_df %>% mutate(class_ongoing_sub = class_buy_cumsum_sub - class_finish_cumsum_sub)


#fill in accum_client_df
accum_client_df[accum_client_df$category == "Total", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_buy_cumsum_all"]
accum_client_df[accum_client_df$category == "OB", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_finish_cumsum_sub"]
accum_client_df[accum_client_df$category == "OB(ongoing)", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_ongoing_sub"]
accum_client_df[accum_client_df$category == "DM", "value"] <- client_stat_df[client_stat_df$client_type == 1, "class_finish_cumsum_sub"]
accum_client_df[accum_client_df$category == "DM(ongoing)", "value"] <- client_stat_df[client_stat_df$client_type == 1, "class_ongoing_sub"]

rm(list = c("clinical_stat_category", "client_stat_df"))

client_monthly_stat_report <- googleVis::gvisAnnotationChart(accum_client_df,
                                                             datevar = "date",
                                                             numvar = "value",
                                                             idvar = "category",
                                                             titlevar = "anno_title",
                                                             annotation = "anno_text",
                                                             date.format = "%Y/%m/%d",
                                                             options=list(
                                                               displayAnnotations = TRUE,
                                                               #chart = "{chartArea:{backgroundColor:'#003b70'}}",
                                                               legendPosition='newRow',
                                                               width=750, height=350, gvis.editor = "[選項]:圖表轉換"
                                                             ))
#output
  ##client_monthly_stat_report %>% plot()



# 3. success_stat ----------------------------------------------------------


# 初日減重門診，已經幫助了多少人成功減重? 
## exclude:   quit/pause/missing value, [目前分析樣本] total N = `success_df_freq[["Total"]] %>% max()` 
## successfully loss weight freq: `success_df_freq[success_df_freq$gender == "Total", "success"]` `success_df_freq[success_df_freq$gender == "female", "success"]` `success_df_freq[success_df_freq$gender == "male", "success"]`
## (%): `success_df_pct[success_df_freq$gender == "Total", "success"]` `success_df_pct[success_df_freq$gender == "female", "success"]` `success_df_pct[success_df_freq$gender == "male", "success"]`

#pie chart



#create source data: a
stat_table_1st_ob$`∆weight_gp` <- cut(stat_table_1st_ob$`∆weight%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆fat_gp` <- cut(stat_table_1st_ob$`∆bf%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆muscle_mass_gp` <- cut(stat_table_1st_ob$`∆bm%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))

data <- stat_table_1st_ob %>% select(c("gender","∆weight%","∆bf%","∆bm%","∆weight_gp","∆fat_gp","∆muscle_mass_gp"))
data$gender <-  factor(data$gender, levels = c("female", "male"))
#table
#weight
a <- 
  data %>% 
  dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆weight%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
a <- a[with(a, order(gender)),]
a <- a %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(a)[c(1,3)] <- c("Group", "Value")
a$variable <-  "Weight"

#success
success_df <- a %>% filter(variable == "Weight") %>% filter(!is.nan(Value)) %>% dplyr::mutate(., success = cut(Value, c(-Inf, 0, Inf), c(1, 0)))
success_df_freq <- data.frame(gender = c("female", "male"),
                              success = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 1) %>% pull(),
                              failure = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 0) %>% pull()) %>% janitor::adorn_totals("row") %>% janitor::adorn_totals("col")
success_df_pct <- success_df_freq %>% janitor::adorn_percentages() %>% janitor::adorn_pct_formatting()
rm(success_df)
