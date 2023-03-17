#Lin_functions

library(magrittr)
library(dplyr)
library(googleVis)


# Font --------------------------------------------------------------------

##---show Chinese Font
library(showtext)
font_add(family = "berlin_default", regular = "/System/Library/Fonts/STHeiti Light.ttc")
#family name随便起或者和字体名字一样，只是个代号，路径填字体文件的绝对路径，后面声明family = "<family_name>"就行
#font_files()可以查看可用字体的路径和名字
showtext_auto(enable = TRUE)



## [Function 1: ]Mapping function was designed by LincolnFa (R.O.C) on Nov. 12, 2 ----------------



lin_mapping <- function(mapping_df, mapping_variable,mapping_ref,
                        lookup_df, lookup_variable, lookup_ref,
                        overwrite = FALSE){
  #overwrite > logical, default: FALSE
  
  # Start the clock!
  ptm <- proc.time()
  
  #package
  library(dplyr)
  library(magrittr)
  #[df轉換為字串]: df > character
  mapping_df_chr <- deparse(substitute(mapping_df))
  mapping_variable_chr <- deparse(substitute(mapping_variable))
  mapping_ref_chr <- deparse(substitute(mapping_ref))
  lookup_df_chr <- deparse(substitute(lookup_df))
  lookup_variable_chr <- deparse(substitute(lookup_variable))
  lookup_ref_chr <- deparse(substitute(lookup_ref))
  
  
  
  #df$variable
  mapping_ref_x <- eval(parse(text = paste(mapping_df_chr, mapping_ref_chr, sep = "$")))
  lookup_variable_x <- eval(parse(text = paste(lookup_df_chr, lookup_variable_chr, sep = "$")))
  lookup_ref_x <- eval(parse(text = paste(lookup_df_chr, lookup_ref_chr, sep = "$")))
  
  #若還沒新增欄位,add mapping variable
  if (!(mapping_variable_chr %in% colnames(mapping_df))) {
    mapping_df[mapping_variable_chr] <- NA
  }
  
  j = 0
  k = 0
  #Mapping code begins
  for (i in c(1:nrow(mapping_df))) {
    #length != 0, 若查得到資料
    if (length(which(lookup_ref_x == mapping_ref_x[i])) != 0) {
      #是否覆蓋原data, 有data and 不覆蓋 => 不寫入data
      if ( !(is.na(mapping_df[i,mapping_variable_chr]))  && (overwrite == FALSE) ) {
      }else{
        #若查到多筆,選第1筆mapping
        mapping_df[i,mapping_variable_chr] <- lookup_variable_x[which(lookup_ref_x == mapping_ref_x[i])[1]]
        k = k + 1
      }
    }else{
      #若沒查到,計數+1
      j = j + 1
    }
  }#Mapping code ends
  
  #Print result
  if (i == nrow(mapping_df)) {
    cat("[Mapping completed!]", 
        paste0("- match counts= ", nrow(mapping_df)-j),
        paste0("- unmatch counts= ", j), 
        paste0("- overwrite counts= ", k),
        paste0( "- missing value counts= ", length(which(is.na(mapping_df[,mapping_variable_chr])))), fill = 2)
    
  }
  # Stop the clock
  cat("[執行時間]\n")
  print(proc.time() - ptm)
  return(mapping_df)
  #function end
}



#[Function 2: ] 產生欄位字串工具 function was designed by LincolnFa (R.O.C) on Nov. 12, 11 --------
lin_print_colname = function(data){
  if (is.vector(data)) {
    cat("Format: \"text\" \n")
    cat(paste0("c(","\"", paste(names(data), collapse = "\",\""),"\"",")"))
    cat("\nFormat: \`text\` \n")
    cat(paste0("c(","\`", paste(names(data), collapse = "\`,\`"),"\`",")"))
  }else{
  data <- as.data.frame(data)
  cat("Format: \"text\" \n")
  cat(paste0("c(","\"", paste(names(data), collapse = "\",\""),"\"",")"))
  cat("\nFormat: \`text\` \n")
  cat(paste0("c(","\`", paste(names(data), collapse = "\`,\`"),"\`",")"))
  }
  #function ends
}




#[Function 3: ] Pie chart function --------
#Input: dataset, variable, graph title 

lin_pie_chart <- function(df, variable, title, print = FALSE){
  #package
  library(ggplot2)
  library(ggrepel)
  library(tidyverse)
  library(magrittr)
  #[DF轉換為字串]: df > character
  data <- deparse(substitute(df))
  variable <- deparse(substitute(variable))
  
  #data$group
  x <- eval(parse(text=paste(data, variable,sep = "$")))
  #data
  x1 <- eval(parse(text = data))
  #group
  x2 <- as.name(variable)
  #graph title name
  x3 <- as.character(title)
  
  baseline_gp_dist <- x %>% table() %>% as.data.frame()
  #run pie chart
  baseline_gp_dist <- baseline_gp_dist %>% 
    mutate(percentage = round(Freq/sum(Freq)*100,2) )
  
  #position
  baseline_gp_dist_2 <- baseline_gp_dist %>% 
    mutate(csum = rev(cumsum(rev(percentage))), 
           pos = percentage/2 + lead(csum, 1),
           pos = if_else(is.na(pos), percentage/2, pos))
  #ggplot: pie chart
  
  if (length(baseline_gp_dist$.) <= 9) {
    plot <- 
    ggplot(baseline_gp_dist, aes(x = "" , y = percentage, fill = .)) +
      geom_col(width = 1, color = "#000000") +
      coord_polar(theta = "y") +
      scale_fill_brewer( palette =  "Pastel1", direction = 0.5) +
      geom_label_repel(data = subset(baseline_gp_dist_2, percentage != 0),
                       aes(y = pos, label = paste0(percentage, "%")),
                       size = 6.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Group")) +
      theme_void()+
      theme(plot.title = element_text(size=20,face="bold",hjust = 0.5, vjust = 1.0,
                                      margin = margin(10,0,25,0)),
            legend.text = element_text(size = 15),
            plot.margin = margin(1, 1, 1, 1, "cm")
      )+
      ggtitle(x3) 
  }else{
    plot <- 
    ggplot(baseline_gp_dist, aes(x = "" , y = percentage, fill = .)) +
      geom_col(width = 1, color = "#000000") +
      coord_polar(theta = "y") +
      scale_fill_brewer( palette =  "Set3", direction = 0.5) +
      geom_label_repel(data = subset(baseline_gp_dist_2, percentage != 0),
                       aes(y = pos, label = paste0(percentage, "%")),
                       size = 6.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Group")) +
      theme_void()+
      theme(plot.title = element_text(size=20,face="bold",hjust = 0.5, vjust = 1.0,
                                      margin = margin(0,0,25,0)),
            legend.text = element_text(size = 15),
            plot.margin = margin(1, 1, 1, 1, "cm")
      )+
      ggtitle(x3) 
  }
  
  if (print == TRUE) {
    jpeg(paste0("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/02.Analysis/03. Layout/00. img/", x3,  
                "_", Sys.Date(), ".jpg"),  
         type = "quartz",
         res = 300,
         width = 2400, height = 2400, units = "px")
    print(plot)
    dev.off()
    cat("\n",
        "[Successfully output jpeg!]",
        "\n\n")
  }
  
  return(plot)
  
}


#[Function 5: ] box plot function --------

boxplot_framework <- function(upper_limit,
                              family_font = "berlin_default",
                              lower_limit = 0,
                              logY = FALSE,
                              fill_var = NA,
                              fill = "lightgrey", width = 0.6){
  
  update_geom_defaults("text",
                       list(size = 3,
                            family = family_font))
  
  n_fun <- function(x, lY = logY){
    return(data.frame(y = ifelse(logY, 0.95*log10(upper_limit), 0.95*upper_limit),
                      label = length(x)))
  }
  
  prettyLogs <- function(x){
    pretty_range <- range(x[x > 0])
    pretty_logs <- 10^(-10:10)
    log_index <- which(pretty_logs < pretty_range[2] &
                         pretty_logs > pretty_range[1])
    log_index <- c(log_index[1]-1,log_index,
                   log_index[length(log_index)]+1)
    pretty_logs_new <-  pretty_logs[log_index]
    return(pretty_logs_new)
  }
  
  fancyNumbers <- function(n){
    nNoNA <- n[!is.na(n)]
    x <-gsub(pattern = "1e",replacement = "10^",
             x = format(nNoNA, scientific = TRUE))
    exponents <- as.numeric(sapply(strsplit(x, "\\^"), function(j) j[2]))
    
    base <- ifelse(exponents == 0, "1", ifelse(exponents == 1, "10","10^"))
    exponents[base == "1" | base == "10"] <- ""
    textNums <- rep(NA, length(n))
    textNums[!is.na(n)] <- paste0(base,exponents)
    
    textReturn <- parse(text=textNums)
    return(textReturn)
  }
  
  if(!is.na(fill_var)){
    basic_elements <- list(stat_boxplot(geom ='errorbar', width = width),
                           geom_boxplot(width = width),
                           stat_summary(fun.data = n_fun,
                                        geom = "text",
                                        position = position_dodge(width),
                                        hjust =0.5,
                                        aes_string(group=fill_var)),
                           expand_limits(y = lower_limit),
                           theme_USGS_box())
  } else {
    basic_elements <- list(stat_boxplot(geom ='errorbar', width = width),
                           geom_boxplot(width = width, fill = fill),
                           stat_summary(fun.data = n_fun,
                                        geom = "text", hjust =0.5),
                           expand_limits(y = lower_limit),
                           theme_USGS_box())
  }
  
  if(logY){
    return(c(basic_elements,
             scale_y_log10(limits = c(lower_limit, upper_limit),
                           expand = expand_scale(mult = c(0, 0)),
                           labels=fancyNumbers,
                           breaks=prettyLogs),
             annotation_logticks(sides = c("rl"))))
  } else {
    return(c(basic_elements,
             scale_y_continuous(sec.axis = dup_axis(label = NULL,
                                                    name = NULL),
                                expand = expand_scale(mult = c(0, 0)),
                                breaks = pretty(c(lower_limit,upper_limit), n = 5),
                                limits = c(lower_limit,upper_limit))))
  }
}

ggplot_box_legend <- function(family = "berlin_default"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    geom_text(aes(x = 1.17, y = 950,
                  label = "Number of values"),
              fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside value"),
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.9),
                  y =  ggplot_output[["lower_dots"]],
                  label = "-Value is >1.5 times and"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot)
  
}


theme_USGS_box <- function(base_family = "berlin_default", ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 8),
      axis.ticks.length = unit(-0.05, "in"),
      axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.ticks.x = element_blank(),
      aspect.ratio = 1,
      legend.background = element_rect(color = "black", fill = "white")
    )
}




#show the lm equation (https://stats.stackexchange.com/questions/63600/how-to-translate-the-results-from-lm-to-an-equation )
  #Example: model_equation(modelcrime, digits = 3, trim = TRUE)
model_equation <- function(model, ...) {
  library(dplyr)
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}




# [Function 6:] Histo-Density + QQ plot + Normality Test ------------------------------------------------------



lin_data_profile <- function(data, x, group = NA, add = mean) {
  # Start the clock!
  ptm <- proc.time()
  
  library(ggpubr)
  library(cowplot)
  
  #[df轉換為字串]: df > character
  data_chr <- deparse(substitute(data))
  x_chr <- deparse(substitute(x))
  group_chr <- deparse(substitute(group))
  add_chr <- deparse(substitute(add))
  
  #[col_variable轉換為name?] [data$col]
  x_name<- eval(substitute(x), data, parent.frame())
  
  if (group_chr != "NA") {
    # 1. Create the histogram plot
    hist <- gghistogram(
      data,
      x = x_chr,
      bins = 30,
      add = add_chr,
      rug = TRUE,
      fill = group_chr,
      palette = c("#00AFBB", "#E7B800")
    )
    
    #2. Create the density plot with y-axis on the right
    #Remove x axis elements
    density <- ggdensity(
      data, x = x_chr,
      color= group_chr,
      palette = c("#00AFBB", "#E7B800"),
      alpha = 0,
      lwd = 1.0
    ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
      theme_half_open(11, rel_small = 1) +
      rremove("x.axis")+
      rremove("xlab") +
      rremove("x.text") +
      rremove("x.ticks") +
      rremove("legend")
  }
  
  if (group_chr == "NA") {
    # 1. Create the histogram plot
    hist <- gghistogram(
      data,
      x = x_chr,
      bins = 30,
      add = add_chr,
      rug = TRUE,
      fill = "#00AFBB",
      palette = c("#00AFBB", "#E7B800")
    )
    
    #2. Create the density plot with y-axis on the right
    #Remove x axis elements
    density <- ggdensity(
      data, x = x_chr,
      color= "#00AFBB",
      palette = c("#00AFBB", "#E7B800"),
      alpha = 0,
      lwd = 1.0
    ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
      theme_half_open(11, rel_small = 1) +
      rremove("x.axis")+
      rremove("xlab") +
      rremove("x.text") +
      rremove("x.ticks") +
      rremove("legend")
  }
  
  # 3. Align the two plots and then overlay them.
  aligned_plots <- align_plots(hist, density, align="hv", axis="tblr")
  plot_1 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  
  
  
  #4. QQ plot
  plot_2 <- ggplot(data, aes(sample = x_name))+
    stat_qq() +
    stat_qq_line() +
    theme_bw()
  
  
  cat("Data =", paste0(data_chr, ","),
      "Varialbe =", x_chr, 
      "\n",
      sep = " ")
  
  #5. Normality test
  if (length(x_name) <= 50 ) {
    ##5.1常態性檢定 Shapiro-Wilk常態檢定：樣本數50個以下。
    cat("N =", length(x_name)," [N <= 50 >> Normality Test: Shapiro-Wilk \n")
    normality_result <- shapiro.test(x_name)
  }else{
    ##5.2常態性檢定 Kolmogorov-Smirnov（K-S）檢定：樣本數50個以上。
    cat("N =", length(x_name), "[N > 50] >> Normality Test: Kolmogorov-Smirnov Test \n")
    normality_result <- ks.test(x_name+runif(length(x_name),-0.05,0.05),"pnorm",mean(x_name),sd(x_name))
    
  }
  
  
  library(cowplot)
  result <- plot_grid(plot_1, plot_2)
  print(normality_result)
  
  
  cat("[統計結果]\n")
  if (normality_result$p.value < 0.05) {
    cat("p-value = ",
        normality_result$p.value,
        "< 0.05",
        "\n",
        ">> Non-normal distribution",
        "\n\n")
  }else{
    cat("p-value = ",
        normality_result$p.value, 
        "> 0.05", 
        "\n",
        ">> Normal distribution",
        "\n\n")
  }
  print(summary(x_name))
  
  cat("\n\n[執行時間]\n")
  print(proc.time() - ptm)
  
  return(result)
  #function end
  
}





# [Function 7:] Screening out missing value -------------------------------

lin_exclude_NA_col <- function(df, variables){
  # Start the clock!
  df1 <- df
  
  ptm <- proc.time()
  
  #establish list
  for (i in c(1:length(variables))) {
    if (i == 1) {
      #i = 1
      screen_variables_list <- list(which(!is.na(df1[[variables[i]]])))
    }else{
      #i > 1
      screen_variables_list <- append(screen_variables_list, 
                                      list(which(!is.na(df1[[variables[i]]]))))
    }
  }
  
  
  df1 <- df1[Reduce(dplyr::intersect, screen_variables_list), ]
  
  
  cat("\n[Completed!]\n")
  cat("\n[執行時間]\n")
  print(proc.time() - ptm)
  
  cat("Total Obs.:", nrow(df),"counts\n",
      "Included Obs.:", nrow(df1),"counts\n",
      "Excluded Obs.:", nrow(df) - nrow(df1),"counts\n",
      "Total Clients.:", df[["id"]] %>% unique() %>% length(),"counts\n",
      "Included Clients:", df1[["id"]] %>% unique() %>% length(),"counts\n",
      "Excluded Clients:", df[["id"]] %>% unique() %>% length() - df1[["id"]] %>% unique() %>% length(),"counts\n")
  
  return(df1)
  #function end
  
}


# [Function 8:] progess bar -------------------------------
progress <- function (x, max = 100) {
  time <- difftime(Sys.time(), start_time, units = "secs") %>% round(0) %>% hms::as_hms()
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d%%',
              paste(rep('=', percent / 2), collapse = ''),
              floor(percent)), paste0("(", x, "/", max, ") ", time))
  if (x == max)
    cat('\n')
}

# [Function 9:] calculate AUC -------------------------------
lin_AUC_calc <- function(df, variables, increment_value = 0){
  library(magrittr)
  # Start the clock!
  ptm <- proc.time()
  
  df <- df %>% dplyr::select(variables) - increment_value
  AUC_result <- c()
  OGTT_time<-c(0,60,120)
  for (i in c(1:nrow(df))) {
    OGTT_value  <- df[i,] %>% as.numeric()
    AUC<-data.frame(OGTT_time,OGTT_value)
    AUC_result[i] <- caTools::trapz(AUC$OGTT_time, AUC$OGTT_value)
  }
  
  cat("\n[Completed!]\n")
  cat("\n[執行時間]\n")
  print(proc.time() - ptm)
  
  return(AUC_result)
  
}


# [Function 10:] Insulin Response Pattern Categorization -------------------------------
#****[20221219 version, confirmed]
lin_insulin_rsp_pattern <- function(df, variables, pattern = 1, plot = NULL, table_img = FALSE, layout = FALSE){
  
  plot_chr <- deparse(substitute(plot))
  
  
  # Start the clock!
  ptm <- proc.time()
  
  library(data.table)
  library(magrittr)
  
  df[["I"]] <- "Unclassified"
  
  setDT(df)[
    (is.na(eval(parse(text = variables[1])))) |
      (is.na(eval(parse(text = variables[2])))) |
      (is.na(eval(parse(text = variables[3])))),
    I := NA]
  
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[3])) < 37) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern I (Low)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[3])) < 37) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern I (High.0)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[3])) < 37) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern I (High.1)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[3])) < 37) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern I (High.B)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern II (Low)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern II (High.0)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern II (High.1)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern II (High.B)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern III (Low)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern III (High.0)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern III (High.1)"]
  
  setDT(df)[
    !(eval(parse(text = variables[1])) <= 10) & (eval(parse(text = variables[1])) <= 25) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern III (High.B)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) > 25) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[3])) < 37) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern IV"]
  
  
  setDT(df)[
    (eval(parse(text = variables[1])) > 25) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & (eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern IV (I-alike, High)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) > 25) &
      (eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern IV (II-alike, Low)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) > 25) &
      !(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[3])) < 37) & (eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern IV (II-alike, High)"]
  
  
  setDT(df)[
    (eval(parse(text = variables[1])) > 25) &
      #!(eval(parse(text = variables[2])) <= 90) &
      (eval(parse(text = variables[1])) < eval(parse(text = variables[2]))) & !(eval(parse(text = variables[2])) > eval(parse(text = variables[3]))),
    I := "Pattern IV (III-alike)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 30) &
      (eval(parse(text = variables[2])) <= 30) &
      (eval(parse(text = variables[3])) <= 30),
    I := "Pattern V (Late DM)"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) <= 10) &
      (eval(parse(text = variables[2])) <= 10) &
      (eval(parse(text = variables[3])) <= 10),
    I := "Pattern V (T1DM)"]
  
  
  #
  if (pattern == 1) {
    df[["I"]] <- factor(df[["I"]], levels = (c("Pattern I (Low)","Pattern I (High.0)","Pattern I (High.1)","Pattern I (High.B)",
                                               "Pattern II (Low)","Pattern II (High.0)","Pattern II (High.1)","Pattern II (High.B)",
                                               "Pattern III (Low)","Pattern III (High.0)","Pattern III (High.1)","Pattern III (High.B)",
                                               "Pattern IV",	"Pattern IV (I-alike, High)","Pattern IV (II-alike, Low)","Pattern IV (II-alike, High)","Pattern IV (III-alike)", 
                                               "Pattern V (T1DM)", "Pattern V (Late DM)",
                                               "Unclassified")))
  }else if (pattern == 2) {
    
    b <- gsub("\\s", "", df$I)
    b <- gsub("[(][A-Za-z0-9.,-]{0,}[)]", "", b)
    b <- gsub("Pattern", "", b)
    df[["I"]]<- paste("Pattern", gsub("[^IV]", "", b), sep = " ")
    df[["I"]][which(b == "Pattern NA")] <- NA
    
    df[["I"]] <- factor(df[["I"]], levels = (c("Pattern I","Pattern II","Pattern III","Pattern IV", "Pattern V","Unclassified")))
    
  }
  
  
  #summary table
  summary_table <- 
    df %>% 
    group_by(I, .drop = FALSE) %>% 
    summarise("insulin_ac(SE)" = paste(mean(eval(parse(text = variables[1]))) %>% round(2), sd(eval(parse(text = variables[1]))) %>% divide_by(sqrt(n())) %>% round(2), sep = " ± "),
              "insulin_pc1(SE)" = paste(mean(eval(parse(text = variables[2]))) %>% round(2), sd(eval(parse(text = variables[2]))) %>% divide_by(sqrt(n())) %>% round(2), sep = " ± "),
              "insulin_pc2(SE)" = paste(mean(eval(parse(text = variables[3]))) %>% round(2), sd(eval(parse(text = variables[3]))) %>% divide_by(sqrt(n())) %>% round(2), sep = " ± "),
              N = n()) 
  
  summary_table_kable <- 
    summary_table %>% 
    kable(format = "html", caption = "<b>Categorization: Insulin Response Pattern</b>", align = "c") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE, font_size = 15) %>% 
    gsub("font-size: initial !important;", 
         "font-size: 12pt !important;", 
         .)
  
  
  #add percentage
  setDT(summary_table)[, percent := (prop.table(N)*100) %>% round(2) ]
  
  #count
  count_plot <- 
  summary_table %>%
    mutate( I = forcats::fct_rev(I)) %>% 
    ggplot(aes(x = I, y = N))+
    geom_bar(stat="identity",
             position="dodge",
             fill = "#00AFBB")+
    geom_text(aes(label = paste0(N)),
              vjust = 0.5, hjust = -0.5,
              size = 3) +
    scale_y_continuous(limits = c(0, ceiling(max(summary_table[["N"]])/50)*50))+
    labs(title = "GIRC分型", x = "", y = "Count")+
    theme_bw()+
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12))+
    coord_flip()
  
  
  
  #percentage
  percentage_plot <- 
  summary_table %>%
    mutate( I = forcats::fct_rev(I)) %>% 
    ggplot(aes(x = I, y = percent))+
    geom_bar(stat="identity",
             position="dodge",
             fill = "#00AFBB")+
    geom_text(aes(label = paste0(percent,"%")),
              vjust = 0.5, hjust = -0.2,
              size = 3) +
    scale_y_continuous(limits = c(0, ceiling(max(summary_table[["percent"]])/10)*10 +5),
                       breaks = seq(0, ceiling(max(summary_table[["percent"]])/10)*10 +5, 5)) +
    labs(title = "GIRC分型", x = "", y = "Percentage(%)")+
    theme_bw()+
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12))+
    coord_flip()
  
  
  
  if (table_img == TRUE) {
    summary_table_kable %>% 
      save_kable(file = paste0("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/02.Analysis/03. Layout/00. img/", "statistics_table", 
                               "_", Sys.Date(), ".jpg"),
                 zoom = 5, bs_theme = "journal")
  }
  
  if (layout == FALSE) {
    
    if (plot_chr == "count") {
      print(count_plot)
    }else if (plot_chr == "percentage") {
      print(percentage_plot)
    }
    
    cat("\n[Completed!]\n")
    cat("\n[執行時間]\n")
    print(proc.time() - ptm)
    
    cat("\n[Result]\n")
    
    result <- table(df[["I"]]) %>% addmargins() %>% as.data.frame()
    
    print(result)
    print(summary_table_kable)
    return(df)
  }else{
    
    if (plot_chr == "count") {
      plot <- count_plot
      print(count_plot)
    }else if (plot_chr == "percentage") {
      plot <- percentage_plot
      print(percentage_plot)
    }
    print(summary_table_kable)
    a <- list(summary_table_kable, plot)
    return(a)
  }
  
  
}




# [Function 11:] change vars language format ---------------------------------------------------------------------

lin_ch_en_format <- function(x, format, origin){
  #1. input vars_table
  if (exists("vars_table") %>% not()) {
    library(googlesheets4)
    vars_table <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', 
                                            sheet = "vars_table",
                                            col_types = "icccc")
    names(vars_table) <- c("num", "item_id", "ch", "en", "raw_en")
  }
  
  #2. return vars not establish in the vars_table
  if (length(x[which(!(x %in% vars_table[[origin]]))]) != 0) {
    cat("\n[Not matched list:]\n")
    return(x[which(!(x %in% vars_table[[origin]]))])
  }else{
    #3. change format
    for (i in c(1:length(x))) {
      if (i == 1) {
        #重複選第一筆
        results <- vars_table[vars_table[[origin]] %in% (x[i]), format] %>% head(1)
      }else{
        #重複選第一筆
        results <- append(results, vars_table[vars_table[[origin]] %in% (x[i]), format] %>% head(1))
      }
    } 
    results <- results %>% as.character()
    cat("\n[Successfully Match:]\n")
    return(results)
  }
}



# [Function 12:] Connect to db -----------------------------------------------------------
lin_connect_db <- function(options = NULL){
  
  # #Info
  # host = '35.201.248.55'x
  # pw = 'zCxHjp0Byy11Jm2D'
  # db = 'warehouse_production'
  # user = 'postgres'
  # path =  "/Users/lincoln/Lincoln/02.Work/04.\ R&D/02.\ HIIS_OPP/02.Analysis/01.\ Code/00.\ ProgreSQL/"
  
  
  if (is.null(options)) {
    cat("\n", "[Options]:\n",
        "1. test: Connect test.\n",
        "2. connect: Connect to db.\n",
        "3. exit: Disconnect.\n",
        "[Query]:\n",
        "- df <- DBI::dbGetQuery(db, 'SELECT * FROM products')\n",
        "- dplyr::glimpse(df)\n\n")
    options_chr <- ""
  }else{
    options_chr <- as.character(options)
  }
  
  if ((options_chr == "test") | (options_chr == "connect")) {
    source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/02.Analysis/01.\ Code/connect_db_test.R")
    
    if ((connect_test == TRUE)) {
      print("[Successful connection!!]")
      if ((options_chr == "connect")) {
        source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/02.Analysis/01.\ Code/connect_db.R")
      }
    }else{
      print("[Connection Failure QQ!]")
    }
  }
  if (options_chr == "exit") {
    source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/02.Analysis/01.\ Code/disconnect_db.R")
  }
  
}



# [Function 13:] timestamp_encrypt ---------------------------------------------------------------

lin_timestamp <- function(time = Sys.time()){
  library(magrittr)
  library(stringr)
  key = 303
  if (is.character(time)) {
    timestamp_a <- time %>% stringr::str_sub(start = 1L, end = 3L) %>% strtoi(base = 16L) %>% magrittr::add(-key)
    timestamp_b <- time %>% stringr::str_sub(start = 4L, end = 6L) %>% strtoi(base = 16L) %>% magrittr::add(-key)
    timestamp_c <- time %>% stringr::str_sub(start = 7L, end = 9L) %>% strtoi(base = 16L) %>% magrittr::add(-key)
    timestamp_de <- c(timestamp_a, timestamp_b, timestamp_c)
    timestamp_de <- paste(timestamp_de, collapse = ":")
    return(timestamp_de)
  }else{
    timestamp_en <- format(time, "%H:%M:%S") %>% stringr::str_split(pattern = ":") %>% unlist() %>% as.numeric() %>% magrittr::add(key) %>% format.hexmode(width = 2, upper.case = TRUE) %>% paste0(collapse = "")
  }
  timestamp_en <- format(time, "%H:%M:%S") %>% stringr::str_split(pattern = ":") %>% unlist() %>% as.numeric() %>% magrittr::add(key) %>% format.hexmode(width = 2, upper.case = TRUE) %>% paste0(collapse = "")
  return(timestamp_en)
}





# [Function 10:] Insulin Response Pattern Categorization -------------------------------
lin_DM_diagnosis <- function(df = NULL, variables){
  
  
  if (is.null(df)) {
    cat("lin_DM_diagnosis(df, variables)","\n",
        "variables = \n",
        "1. hba1c\n",
        "2. glucose_ac\n",
        "3. glucose_pc_1hr\n",
        "4. glucose_pc_2hr\n",
        "try: df %>% names() %>% grep(\"hba1c|glucose\", ., value = TRUE)\n\n"
        )
  }
  
  
  # Start the clock!
  ptm <- proc.time()
  
  library(data.table)
  library(magrittr)
  
  df[["DM"]] <- "Unclassified"
  
  setDT(df)[
    ((eval(parse(text = variables[1])) >= 5.7) & (eval(parse(text = variables[1])) < 6.5)) |
      ((eval(parse(text = variables[2])) >= 100) & (eval(parse(text = variables[2])) < 126)) |
      ((eval(parse(text = variables[4])) >= 140) & (eval(parse(text = variables[4])) < 200)),
    DM := "Pre-DM"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) >= 6.5) |
      (eval(parse(text = variables[2])) >= 126) |
      (eval(parse(text = variables[4])) >= 200),
    DM := "DM"]
  
  setDT(df)[
    (eval(parse(text = variables[1])) < 5.7) &
      (eval(parse(text = variables[2])) < 100) &
      (eval(parse(text = variables[4])) < 140),
    DM := "Normal"]
  
  
  cat("\n[Completed!]\n")
  cat("\n[執行時間]\n")
  print(proc.time() - ptm)
  
  cat("\n[Result]\n")
  
  result <- table(df[["DM"]]) %>% addmargins() %>% as.data.frame()
  
  print(result)
  return(df)
  
}
