########
# Data pipeline
#  combining pest data
#
########

rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)


# Functions


names(dat)

State <- colnames(read_excel("data/raw/1979 Values.xlsx"))[[1]]
Year <- colnames(read_excel("data/raw/1979 Values.xlsx"))[[7]]

dat <- read_excel("data/raw/1979 Values.xlsx",skip=3) %>%
  rowid_to_column(var="id")

table1_end <- (dat %>% filter(Pest == "TOTAL"))$id  

table2_start <- (dat %>% filter(Pest == "Data Input"))$id  
table2_end <- (dat %>% filter(Pest == "% loss to other (chemical injury, weeds, diseases, etc.)"))$id  

table3_start <- (dat %>% filter(`Acres Treated` == "Yield and Magement Results"))$id  
table3_end <- (dat %>% filter(`Acres Treated` == "Applications by Ground (acres)"))$id  

table4_start <- (dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Economic Results"))$id +1 
table4_end <- (dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Total Losses + Costs"))$id  

table5_start <- (dat %>% filter(Pest == "Upland Cotton"))$id
table5_end <- (dat %>% filter(Pest == "Total Upland Cotton"))$id  

table6_start <- (dat %>% filter(Pest == "Non Upland Cotton"))$id
table6_end <- (dat %>% filter(Pest == "Total (all Cotton)"))$id  

table7_start <- ((dat %>% filter(`Acres Infested` == "% Acres"))$id)[2]
table7_end <- (dat %>% filter(Pest == "No. Acres with No foliar applications"))$id  



table1 <- dat[1:table1_end,] %>%
  drop_na(Pest)

table2 <- dat[table2_start:table2_end,] %>%
  drop_na(Pest) %>% select(2:3) %>%
  rename(Information = Pest, data = `Acres Infested`) %>%
  pivot_wider(names_from = "Information", values_from = "data")

table3 <- dat[table3_start:table3_end,] %>%
  select(`Acres Treated`,`% Acres Treated`) %>%
  drop_na(`Acres Treated`) %>%
  pivot_wider(names_from = `Acres Treated`, values_from = `% Acres Treated`)

table4 <- dat[table4_start:table4_end,] %>%
  select(`Cost of 1 insecticide app (including application cost)`,`% loss /acre infested`, `# of apps/ total acres`) %>%
  rename(ID = `Cost of 1 insecticide app (including application cost)`,Total = `% loss /acre infested`, Per_Acre = `# of apps/ total acres`) %>%
  drop_na(ID) %>%
  pivot_wider(
    names_from = ID,
    values_from = c(Total, Per_Acre))


table5 <- dat[table5_start:table5_end,] %>%
  row_to_names(1) %>% 
  clean_names() %>%
  select(-starts_with("na")) %>%
  select(-x54) %>%
  drop_na(upland_cotton)

table5_long <- table5 %>%
  mutate(upland_cotton = case_when(
    upland_cotton == "Organic" ~ "Organic_upland",
    TRUE ~ upland_cotton
  )) %>%
  pivot_wider(
    names_from = upland_cotton,
    values_from = c(-upland_cotton))

table6 <- dat[table6_start:table6_end,] %>% select(-id)
names(table6) <- names(table5)
table6 <- table6 %>% 
  clean_names() %>%
  mutate(upland_cotton = case_when(
    upland_cotton == "Organic" ~ "Organic_non_upland",
    TRUE ~ upland_cotton
  )) %>%
  select(-starts_with("na")) %>%
  pivot_wider(
    names_from = upland_cotton,
    values_from = c(-upland_cotton))

table7 <- dat[table7_start:table7_end,] %>%
  row_to_names(1) %>%
  clean_names() %>%
  select(-starts_with("na_")) %>%
  select(-starts_with("x")) %>%
  pivot_wider(
    names_from = na,
    values_from = c(-na))
  

test <- cbind(table1,table2,table3,table4,table5_long,table6,table7) %>% 
  fill(c(names(table2),names(table3),names(table4),names(table5_long),names(table6),names(table7))) %>% 
  as_tibble()




dups=which(duplicated(test))
testframe <- subset(testframe, select = -c(dups))

any(duplicated(names(test)))
length(which(duplicated(names(test))==TRUE))

test %>% select(175,180,185,190,195,200,205,210)


which( duplicated( names( test ) ) )
#



# Lets produce a for loop for the single sheet



?list.files




files <- list.files("data/raw", full.names = TRUE)
files2 <- gsub("\\~","",files)
files3 <- gsub("\\$","",files2)

files4 <- unique(gtools::mixedsort(files3))

test_files <- c("data/raw/2017 Values.xlsx","data/raw/2018 Values.xlsx")

dat_list <- list()
overall_dat_list <- list()

for (x in files4) {
  tryCatch({
  cat("processing", x, "\n")
  Sys.sleep(0.01)
  flush.console()

sheetcount <- length(excel_sheets(x))

for (i in 1:sheetcount){
  
  region <- (names(read_excel(x,sheet=i,.name_repair	="minimal"))[1])
  year <- names(read_excel(x,sheet=i,.name_repair	="minimal"))[7]
  
  loop_dat <- read_excel(x,sheet=i,skip=2) %>%
    rowid_to_column(var="id")
  
  table1_end <- (loop_dat %>% filter(Pest == "TOTAL" | Pest == "Total"))$id  
  
  table2_start <- (loop_dat %>% filter(Pest == "Data Input"))$id  
  table2_end <- (loop_dat %>% filter(Pest == "% loss to other (chemical injury, weeds, diseases, etc.)"))$id  

  table3_start <- (loop_dat %>% filter(`Acres Treated` == "Yield and Management Results" | `Acres Treated` == "Yield and Magement Results" | `Acres Treated` == "Yield and Ma0gement Results"))$id  
  table3_end <- (loop_dat %>% filter(`Acres Treated` == "Applications by Ground (acres)"))$id  
  
  table4_start <- (loop_dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Economic Results"))$id +1 
  table4_end <- (loop_dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Total Losses + Costs"))$id  
  
  table5_start <- (loop_dat %>% filter(Pest == "Upland Cotton"))$id
  table5_end <- (loop_dat %>% filter(Pest == "Total Upland Cotton"))$id  
  
  table6_start <- (loop_dat %>% filter(Pest == "Non Upland Cotton"))$id
  table6_end <- (loop_dat %>% filter(Pest == "Total (all Cotton)"))$id  
  
  table7_start <- ((loop_dat %>% filter(`Acres Infested` == "% Acres"))$id)[2]
  table7_end <- (loop_dat %>% filter(Pest == "No. Acres with No foliar applications"))$id  
  
  
  table1 <- loop_dat[1:table1_end,] %>%
    drop_na(Pest)
  
  table2 <- loop_dat[table2_start:table2_end,] %>%
    drop_na(Pest) %>% select(2:3) %>%
    rename(Information = Pest, data = `Acres Infested`) %>%
    pivot_wider(names_from = "Information", values_from = "data")
  
  table3 <- loop_dat[table3_start:table3_end,] %>%
    select(`Acres Treated`,`% Acres Treated`) %>%
    drop_na(`Acres Treated`) %>%
    pivot_wider(names_from = `Acres Treated`, values_from = `% Acres Treated`)
  
  #table4 <- loop_dat[table4_start:table4_end,] %>%
  #  select(`Cost of 1 insecticide app (including application cost)`,`% loss /acre infested`, `# of apps/ total acres`) %>%
  #  rename(ID = `Cost of 1 insecticide app (including application cost)`,Total = `% loss /acre infested`, Per_Acre = `# of apps/ total acres`) %>%
  #  drop_na(ID) %>%
  #  pivot_wider(
  #    names_from = ID,
  #    values_from = c(Total, Per_Acre))
  
  
  #table5 <- loop_dat[table5_start:table5_end,] %>%
  #  row_to_names(1) %>% 
  #  clean_names() %>%
  #  select(-starts_with("na")) %>%
  #  select(-1) %>%
  #  drop_na(upland_cotton)
  
  #table5_long <- table5 %>%
  #  mutate(upland_cotton = case_when(
  #    upland_cotton == "Organic" ~ "Organic_upland",
  #    TRUE ~ upland_cotton
  #  )) %>%
  #  pivot_wider(
  #    names_from = upland_cotton,
  #    values_from = c(-upland_cotton))
  
  #table6 <- loop_dat[table6_start:table6_end,] %>% select(-id)
  #names(table6) <- names(table5)
  #table6 <- table6 %>% 
  #  clean_names() %>%
  #  mutate(upland_cotton = case_when(
  #    upland_cotton == "Organic" ~ "Organic_non_upland",
  #    TRUE ~ upland_cotton
  #  )) %>%
  #  select(-starts_with("na")) %>%
  #  pivot_wider(
  #    names_from = upland_cotton,
  #    values_from = c(-upland_cotton))
  
  #table7 <- loop_dat[table7_start:table7_end,] %>%
  #  row_to_names(1) %>%
  #  clean_names() %>%
  #  select(-starts_with("na_")) %>%
  #  select(-starts_with("x")) %>%
  #  pivot_wider(
  #    names_from = na,
  #    values_from = c(-na))
  
  
  #dat_list[[i]] <- cbind(table1,table2,table3,table4,table5_long,table6,table7) %>% 
  #  fill(c(names(table2),names(table3),names(table4),names(table5_long),names(table6),names(table7))) %>%
  #  mutate( region = region,
  #          year2 = year) %>%
  #  select(Pest,`Acres Infested`,region,year2,`% Acres Infested`,`Total Acres`)
  dat_list[[i]] <- cbind(table1,table2,table3) %>% 
    select(Pest,`Acres Infested`,`% Acres Infested`,`Total Acres`) %>%
    mutate( region = region,
            year2 = year) %>% as_tibble()
  
}

overall_dat_list[[x]] <- dat_list
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

pest_pressure_data <- bind_rows(overall_dat_list, .id = "column_label")



pest_pressure_data <- pest_pressure_data %>%
  clean_names() %>%
  mutate(column_label = factor(column_label), pest = factor(pest), region = factor(region),
         year2 = as.integer(year2)) 

pest_pressure_data2 <- pest_pressure_data %>%
  mutate(pest = tolower(pest), pest = factor(gsub(" ", "_", pest)))

write.csv(pest_pressure_data2,"data/processed/Pest_damage_dat.csv")


### Testing loop



    
    sheetcount <- length(excel_sheets("data/raw/2017 Values.xlsx"))
    x <- "data/raw/2017 Values.xlsx"
    dat_list <- list()
    for (i in 1:sheetcount){
      tryCatch({
      cat("processing", i,"of",sheetcount, "\n")
      Sys.sleep(0.01)
      flush.console()
      
      
      region <- paste0(names(read_excel(x,sheet=i,.name_repair	="minimal"))[1],"_",names(read_excel(x,sheet=i,.name_repair	="minimal"))[2])
      year <- names(read_excel(x,sheet=i,.name_repair	="minimal"))[7]
      
      loop_dat <- read_excel(x,sheet=17,skip=2) %>%
        rowid_to_column(var="id")
      
      table1_end <- (loop_dat %>% filter(Pest == "TOTAL" | Pest == "Total"))$id  
      
      table2_start <- (loop_dat %>% filter(Pest == "Data Input"))$id  
      table2_end <- (loop_dat %>% filter(Pest == "% loss to other (chemical injury, weeds, diseases, etc.)"))$id  
      
      table3_start <- (loop_dat %>% filter(`Acres Treated` == "Yield and Management Results" | `Acres Treated` == "Yield and Magement Results" | `Acres Treated` == "Yield and Ma0gement Results"))$id  
      table3_end <- (loop_dat %>% filter(`Acres Treated` == "Applications by Ground (acres)"))$id  
      
      table4_start <- (loop_dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Economic Results"))$id +1 
      table4_end <- (loop_dat %>% filter(`Cost of 1 insecticide app (including application cost)` == "Total Losses + Costs"))$id  
      
      table5_start <- (loop_dat %>% filter(Pest == "Upland Cotton"))$id
      table5_end <- (loop_dat %>% filter(Pest == "Total Upland Cotton"))$id  
      
      table6_start <- (loop_dat %>% filter(Pest == "Non Upland Cotton"))$id
      table6_end <- (loop_dat %>% filter(Pest == "Total (all Cotton)"))$id  
      
      table7_start <- ((loop_dat %>% filter(`Acres Infested` == "% Acres"))$id)[2]
      table7_end <- (loop_dat %>% filter(Pest == "No. Acres with No foliar applications"))$id  
      
      
      table1 <- loop_dat[1:table1_end,] %>%
        drop_na(Pest)
      
      table2 <- loop_dat[table2_start:table2_end,] %>%
        drop_na(Pest) %>% select(2:3) %>%
        rename(Information = Pest, data = `Acres Infested`) %>%
        pivot_wider(names_from = "Information", values_from = "data")
      
      table3 <- loop_dat[table3_start:table3_end,] %>%
        select(`Acres Treated`,`% Acres Treated`) %>%
        drop_na(`Acres Treated`) %>%
        pivot_wider(names_from = `Acres Treated`, values_from = `% Acres Treated`)
      
      table4 <- loop_dat[table4_start:table4_end,] %>%
        select(`Cost of 1 insecticide app (including application cost)`,`% loss /acre infested`, `# of apps/ total acres`) %>%
        rename(ID = `Cost of 1 insecticide app (including application cost)`,Total = `% loss /acre infested`, Per_Acre = `# of apps/ total acres`) %>%
        drop_na(ID) %>%
        pivot_wider(
          names_from = ID,
          values_from = c(Total, Per_Acre))
      
      
      table5 <- loop_dat[table5_start:table5_end,] %>%
        row_to_names(1) %>% 
        clean_names() %>%
        select(-starts_with("na")) %>%
        select(-1) %>%
        drop_na(upland_cotton)
      
      table5_long <- table5 %>%
        mutate(upland_cotton = case_when(
          upland_cotton == "Organic" ~ "Organic_upland",
          TRUE ~ upland_cotton
        )) %>%
        pivot_wider(
          names_from = upland_cotton,
          values_from = c(-upland_cotton))
      
      table6 <- loop_dat[table6_start:table6_end,] %>% select(-id)
      names(table6) <- names(table5)
      table6 <- table6 %>% 
        clean_names() %>%
        mutate(upland_cotton = case_when(
          upland_cotton == "Organic" ~ "Organic_non_upland",
          TRUE ~ upland_cotton
        )) %>%
        select(-starts_with("na")) %>%
        pivot_wider(
          names_from = upland_cotton,
          values_from = c(-upland_cotton))
      
      table7 <- loop_dat[table7_start:table7_end,] %>%
        row_to_names(1) %>%
        clean_names() %>%
        select(-starts_with("na_")) %>%
        select(-starts_with("x")) %>%
        pivot_wider(
          names_from = na,
          values_from = c(-na))
      
      
      dat_list[[i]] <- cbind(table1,table2,table3) %>% 
        select(Pest,`Acres Infested`,`% Acres Infested`,`Total Acres`) %>%
        mutate( region = region,
                year2 = year) 
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    
   names(cbind(table1,table2)) 
which( duplicated( names( cbind(table1,table2,table3,table4,table5_long,table6,table7) ) ) )

cbind(table1,table2,table3,table4,table5_long,table6,table7) %>% select(182,187,192,202,207,212,217)
