########
# Data pipeline
#  just getting out
#   total acres, yield and such
########

rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)

`%ni%` <- Negate(`%in%`)
# Functions


State <- colnames(read_excel("data/raw/1995 Values.xlsx"))[[1]]
Year <- colnames(read_excel("data/raw/1995 Values.xlsx"))[[1]]
sheet_name <- (excel_sheets("data/raw/1995 Values.xlsx")[1])

dat <- read_excel("data/raw/1995 Values.xlsx",skip=3) %>%
  rowid_to_column(var="id")




table2_start <- (dat %>% filter(Pest == "Data Input"))$id  
table2_end <- (dat %>% filter(Pest == "% loss to other (chemical injury, weeds, diseases, etc.)"))$id  

table3_start <- (dat %>% filter(`Acres Treated` == "Yield and Magement Results" |`Acres Treated` ==  "Yield and Management Results"))$id  
table3_end <- (dat %>% filter(`Acres Treated` == "Applications by Ground (acres)"))$id  

table1 <-dat[table2_start:table2_end,] %>%
  drop_na(Pest) %>% select(2:3) %>%
  rename(Information = Pest, data = `Acres Infested`) %>%
  pivot_wider(names_from = "Information", values_from = "data") %>%
  select(State,Region,Year,'Total Acres (Upland)','Yield / Acre (Upland)') %

table2 <-dat[table3_start:table3_end,] %>%
  select(`Acres Treated`,`% Acres Treated`) %>%
  drop_na(`Acres Treated`) %>%
  pivot_wider(names_from = `Acres Treated`, values_from = `% Acres Treated`) %>%
  select(`Total Acres`)

test <- cbind(table1,table2) %>%
  mutate(Sheet_name = sheet_name, Sheet_Year = Year)


# Lets produce a for loop for the single excel file



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
      
      
      State <- colnames(read_excel(x,sheet=i,.name_repair	="minimal"))[[1]]
      Year <- colnames(read_excel(x,sheet=i,.name_repair	="minimal"))[[7]]
      sheet_name <- (excel_sheets(x)[i])
      
      dat <- read_excel(x,sheet=i,skip=3) %>%
        rowid_to_column(var="id")
  
      table1_end <- (dat %>% filter(Pest == "TOTAL" | Pest == "Total"))$id  
      
      table2_start <- (dat %>% filter(Pest == "Data Input"))$id  
      table2_end <- (dat %>% filter(Pest == "% loss to other (chemical injury, weeds, diseases, etc.)"))$id  
      
      table3_start <- (dat %>% filter(`Acres Treated` == "Yield and Magement Results" |`Acres Treated` ==  "Yield and Management Results" |`Acres Treated` == "Yield and Ma0gement Results"))$id  
      table3_end <- (dat %>% filter(`Acres Treated` == "Applications by Ground (acres)"))$id  
      
      table3 <- dat[1:table1_end,] %>%
        drop_na(Pest)  %>%
        select(any_of(c("Pest","Acres Infested","Acres Treated")),starts_with("# of apps")) 
        
      
      table1 <-dat[table2_start:table2_end,] %>%
        drop_na(Pest) %>% select(2:3) %>%
        rename(Information = Pest, data = `Acres Infested`) %>%
        pivot_wider(names_from = "Information", values_from = "data") %>%
        select(any_of(c("State","Region","Year")),starts_with("Total Acres"),starts_with("Yield")) 
      
      table2 <-dat[table3_start:table3_end,] %>%
        select(`Acres Treated`,`% Acres Treated`) %>%
        drop_na(`Acres Treated`) %>%
        pivot_wider(names_from = `Acres Treated`, values_from = `% Acres Treated`) %>%
        select(`Total Acres`)
      
      dat_list[[i]]  <- cbind(table1,table2,table3) %>%
        mutate(Sheet_name = sheet_name, Sheet_Year = Year) %>% as_tibble()
      
      
      #if (dat_list[i]$State == "2013") {  # replaces value in current row if TRUE and TRUE
      #  error_sheets[[i]] <- x}
      
      #if (dat_list[i]$Region == NA | dat_list[i]$Region == "2013") {  # replaces value in current row if TRUE and TRUE
      #  error_region[[i]] <- x}
      
      #if (dat_list[i]$Year == NA) {  # replaces value in current row if TRUE and TRUE
      #  error_year[[i]] <- x}
      
    }
    
    overall_dat_list[[x]] <- dat_list
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

BT_shit <- c("nonBT","BT","non-BT","bt","nonbt","non-bt","Bt", "Pima", "pima","Conventional","conventional")


pest_pressure_data <- bind_rows(overall_dat_list, .id = "column_label")  %>% 
  filter(!grepl(paste(BT_shit, collapse="|"), Sheet_name),
         Pest != "TOTAL") %>%
  mutate(Region = case_when(
          State == "2013" ~ "Coast & Blacklands",
          TRUE ~ Region), 
        Year = case_when(
          State == "2013" ~ "2013",
          TRUE ~ Year),
        State = case_when(
          State == "2013" ~ "Texas",
          TRUE ~ State))


unique(levels(factor(pest_pressure_data$State)))
unique(levels(factor(pest_pressure_data2$Sheet_name)))

AZ_fix <- c("Arizo","Arizo0")
LA_fix <- c("Louisia","Louisia0")
NC_fix <- c("North Caroli0","North Caroli")
SC_fix <- c("South Caroli0","South Caroli","Sout Carolina")
VA_fix <- c("Virgina")
TX_fix <- c("Texs","texas")

remove_regions <- c("West","US","Central","Midsouth","Sheet41","Sheet34","Southeast")

pest_pressure_data2 <- pest_pressure_data %>%
  mutate(State = case_when(
    State %in% AZ_fix ~ "Arizona",
    State %in% LA_fix ~ "Louisiana",
    State %in% NC_fix ~ "North Carolina",
    State %in% SC_fix ~ "South Carolina",
    State %in% VA_fix ~ "Virginia",
    State %in% TX_fix ~ "Texas",
    TRUE ~ State
  ),Year = case_when(
    Year == "184" ~ "1984",
    Year == "0" | Year == "0.0" ~ "2019",
    TRUE ~ Year
  )) %>% 
  mutate(across(where(is.character), str_squish)) %>%
  filter(Year > 1970)


names <- state.name

view(pest_pressure_data2 %>% filter(Year < 1979))

to_filter <- pest_pressure_data2 %>% 
       group_by(Year,State) %>% 
      summarize(n = length(unique(levels(factor(Sheet_name))))) %>%
       mutate(Year = as.integer(as.numeric(Year)),
              year_state = paste0(Year,"_",State)) %>%
  filter(n > 1) %>% droplevels()

to_filter <- unique(to_filter$year_state)

no_filtering_needed <- pest_pressure_data2 %>%
  mutate(Year = as.integer(as.numeric(Year)),
         year_state = paste0(Year,"_",State)) %>%
  filter(year_state %ni% to_filter)


filtering_needed <- pest_pressure_data2 %>%
  mutate(Year = as.integer(as.numeric(Year)),
         year_state = paste0(Year,"_",State)) %>%
  filter(year_state %in% to_filter,
         Sheet_name %ni% names)

filtered_dat <- rbind(no_filtering_needed,filtering_needed) %>%
  mutate(across(`Total Acres (Upland)`:`Total Acres`,as.numeric),
         across(`Acres Infested`:`# of apps /acres treated`,as.numeric),
         `Total Acres (Pima)` = as.numeric(`Total Acres (Pima)`),
         State = factor(State),
         Region = factor(Region),
         Sheet_name = factor(Sheet_name),
         Sheet_Year = as.integer(Sheet_Year),
         column_label = as.integer(column_label))


view(filtered_dat %>% group_by(State,Year) %>%
  summarise(sum = sum(`Total Acres`,na.rm=TRUE)))



cleaned <- filtered_dat %>% group_by(State,Year) %>%
  summarize(Total_acres=sum(`Total Acres`),
            Yield = sum(`Yield / Acre (Upland)`),
            )

### Now combining with the dataset produced by anders

dat <- read_csv("data/processed/CottonLosses_03-12-22_final.csv") %>%
  clean_names() %>%
  mutate(across(where(is.character), ~na_if(., ".")),
         across(acres_infested:loss_cost_acre, ~gsub("\\%", "", .) %>% as.numeric),
         across(state:pest,  as.factor)) %>%
  filter(subset_txregions == "no") %>% droplevels() %>%
  select(2:4,6:9,11,13) %>%
  mutate(state = str_to_sentence(state)) %>%
  rename(Year = year,State=state) %>%
  left_join(cleaned,by=c("State","Year"))

write.csv(dat,file="data/processed/clean_data_March14.csv")

ggplot(dat,aes(x=Year,y=acres_infested,color=group)) + geom_smooth()
