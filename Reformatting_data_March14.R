########
# Data pipeline
#  just getting out
#   total acres, yield and such
########

rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
library(ggridges)

# Functions
`%ni%` <- Negate(`%in%`)


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
      
      State_top1 <- colnames(read_excel(x,sheet=i,.name_repair	="minimal"))[[1]]
      Year_top <- colnames(read_excel(x,sheet=i,.name_repair	="minimal"))[[7]]
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
        mutate(Sheet_name = sheet_name, Sheet_Year = Year_top, State_top = State_top1) %>% as_tibble()
      
      #table1_end <- (dat %>% filter(Pest == "TOTAL" | Pest == "Total"))$id  
      
      #table3 <- dat[1:table1_end,] %>%
      #  drop_na(Pest)  %>%
      #  select(any_of(c("Pest","Acres Infested","Acres Treated")),starts_with("# of apps")) 
        
      
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

BT_shit <- c("nonBT","BT","non-BT","bt","nonbt","non-bt","Bt", "Pima", "pima","Conventional","conventional","nonTB","Conv", "Bollgard")


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

names <- state.name
NA_Dat <- pest_pressure_data[!complete.cases(pest_pressure_data$State), ] %>%
  filter(State_top %in% names) %>%
  mutate(State = State_top, Year = Sheet_Year)


pest_pressure_data.5 <- pest_pressure_data %>%
  drop_na(State) %>%
  rbind(NA_Dat)

unique(levels(factor(pest_pressure_data.5$State)))
unique(levels(factor(pest_pressure_data.5$Sheet_state_top)))

AZ_fix <- c("Arizo","Arizo0")
LA_fix <- c("Louisia","Louisia0")
NC_fix <- c("North Caroli0","North Caroli")
SC_fix <- c("South Caroli0","South Caroli","Sout Carolina")
VA_fix <- c("Virgina")
TX_fix <- c("Texs","texas")

remove_regions <- c("West","US","Central","Midsouth","Sheet41","Sheet34","Southeast")

pest_pressure_data2 <- pest_pressure_data.5 %>%
  mutate(State = case_when(
    State %in% AZ_fix ~ "Arizona",
    State %in% LA_fix ~ "Louisiana",
    State %in% NC_fix ~ "North Carolina",
    State %in% SC_fix ~ "South Carolina",
    State %in% VA_fix ~ "Virginia",
    State %in% TX_fix ~ "Texas",
    State_top %in% AZ_fix ~ "Arizona",
    State_top %in% LA_fix ~ "Louisiana",
    State_top %in% NC_fix ~ "North Carolina",
    State_top %in% SC_fix ~ "South Carolina",
    State_top %in% VA_fix ~ "Virginia",
    State_top %in% TX_fix ~ "Texas",
    State_top == "Tennessse" ~ "Tennessee",
    State == "Tennessse" ~ "Tennessee",
    TRUE ~ State
  ),Year = case_when(
    Year == "184" ~ "1984",
    Year == "0" | Year == "0.0" ~ "2019",
    TRUE ~ Year
  )) %>% 
  mutate(across(where(is.character), str_squish)) %>%
  filter(Year > 1970, State %ni% remove_regions) %>% 
  clean_names() %>%
  mutate(across(total_acres_upland:total_acres,as.numeric),
         across(acres_infested:number_of_apps_total_acres,as.numeric),
         #across(sheet_name:state_top,as.factor),
                total_acres_pima = as.numeric(total_acres_pima),
          state = as.character(state))


view(pest_pressure_data2 %>% filter(year == 2009))

pest_pressure_data2 %>% filter(state == "Texas" & sheet_name == "Virginia")



pest_pressure_data2 %>% mutate(state = case_when(
  state == "Texas" & sheet_name == "Virginia" ~ "Virginia",
  TRUE ~ (state)
))


pest_pressure_data3 <- pest_pressure_data2 %>% mutate(state = case_when(
  state == "Texas" & sheet_name == "Virginia" ~ "Virginia",
  TRUE ~ state
))  %>% mutate(across(sheet_name:state_top,as.factor),
                state = as.factor(state))

summary(as.numeric(pest_pressure_data2$total_acres))


#something is weird about texas. Lets look at it



TX_dat <- pest_pressure_data3 %>% filter(state=="Texas") %>%
  mutate(year = as.integer(year), sheet_year = as.integer(sheet_year))


# Texas in 1987 didnt have a state level sum. Creating it now...


TX_1987 <- TX_dat %>% filter(year == 1987) %>% 
  group_by(state,year,pest) %>%
  summarise(across(total_acres_upland:total_acres, sum),
            across(acres_infested:number_of_apps_total_acres, sum),
            column_label = first(column_label),
            region = first(region),
            across(sheet_name:state_top, first),
            total_acres_pima = sum(total_acres_pima))
         

   
TX_state_level <- TX_dat %>%
  filter(year != 1987) %>%
  filter(sheet_name == "Texas") %>%
  rbind(TX_1987)



TX_state_level %>% 
  ggplot(aes(x=(year),y=total_acres_upland)) +
  geom_point() + geom_line() +
  scale_x_continuous(limits=c(1983,2020),breaks = seq(1983,2020,1))



# okay now filtering out all the "state' tabs from regional tabs for all other states except texas

other_dat <- pest_pressure_data3 %>% filter(state !="Texas") 





to_filter <- other_dat %>% 
       group_by(year,state) %>% 
      summarize(n = length(unique(levels(factor(sheet_name))))) %>%
       mutate(year = as.integer(as.numeric(year)),
              year_state = paste0(year,"_",state)) %>%
  filter(n > 1) %>% droplevels()

unique(levels(factor(other_dat$state)))


to_filter <- unique(to_filter$year_state)

no_filtering_needed <- other_dat %>%
  mutate(year = as.integer(as.numeric(year)),
         year_state = paste0(year,"_",state)) %>%
  filter(year_state %ni% to_filter) %>% droplevels()


unique(levels(factor(filtering_needed$Sheet_name)))



filtering_needed <- pest_pressure_data3 %>%
  mutate(year = as.integer(as.numeric(year)),
         year_state = paste0(year,"_",state)) %>%
  filter(year_state %in% to_filter,
         sheet_name %ni% names) 


names(TX_state_level)
names(filtered_dat)



filtered_dat <- rbind(no_filtering_needed,filtering_needed) %>%
 # rename(dheet_year = sheet_year) %>%
  select(!c(year_state)) %>%
  rbind(TX_state_level) %>%
  mutate(state = factor(state),
         region = factor(region),
         sheet_name = factor(sheet_name),
         dheet_year = as.integer(sheet_year),
         column_label = as.integer(column_label)) %>% droplevels()


# There are sheets without reported total acres. As such, im going to give them NASS data

NASS <- read_csv("data/NASS_dat.csv") %>%
  select(Year,State,Value) %>%
  mutate(year = Year, state = str_to_title(State)) %>%
  select(3:5)




NA_Dat <- filtered_dat[!complete.cases(filtered_dat$total_acres), ] %>%
  left_join(NASS,by=c("year","state")) %>%
  mutate(region = state, total_acres_upland = Value) %>%
  select(state,year,total_acres_upland) %>%
  distinct()

TA_nas <- NA_Dat %>% mutate(state_year = paste0(state,"_",year))
TA_nas <- unique(factor(TA_nas$state_year))

#For the regions without reported total acres, I am summarizing to the state level

state_level <- filtered_dat %>% mutate(state_year = paste0(state,"_",year)) %>%
  filter(state_year %in% TA_nas) %>% group_by(state,year,pest) %>%
  summarise(across(total_acres_upland:total_acres, sum),
            across(acres_infested:number_of_apps_total_acres, sum),
            column_label = first(column_label),
            region = first(region),
            across(sheet_name:state_top, first),
            total_acres_pima = sum(total_acres_pima)) %>%
  left_join(NASS, by=c("state","year")) %>%
  mutate(sheet_name = state, total_acres = Value) %>%
  select(!Value)


summary(state_level)


#Alright I summed up the regions to state and added the acres harvested from NASS lets recombine with the overall dataset

filtered_dat <- filtered_dat %>%
  drop_na(total_acres) %>%
  select(!dheet_year) %>%
  rbind(state_level)



#test <- (filtered_dat %>% group_by(state,year) %>%
#  summarise(sum = mean(total_acres,na.rm=TRUE)))

#summary(test)

#Regions are still wonky. lets rename them. For example Alabama C most likely means Alabama Central

levels(unique(filtered_dat$sheet_name))


filtered_dat2 <- filtered_dat %>%
  mutate(
    sheet_name = as.character(sheet_name),
    sheet_name = case_when(
    sheet_name == "Alabama C" ~ "Alabama Central" ,
    sheet_name %in% c("Alabama N","North AL","North Alabama") ~ "Alabama North",
    sheet_name %in% c("Alabama S","South AL","South Alabama") ~ "Alabama South",
    sheet_name %in%  c("Arkansas N","North Arkansas") ~ "Arkansas North",
    sheet_name %in% c("Arkansas NE", "NE Arkansas","Northeast AR","Northeast Arkansas") ~ "Arkansas Northeast" ,
    sheet_name %in%  c("Arkansas S","South Arkansas","Southern Arkansas") ~ "Arkansas South" ,
    sheet_name %in% c("Arkansas SE","SE Arkansas","Southeast AR","Southeast Arkansas") ~ "Arkansas Southeast",
    sheet_name == "Califonia" ~ "California" ,
    sheet_name == "California imp v" ~ "California Imperial Valley" ,
    sheet_name %in% c("California Sac","Sacramento") ~ "California Sacramento Valley" ,
    sheet_name %in% c("California San Joaquin","California SJV","San Joaquin","San Joaquin Valley California") ~ "California San Joaquin Valley" ,
    sheet_name == "California-Upland" ~ "California Upland" ,
    sheet_name == "Central Alabama" ~ "Alabama Central" ,
    sheet_name == "Georigia" ~ "Georgia" ,
    sheet_name %in% c("Imperial Valley","Imperial Valley California") ~ "California Imperial Valley",
    sheet_name == "Kansas SW" ~ "Kansas Southwest",
    sheet_name == "Mississppi" ~ "Mississippi",
    sheet_name == "MS Delta" ~ "Mississippi Delta",
    sheet_name == "MS Hills" ~ "Mississippi Hills",
    sheet_name == "Virgina" ~ "Virginia",
    sheet_name == "Texas Area 1" ~ "Texas",
    TRUE ~ sheet_name ))



test <- filtered_dat %>% 
  group_by(State,Year) %>%
  summarise(n=n())

filtered_dat2 %>% filter(sheet_name == "Texas Area 1")
filtered_dat %>% filter(state == "Arizona" & year == 2011)
pest_pressure_data %>% filter(State == "Arizona")

unique(levels(factor(filtered_dat2$sheet_name)))



#alright lets visualize total acres to ensure that everything looks fine

cleaned <- filtered_dat2 %>% 
  group_by(state,year) %>%
  summarize(total_acres=mean(total_acres,na.rm=TRUE),
            yield = mean(yield_acre_upland,na.rm=TRUE),
            yield_potenital = mean(yield_potential_lb_acre,na.rm=TRUE),
            )

summary(cleaned$yield_potenital)

ggplot(cleaned,aes(x=year,y=yield_potenital)) + geom_point()



cleaned %>%
  #filter(State == "Texas") %>%
ggplot(aes(x=(year),y=total_acres,color=state)) + geom_point() + geom_line() +
  scale_x_continuous(limits=c(1983,2020),breaks = seq(1983,2020,2)) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5", "#048ad1")) +
  facet_wrap(~state,scales="free")

cleaned %>%
  #filter(State == "Texas") %>%
  ggplot(aes(x=(year),y=yield_potenital,color=state)) + geom_point() + geom_line() +
  scale_x_continuous(limits=c(1983,2020),breaks = seq(1983,2020,2)) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5", "#048ad1"))



cleaned %>%
  filter(state != "Texas") %>%
  ggplot(aes(x=(year),y=total_acres,color=state)) + geom_point() + geom_line() +
  scale_x_continuous(limits=c(1983,2020),breaks = seq(1983,2020,2)) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5")) +
  facet_wrap(~state)

cleaned %>% group_by(state,year) %>%
  summarize(n=n()) %>% pivot_wider(names_from = year,values_from = n)


#Okay I think there are a few data entry errors. Specifically:
# Alabama 1986
# Kansas early 2000s
# California 1980s
# Virginia 1980s

### Lets compare it with NASS cotton acres harvested


states_sum <- cleaned %>% group_by(state,year) %>%
  summarize(total_acres = sum(total_acres))


NASS_comparison <- states_sum %>% full_join(NASS,by=c("year","state")) %>%
  mutate(difference = total_acres - Value, abs_diff = abs(difference))


NASS_comparison %>% filter(abs_diff > 1000000)


NASS_comparison %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(diff = total_acres - lag(total_acres, default = first(total_acres))) %>%
  arrange(desc(diff)) %>%
  select(1:3,diff)





t1 <- ggplot(NASS_comparison,aes(x=Value,y=total_acres,color=state)) + geom_point() +
  geom_abline(slope=1) +
  coord_equal() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5", "#048ad1")) +
  ggpubr::theme_pubr() + ylab("Acres reported in pest damage dataset") + xlab("NASS acres harvested") + theme(legend.position = "bottom")

t2 <- NASS_comparison %>% filter(state != "Texas") %>%
  ggplot(aes(x=Value,y=total_acres,color=state)) + geom_point() +
  geom_abline(slope=1) +
  coord_equal() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5", "#048ad1")) +
  ggpubr::theme_pubr() + ylab("") + xlab("NASS acres harvested") + theme(legend.position="none")

t1 + t2 


summary(NASS_comparison$difference)


# Okay for now, I am just going to remove them from the dataset

filtered_dat2 %>% filter(state == "Alabama") %>% filter(total_acres >500000)
filtered_dat2 %>% filter(state == "Kansas") %>% filter(total_acres >500000)
filtered_dat2 %>% filter(state == "California") %>% filter(year < 1990 & total_acres < 500000)
filtered_dat2 %>% filter(state == "Virginia") %>% filter(year == 1988)
filtered_dat2 %>% filter(state == "Florida") %>% filter(total_acres > 500000)


filtered_dat3 <- filtered_dat2 %>% left_join(NASS,by=c("state","year")) %>%
  mutate(total_acres = case_when(
    state == "Alabama" & year == 1986 ~ Value,
    state == "Kansas" & year == 2007 ~ Value,
    state == "California" & year == 1987 ~ Value,
    state == "Virginia" & year == 1988 ~ Value,
    state == "Florida" & year == 2009 ~ Value,
    TRUE ~ total_acres
  ))
 




cleaned <- filtered_dat3 %>% 
  group_by(state,year) %>%
  summarize(total_acres=mean(total_acres,na.rm=TRUE),
            yield = mean(yield_acre_upland,na.rm=TRUE),
            yield_potenital = mean(yield_potential_lb_acre,na.rm=TRUE),
  )

summary(cleaned$yield_potenital)

ggplot(cleaned,aes(x=year,y=yield_potenital)) + geom_point()



cleaned %>%
  #filter(state == "Alabama") %>%
  ggplot(aes(x=(year),y=total_acres,color=state)) + geom_point() + geom_line() +
  scale_x_continuous(limits=c(1983,2020),breaks = seq(1983,2020,4)) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#21f0b6", "#982a25", "#82ee2f", "#b131ae", "#378811", "#9620fc", "#bbcf7a", "#4346ab", "#f4a95c", "#214a65", "#e8a8b0", "#007961", "#f85b57", "#75d5e1", "#683c00", "#ea85f5", "#048ad1")) +
  facet_wrap(~state,scales = "free")

cleaned %>% group_by(state,year) %>%
  summarize(n=n()) %>% pivot_wider(names_from = year,values_from = n)





# Alright this is where I stop. Next I need to clean up the pest names. drop things. etc.
# I am going to write out the csv and manually enter the pest names as there are a lot

length(unique(filtered_dat3$pest))

pests <- as.data.frame(unique(filtered_dat3$pest))


#anders <- read_csv("data/processed/clean_data_March14.csv") %>%
#  select(group,order,pest) %>% distinct()


#write.csv(anders,"data/processed/anders.csv")
#write.csv(pests,"data/processed/pest_list.csv")

#Alright now that is done, lets clean up the data

pests <- read_csv("data/processed/pest_list.csv") %>%
  distinct()


filtered_dat4 <- filtered_dat3 %>%
  left_join(pests,by=c("pest" = "old_list")) %>%
  filter(order != "drop") %>%
  select(!pest) %>%
  rename(pest = pest.y, NASS_acres_harvest = Value) %>%
  drop_na(acres_infested) %>%
  remove_empty(which = "cols") 

length(unique(filtered_dat4$group))
unique(filtered_dat4$group)

length(unique(filtered_dat4$order))
unique(filtered_dat4$order)

length(unique(filtered_dat4$pest))
unique(filtered_dat4$pest)

summary(filtered_dat4)


names(filtered_dat4)



# pest population data peek

filtered_dat4 %>%
  group_by(state,group) %>%
  select(10:13) %>%
  mutate(acres_infested_log = log1p(acres_infested), acres_treated_log = log1p(acres_treated), number_of_apps_acres_treated_log = log1p(number_of_apps_acres_treated),number_of_apps_total_acres_log = log1p(number_of_apps_total_acres)) %>%
  pivot_longer(cols=c(3:10),names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=value,y=group)) +
  geom_density_ridges(alpha = 0.7) + facet_wrap(~variable,scales="free") +
  scale_x_continuous(limits=c(0,NA))

# state growing data peek


filtered_dat4 %>%
  group_by(state) %>%
  select_if(is.numeric) %>%
  select(!year) %>%
  mutate(total_acres_upland_log = log1p(total_acres_upland), yield_acre_upland_log = log1p(yield_acre_upland), yield_potential_lb_acre_log = log1p(yield_potential_lb_acre),yield_acre_pima_log = log1p(yield_acre_pima), total_acres_log = log1p(total_acres)) %>%
  pivot_longer(cols=c(3:7,13:16),names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=value,y=state)) +
  geom_density_ridges(alpha = 0.7) + facet_wrap(~variable,scales="free") +
  scale_x_continuous(limits=c(0,NA))







#everything looks OKAY. not great. as there are still SOME errors. But i think largely I have found most of them without getting to far into the weeds. Lets save this csv and go to modeling.


write.csv((filtered_dat4 %>% select(!c(column_label,state_top,sheet_year))),file="data/processed/cleaned_ag_pest_data_March18.csv")




