####
# Some quick plotting
#
####
rm(list=ls())

library(tidyverse)
library(ggridges)
library(janitor)
library(mgcv)
library(DHARMa)



names(dat)
dat <- read_csv("data/processed/cleaned_ag_pest_data_March18.csv") %>%
  clean_names() %>%
  select(!c(x1,x2)) %>%
  mutate(across(where(is.character), as.factor),
         toxin = factor(case_when(
           year < 1996 ~ "pre_BT",
           year >= 1996 & year <= 2006 ~ "Transition",
           year > 2006 ~ "BT_era"
         ))) 
  
ggplot(dat,aes(x=percent_acres_infested)) + geom_histogram()

summary(dat)

unique(levels(dat$state))
unique(levels(dat$region))
unique(levels(dat$group))
unique(levels(dat$order))
unique(levels(dat$pest))


# Just some density plots look at how pest groups are divided between the states

dat %>% 
  select(state,group,acres_infested) %>%
  ggplot(aes(x = acres_infested, y = group)) +
  geom_density_ridges_gradient() +
  facet_wrap(~state,scales="free") +
  scale_x_continuous(limits=c(0,NA))

dat %>% 
  select(state,acres_infested,toxin) %>%
  ggplot(aes(x = acres_infested, y = toxin)) +
  geom_density_ridges_gradient() +
  facet_wrap(~state,scales="free") +
  scale_x_continuous(limits=c(0,NA))

dat %>% 
  select(group,acres_infested,toxin) %>%
  ggplot(aes(x = acres_infested, y = toxin)) +
  geom_density_ridges_gradient() +
  facet_wrap(~group,scales="free") +
  scale_x_continuous(limits=c(0,NA))


dat %>% 
  select(state,group,acres_infested,toxin) %>%
  ggplot(aes(x = acres_infested, y = toxin)) +
  geom_density_ridges_gradient() +
  facet_grid(state~group,scales="free") +
  scale_x_continuous(limits=c(0,NA))




# Now doing some exploratory plotting

dat %>%
  select(state,year,order,acres_infested) %>%
  ggplot(aes(x=year,y=acres_infested,color=order)) +
  geom_smooth() +
  facet_wrap(~state,scales="free") +
  ggpubr::theme_pubr()


dat %>%
  select(toxin,year,state,total_acres) %>%
  group_by(state,year,toxin) %>%
  summarize(mean=mean(total_acres)) %>%
  ggplot(aes(x=(year),y=mean,fill=toxin)) +
  geom_col()  +
  geom_vline(xintercept = 1997) +
  facet_wrap(~state,scales="free") +
  ggpubr::theme_pubr()




major_pests <- (dat %>%
  group_by(pest) %>%
  summarize(n=n()) %>%
  arrange(desc(n))) %>%
  filter(n >= 1000) %>% droplevels()

names <- unique(levels(major_pests$pest))
names <- c("")


#just some quick modeling


dat <- dat %>%
  mutate(percent_adj = (percent_acres_infested*(33712-1)+0.5)/33712,
         BT_group = factor(paste0(toxin,"_",group)),
         acres_infested_int = (acres_infested)+1)

unique(dat$BT_group)
?bam


view(dat %>%
  filter(year  >= 1994, group == "boll weevil",state == "Texas") %>% 
  arrange(year))

unique(tx_boll$sheet_name)




dat %>%
  filter(group == "boll weevil",state == "Texas") %>% 
  ggplot(aes(x=as.integer(year),y=acres_infested,color=group)) + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1980,2021,by=4)) +
  geom_smooth(method="gam") +
  geom_point() +
  ggpubr::theme_pubr() +
  ylab("Acres infested") + 
  xlab("Year")
  
mod <- bam(acres_infested   ~
             #te(year,group,bs="fs",m=2,k=20) +
             te(year,state,group,bs=c("tp","re","re"),m=2,k=20) +
             s(region,state,bs="re") + 
             s(group,bs="re") +
             s(total_acres,k=25),
           select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)

aov_mod <- aov(percent_adj~BT_group,data=dat)
summary(aov_mod)
TukeyHSD(aov_mod)


mod_by <- bam(percent_adj  ~
             te(year,by=group,bs="tp",k=20) +
             s(region,state,bs="re") + 
             s(group,bs="re") +
             s(total_acres,k=25),
           select=TRUE,family=betar(),data=dat,discrete = TRUE,nthreads = 23)


mod_by <- bam(acres_infested ~ te(year,by=group,bs="tp",m=2,k=20)  + s(state,bs="re") + s(total_acres,k=20),select=TRUE,family=scat(),
              data=dat,discrete = TRUE,nthreads = 8)

AIC(mod,mod_by)

test <- gratia::smooth_estimates(mod,type="link")

summary(mod)
k.check(mod)
sim_resid <- simulateResiduals(mod_by)
plot(sim_resid)


output <- gratia::smooth_estimates(mod)

b0 <- coef((mod))[1]

test <- gratia::smooth_estimates((mod))

test$adj_est <- test$est 

group_year <- test %>%
  filter(smooth == "te(year,state,group)") %>%
  ggplot(aes(x=year,group=group)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=group),alpha=.1) +
  geom_line(aes(y = adj_est, color=group),size=1.5) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#58b5e1", "#b11478", "#9bea30", "#4224bf", "#749d3c", "#fa7ee3", "#19a71f", "#e313ee", "#09f54c")) +
  scale_fill_manual(values = c("#58b5e1", "#b11478", "#9bea30", "#4224bf", "#749d3c", "#fa7ee3", "#19a71f", "#e313ee", "#09f54c")) + 
  ggtitle("Pest Pressure through the years") + xlab("Year") + ylab("Area Infested (%)") + 
  facet_wrap(state~group,scales="free") 

total_acres <- test %>%
  filter(smooth == "s(total_acres)") %>%
  ggplot(aes(x=total_acres)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se),alpha=.1) +
  geom_line(aes(y = adj_est),size=1.5) + ggpubr::theme_pubr() +
  ggtitle("acres infested x total acres") + xlab("total acres") + ylab("Estimate")


mod_trt <- test %>% filter(smooth == "te(MOD,Trt)") %>%
  ggplot(aes(x=(MOD/60),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr()  +
  scale_color_manual(values = c("#1b9e77","#d95f02"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("Minute of day by treatment smooth") + xlab("Hour of Day") +
  xlim(0,24)
