####
# Some quick plotting
#
####
rm(list=ls())

library(tidyverse)
library(mgcv)

dat <- read_csv("data/processed/Pest-damage-dat-refine-edit.csv", 
                col_types = cols(acres_infested = col_number(),
                                 percent_acres_infested = col_number(),
                                 total_acres = col_number())) %>%
  mutate(pest = factor(pest), region = factor(region))
summary(dat)



dat2 <- dat %>%
  filter(percent_acres_infested <= 1,year2 >0)

ggplot(dat2,aes(x=percent_acres_infested))+geom_histogram()

unique(levels(comparison$pest))



major_pests <- (dat2 %>%
  group_by(pest) %>%
  summarize(n=n()) %>%
  arrange(desc(n))) %>%
  filter(n >= 1000) %>% droplevels()

names <- unique(levels(major_pests$pest))
names <- c("")
#just some quick modeling

comparison <- dat2 %>% filter(pest %in% names)

ggplot(comparison,aes(x=year2,y=percent_acres_infested,color=pest)) + geom_smooth() + ggpubr::theme_pubr()


mod <- gam(percent_acres_infested~te(year2,pest,bs="fs",m=2,k=20) + s(region,bs="re"),select=TRUE,family=scat(),data=comparison)

mod_by <- gam(percent_acres_infested~te(year2,by=pest,bs="tp",m=2,k=20) + s(region,bs="re"),select=TRUE,family=scat(),data=comparison)

AIC(mod,mod_by)

summary(mod)
k.check(mod)
gratia::appraise(mod)
output <- gratia::smooth_estimates(mod)

b0 <- coef((mod))[1]

test <- gratia::smooth_estimates((mod))

test$adj_est <- test$est + b0

date_trt <- test %>%
  drop_na(pest) %>%
  ggplot(aes(x=year2,group=pest)) + #geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=pest),alpha=.1) +
  geom_line(aes(y = adj_est, color=pest),size=1.5) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#5b588f", "#f1c039", "#a21fa1", "#53a4e5")) +
  scale_fill_manual(values = c("#5b588f", "#f1c039", "#a21fa1", "#53a4e5")) + ggtitle("Pest Pressure through the years") + xlab("Year") + ylab("Area Infested (%)")

date_block <- test %>% filter(smooth == "te(Date_numeric,Block)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr() +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("date by block smooth")+ xlab("Date")

mod_trt <- test %>% filter(smooth == "te(MOD,Trt)") %>%
  ggplot(aes(x=(MOD/60),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr()  +
  scale_color_manual(values = c("#1b9e77","#d95f02"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("Minute of day by treatment smooth") + xlab("Hour of Day") +
  xlim(0,24)
