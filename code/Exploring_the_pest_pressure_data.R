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
library(glmmTMB)
library(emmeans)
library(MetBrewer)

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


#Okay lets do some model selection

mod0 <- gam(acres_infested   ~ 1,
           family=tw(), data=dat)

mod1 <- bam(acres_infested   ~ te(year,group,bs=c("tp","re"),m=2,k=20),
            select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)

mod2 <- bam(acres_infested   ~ te(year,group,bs=c("tp","re"),m=2,k=20) +
              s(region,state,bs="re"),
            select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)

mod3 <- bam(acres_infested   ~ te(year,group,bs=c("tp","re"),m=2,k=20) +
              s(region,state,bs="re") + 
              s(total_acres,k=25),
            select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)

mod3.5 <- bam(acres_infested   ~ te(year,group,bs=c("tp","re"),m=2,k=20) +
              s(region,bs="re") + 
              s(total_acres,k=25),
            select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)
mod3.75 <- bam(acres_infested   ~ te(year,group,bs=c("tp","re"),m=2,k=20) +
                s(state,bs="re") + 
                s(total_acres,k=25),
              select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)

  
mod <- bam(acres_infested   ~
             te(year,group,bs=c("tp","re"),m=2,k=20) +
             s(region,state,bs="re") + 
             s(total_acres,k=25),
           select=TRUE,family=tw(), data=dat,discrete = TRUE,nthreads = 23)


mod_by <- bam(acres_infested  ~
             te(year,by=group,bs="tp",k=20) +
             s(region,state,bs="re") + 
             s(total_acres,k=25),
           select=TRUE,family=tw(),data=dat,discrete = TRUE,nthreads = 23)


AIC(mod0,mod1,mod2,mod3,mod3.5,mod3.75,mod_by) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC)

BIC(mod0,mod1,mod2,mod3,mod3.5,mod3.75,mod_by)  %>%
  mutate(deltaBIC = BIC - min(BIC)) %>%
  arrange(deltaBIC)

summary(mod3)
k.check(mod3)
sim_resid <- simulateResiduals(mod3)
plot(sim_resid)



b0 <- coef((mod3))[1]

test <- gratia::smooth_estimates((mod3))

test$adj_est <- test$est 

MetBrewer::colorblind_palettes

group_year <- test %>%
  filter(smooth == "te(year,group)") %>%
  ggplot(aes(x=year,group=group)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=group),alpha=.1) +
  geom_line(aes(y = adj_est, color=group),size=4) + ggpubr::theme_pubr() +
  #scale_color_manual(values = c("#58b5e1", "#b11478", "#9bea30", "#4224bf", "#749d3c", "#fa7ee3", "#19a71f", "#e313ee", "#09f54c")) +
  #scale_fill_manual(values = c("#58b5e1", "#b11478", "#9bea30", "#4224bf", "#749d3c", "#fa7ee3", "#19a71f", "#e313ee", "#09f54c")) + 
  ggtitle("Pest Pressure through the years") + xlab("Year") + ylab("Acres Infested") + 
  geom_vline(xintercept = 1996) +
  geom_vline(xintercept = 2006) +
  scale_color_met_d(name = "Paquin") +
  scale_fill_met_d(name = "Paquin") +  
  #scale_x_continuous(breaks = c(1986,1991,1996,2001,2006,2011,2016,2021)) +
  facet_wrap(~group,scales="free")  + theme(legend.position="none") +
  theme(text = element_text(size=20))

group_year

total_acres <- test %>%
  filter(smooth == "s(total_acres)") %>%
  ggplot(aes(x=total_acres)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se),alpha=.1) +
  geom_line(aes(y = adj_est),size=1.5) + ggpubr::theme_pubr() +
  ggtitle("acres infested x total acres") + xlab("total acres") + ylab("Estimate")


#Now lets just do a glmm to show differences between group and BT era

glmm_dat <- dat %>% select(acres_infested,total_acres,group,toxin,BT_group,state,region,acres_infested_int) %>% drop_na() %>%
  mutate(group = gsub(" ", "_", group),
    bt_group2 = paste0(toxin,".",group))
summary(glmm_dat)

glmm <- glmmTMB(acres_infested ~ bt_group2 + total_acres,data=glmm_dat,
                control = glmmTMBControl(parallel = 23))

summary(glmm)
simresid <- simulateResiduals(glmm)
plot(simresid)


em <- multcomp::cld(emmeans(glmm, ~ bt_group2)) %>% as.data.frame()

?separate
em <- em %>% separate(bt_group2,c("toxin","group"),sep="\\.")

flevels <- c("pre_BT", "Transition", "BT_era")


MetBrewer::colorblind_palettes
MetBrewer::scale_color_met_d()


options(scipen = 999)
emmeans <- em %>%
  select(1:4) %>%
  mutate(lower = emmean - SE, upper = emmean + SE,
         toxin = factor(toxin,levels=flevels)) %>%
  ggplot(aes(x=toxin,y=emmean,color=toxin)) + 
  geom_point(position=position_dodge(width = 0.9),size=8) +
  scale_y_continuous() +
  scale_color_met_d(name = "Demuth") + 
  geom_errorbar(aes(ymin = lower, ymax = upper),position=position_dodge(width = 0.9),width=0,size=1.5) +
  ggpubr::theme_pubr() + #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  facet_wrap(~group,scales="free") + theme(legend.position="none") +
  theme(text = element_text(size=20)) + xlab("") + ylab("Estimated marginal means")

ggsave(group_year,file='output/pest_pressure_years.png', dpi=300,width=15,height=15)
ggsave(emmeans,file='output/pest_emmeans.png', dpi=300,width=15,height=15)
