library(ybamaRmot)
library(ggplot2)
library(lme4)
library(lmerTest)

theme_set(theme(
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black", size = 1),
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 13),
  title = element_text(size = 11),
  plot.tag = element_text(size = 20)
))


data(tb_annualid)

data(tb_focal)
tb_focal$year.uid <- paste(tb_focal$year, tb_focal$uid, sep = ".")

data(tb_fid)
data(tb_alarmcallobs)



focal_mass <- merge(tb_focal, tb_annualid[,
                                          c("year.uid", "massaug", "massjun")],
                    by = "year.uid", all.x = TRUE)



focal_mass$age <- as.integer(fn_age(focal_mass$uid, focal_mass$year))

focal_mass <- subset(focal_mass, !is.na(age))
focal_mass <- subset(focal_mass, age >= 0)

for(i in 1:nrow(focal_mass)){
  if(focal_mass[i, "age"]==0){
    focal_mass[i, "age_class"] <- "Juveniles"   #Age 0 - Juveniles, "J"
  }
  if(focal_mass[i, "age"]==1){
    focal_mass[i, "age_class"] <- "Yearlings"   #Age 0-1 - Yearlings, "Y"
  }
  if(focal_mass[i, "age"]==2){
    focal_mass[i, "age_class"] <- "Subadults"   #Age 1-2 - Sub-adult, "S"
  }
  if(focal_mass[i, "age"]>=3){
    focal_mass[i, "age_class"] <- "Adults"   #Age 3+ - Adults, "A"
  }
}

focal_mass$age_class <- factor(focal_mass$age_class,
                               levels = c("Juveniles", "Yearlings",
                                          "Subadults", "Adults"))


focal_mass <- subset(focal_mass, !is.na(massaug))


focal_mass$massaug_sc <- scale(focal_mass$massaug)





#######################################################

plot_prop_sfg <-
  ggplot(focal_mass, aes(x = massaug_sc, y = prop_sfg)) +
  geom_point(alpha = .1) +
  geom_smooth(col = "red") +
  facet_wrap(~ age_class) +
  theme(strip.text = element_text(size = 14)) +
  ylab("Proportion of time stand foraging") +
  xlab("Scaled(Mass)")

ggsave("figures/prop_sfg.png", #nolint
       plot_prop_sfg)

summary(lmer(prop_sfg ~ massaug_sc + age_class + (1 | uid), data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = log(prop_rfg))) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(prop_rfg ~ massaug_sc * age_class + (1 | uid), data = focal_mass))

##############################

plot_prop_slook <-
  ggplot(focal_mass, aes(x = massaug_sc, y = prop_slook)) +
  geom_point(alpha = .1) +
  geom_smooth(col = "red") +
  facet_wrap(~ age_class) +
  theme(strip.text = element_text(size = 14)) +
  ylab("Proportion of time stand looking") +
  xlab("Scaled(Mass)")

ggsave("figures/prop_slook.png", #nolint
       plot_prop_slook)

summary(lmer(prop_slook ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = prop_run)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(prop_run ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = prop_rlook)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(prop_rlook ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = prop_other)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(prop_other ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = prop_walk)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(prop_walk ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))

##############################

ggplot(focal_mass, aes(x = massaug_sc, y = meanbout_slook)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~ age_class)

summary(lmer(meanbout_slook ~ massaug_sc + age_class + (1 | uid),
             data = focal_mass))
