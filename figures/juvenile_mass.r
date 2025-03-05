library(ggplot2)
library(tidyr)
library(dplyr)


theme_set(theme(
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  title = element_text(size = 11),
  plot.tag = element_text(size = 15)
))


variable_names <- list("beg" = "Beginning of the season",
                       "end" = "End of the season")

variable_labeller <- function(variable, value) {
  return(variable_names[value])
}


male_col <- "black"
female_col <- "#dbbc09"


df <- read.csv("data/tb_annualid.csv") #nolint
df <- subset(df, df$age_class == "J")
df <- subset(df, !is.na(df$massaug))
df$year_recruit_sc <- scale(df$year_recruit)

df_long <- df %>%
  pivot_longer(
    cols = c(massjun, massaug),
    names_to = "season",
    values_to = "mass"
  ) %>%
  mutate(season = ifelse(season == "massjun", "beg", "end"))

head(df_long)


###############
# Mod aug
###############

mod_aug_1 <- lm(massaug ~ poly(year_recruit_sc, 3) * sex, data = df)

mod_aug_2 <- lm(massaug ~ poly(year_recruit_sc, 3) + sex, data = df)

anova(mod_aug_1, mod_aug_2)

mod_aug <- mod_aug_2

summary(mod_aug)

eff_data_aug <- data.frame(effects::effect(c("sex:year_recruit_sc"),
                                           mod_aug, partial.residuals = TRUE))

eff_data_aug$season <- as.factor(rep("end", nrow(eff_data_aug)))


###############
# Mod jun
###############

mod_jun_1 <- lm(massjun ~ poly(year_recruit_sc, 3) * sex, data = df)

mod_jun_2 <- lm(massjun ~ poly(year_recruit_sc, 3) + sex, data = df)

anova(mod_jun_1, mod_jun_2)

mod_jun <- mod_jun_2

summary(mod_jun)

eff_data_jun <- data.frame(effects::effect(c("sex:year_recruit_sc"),
                                           mod_jun, partial.residuals = TRUE))

eff_data_jun$season <- as.factor(rep("beg", nrow(eff_data_jun)))



################ Combine ################

eff_data <- merge(eff_data_jun, eff_data_aug, all = TRUE)


juvenile_trend <- ggplot(eff_data, aes(x = year_recruit_sc,
                                       y = fit,
                                       color = sex)) +
  facet_wrap(~season, labeller = variable_labeller) +
  geom_ribbon(data = eff_data, aes(ymin = lower, # Model's predictions SE
                                   ymax = upper,
                                   fill = sex),
              linetype = 0, alpha = .4) +
  geom_line(data = eff_data, aes(x = year_recruit_sc, # Model's predictions
                                 y = fit,
                                 color = sex)) +
  geom_point(data = df_long, aes(x = year_recruit_sc, # Raw data
                                 y = mass,
                                 color = sex),
             size = .8, shape = 16, alpha = .06) +
  scale_color_manual(values = c(female_col, male_col),
                     labels = c("Female", "Male")) +
  scale_fill_manual(values = c(female_col, male_col),
                    labels = c("Female", "Male")) +
  scale_y_continuous(limits = c(100, 2500),
                     breaks = seq(500, 2500, 500)) +
  scale_x_continuous(labels = unique(df$year_recruit)[c(seq(1, 50, 10))],
                     breaks = unique(df$year_recruit_sc)[c(seq(1, 50, 10))],
                     limits = c(unique(df$year_recruit_sc)[1],
                                unique(df$year_recruit_sc)[50])) +
  xlab("Cohort") +
  ylab("Body mass (g)")

ggsave("figures/fig-juvenile-trend.png", plot = juvenile_trend)



############################ Aug prediction (raw) ############################

plot_aug_raw <- ggplot(df, aes(x = year_recruit, y = massaug)) +
  geom_point(alpha = .25, col = "grey") +
  geom_smooth(method = "loess", formula = "y ~ x",
              size = 1.2, aes(color = sex)) +
  geom_smooth(method = "lm", col = "darkgrey", formula = "y ~ x",
              se = FALSE, linetype = "dashed") +
  xlab("Cohort (birth year)") + ylab("Body mass (g)") +
  scale_color_manual(values = c(female_col, male_col),
                     labels = c("Female", "Male")) +
  facet_wrap(~ df$sex, labeller = variable_labeller)


predictions <- ggplot_build(plot_aug_raw)$data[[2]]

predictions$colour <- as.factor(predictions$colour)

predictions_f <- subset(predictions, predictions$colour == female_col)
predictions_m <- subset(predictions, predictions$colour == male_col)


# Female
## Mass
min(predictions_f$y)
max(predictions_f$y)

## Cohort
predictions_f[which(predictions_f$y == min(predictions_f$y)), "x"]
predictions_f[which(predictions_f$y == max(predictions_f$y)), "x"]


# Male
## Mass
min(predictions_m$y)
max(predictions_m$y)

## Cohort
predictions_m[which(predictions_m$y == min(predictions_m$y)), "x"]
predictions_m[which(predictions_m$y == max(predictions_m$y)), "x"]







########################## FEMALES ONLY ###########################
df_F <- subset(df, df$sex == "F")
df_F <- subset(df, !is.na(df_F$massaug))
df_F$year_recruit_sc <- scale(df_F$year_recruit)

df_F_long <- df_F %>%
  pivot_longer(
    cols = c(massjun, massaug),
    names_to = "season",
    values_to = "mass"
  ) %>%
  mutate(season = ifelse(season == "massjun", "beg", "end"))

head(df_F_long)


###############
# Mod aug
###############

mod_F_aug <- lm(massaug ~ poly(year_recruit_sc, 3), data = df_F)

summary(mod_F_aug)

eff_data_aug_F <- data.frame(effects::effect(c("year_recruit_sc"),
                                             mod_F_aug, partial.residuals = TRUE))

eff_data_aug_F$season <- as.factor(rep("end", nrow(eff_data_aug_F)))


###############
# Mod jun
###############

mod_F_jun <- lm(massjun ~ poly(year_recruit_sc, 3), data = df_F)

summary(mod_F_jun)

eff_data_jun_F <- data.frame(effects::effect(c("year_recruit_sc"),
                                             mod_F_jun, partial.residuals = TRUE))

eff_data_jun_F$season <- as.factor(rep("beg", nrow(eff_data_jun_F)))



################ Combine ################

eff_data_F <- merge(eff_data_jun_F, eff_data_aug_F, all = TRUE)


juvenile_trend_F <- ggplot(eff_data_F, aes(x = year_recruit_sc,
                                       y = fit,
                                       color = "black")) +
  facet_wrap(~season, labeller = variable_labeller) +
  geom_ribbon(data = eff_data_F, aes(ymin = lower, # Model's predictions SE
                                   ymax = upper,
                                   fill = "black"),
              linetype = 0, alpha = .4) +
  geom_line(data = eff_data_F, aes(x = year_recruit_sc, # Model's predictions
                                 y = fit,
                                 color = "black")) +
  geom_point(data = df_F_long, aes(x = year_recruit_sc, # Raw data
                                 y = mass,
                                 color = "black"),
             size = .8, shape = 16, alpha = .2) +
  scale_color_manual(values = "black",
                     labels = "Female") +
  scale_fill_manual(values = "black",
                    labels = "Female") +
  scale_y_continuous(limits = c(100, 2000),
                     breaks = seq(500, 2000, 500)) +
  scale_x_continuous(labels = unique(df$year_recruit)[c(seq(1, 50, 10))],
                     breaks = unique(df$year_recruit_sc)[c(seq(1, 50, 10))],
                     limits = c(unique(df$year_recruit_sc)[1],
                                unique(df$year_recruit_sc)[50])) +
  xlab("Cohort") +
  ylab("Body mass (g)")

ggsave("figures/fig-juvenile-trend-F.png", plot = juvenile_trend_F)
