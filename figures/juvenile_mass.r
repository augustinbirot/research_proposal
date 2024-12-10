library(ggplot2)


theme_set(theme(
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  title = element_text(size = 11)
))

variable_names <- list("F" = "FEMALE",
                       "M" = "MALE")

variable_labeller <- function(variable, value) {
  return(variable_names[value])
}


male_col <- "red"
female_col <- "black"


df <- read.csv("/Users/augustinbirot/Desktop/Work/PhD/research/research_projects/ybm_mass_evolution/data/tb_annualid.csv") #nolint
df <- subset(df, df$age_class == "J")
df <- subset(df, !is.na(df$massaug))
head(df)

################################### massaug ###################################

(plot1_aug <- ggplot(df, aes(x = year_recruit, y = massaug)) +
   geom_point(alpha = .25, col = "grey") +
   geom_smooth(method = "loess", formula = "y ~ x",
               size = 1.2, aes(color = sex)) +
   geom_smooth(method = "lm", col = "darkgrey", formula = "y ~ x",
               se = FALSE, linetype = "dashed") +
   xlab("Cohort (birth year)") + ylab("Body mass (g)") +
   scale_color_manual(values = c(female_col, male_col),
                      labels = c("Female", "Male")) +
   facet_wrap(~ df$sex, labeller = variable_labeller))


predictions <- ggplot_build(plot1_aug)$data[[2]]

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




df$year_recruit_sc <- scale(df$year_recruit)


mod1 <- lm(massaug ~ poly(year_recruit_sc, 3) * sex, data = df)
summary(mod1)

mod2 <- lm(massaug ~ poly(year_recruit_sc, 3) + sex, data = df)

anova(mod1, mod2)

mod <- mod2

summary(mod)



eff_data <- data.frame(effects::effect(c("sex:year_recruit_sc"),
                                       mod, partial.residuals = TRUE))

(plot2_aug <- ggplot(eff_data, aes(x = year_recruit_sc,
                                   y = fit,
                                   color = sex)) +
   geom_ribbon(data = eff_data, aes(ymin = lower, # Model's predictions SE
                                    ymax = upper,
                                    fill = sex),
               linetype = 0, alpha = .4) +
   geom_line(data = eff_data, aes(x = year_recruit_sc, # Model's predictions
                                  y = fit,
                                  color = sex)) +
   geom_point(data = df, aes(x = year_recruit_sc, # Raw data
                             y = massaug,
                             color = sex),
              size = .8, shape = 16, alpha = .06) +
   scale_color_manual(values = c(female_col, male_col),
                      labels = c("Female", "Male")) +
   scale_fill_manual(values = c(female_col, male_col),
                     labels = c("Female", "Male")) +
   scale_y_continuous(limits = c(100, 2500),
                      breaks = seq(500, 2500, 500)) +
   facet_wrap(~sex, labeller = variable_labeller) +
   xlab("Cohort") +
   ylab("Body mass (g)") +
   ggtitle("b)")) # "Mass at the end of the first season"




################################### massjun ###################################

(plot1_jun <- ggplot(df, aes(x = year_recruit, y = massjun)) +
   geom_point(alpha = .25, col = "grey") +
   geom_smooth(method = "loess", formula = "y ~ x",
               size = 1.2, aes(color = sex)) +
   geom_smooth(method = "lm", col = "darkgrey", formula = "y ~ x",
               se = FALSE, linetype = "dashed") +
   xlab("Cohort (birth year)") + ylab("Body mass (g)") +
   scale_color_manual(values = c(female_col, male_col),
                      labels = c("Female", "Male")) +
   facet_wrap(~ df$sex, labeller = variable_labeller))


df$year_recruit_sc <- scale(df$year_recruit)


mod1 <- lm(massjun ~ poly(year_recruit_sc, 3) * sex, data = df)
summary(mod1)

mod2 <- lm(massjun ~ poly(year_recruit_sc, 3) + sex, data = df)

anova(mod1, mod2)

mod <- mod2

summary(mod)



eff_data <- data.frame(effects::effect(c("sex:year_recruit_sc"),
                                       mod, partial.residuals = TRUE))

(plot2_jun <- ggplot(eff_data, aes(x = year_recruit_sc,
                                   y = fit,
                                   color = sex)) +
   geom_ribbon(data = eff_data, aes(ymin = lower, # Model's predictions SE
                                    ymax = upper,
                                    fill = sex),
               linetype = 0, alpha = .4) +
   geom_line(data = eff_data, aes(x = year_recruit_sc, # Model's predictions
                                  y = fit,
                                  color = sex)) +
   geom_point(data = df, aes(x = year_recruit_sc, # Raw data
                             y = massjun,
                             color = sex),
              size = .8, shape = 16, alpha = .06) +
   scale_color_manual(values = c(female_col, male_col),
                      labels = c("Female", "Male")) +
   scale_fill_manual(values = c(female_col, male_col),
                     labels = c("Female", "Male")) +
   scale_y_continuous(limits = c(100, 2500),
                      breaks = seq(500, 2500, 500)) +
   facet_wrap(~sex, labeller = variable_labeller) +
   xlab("Cohort") +
   ylab("Body mass (g)") +
   ggtitle("a)")) # "Mass at birth"


cowplot::plot_grid(plot2_jun, plot2_aug, ncol = 2)
