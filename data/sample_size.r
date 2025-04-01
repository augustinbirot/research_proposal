############## INTRODUCTION ##############

library(ybamaRmot)

data(tb_annualid)

tb_annualid$uid <- as.factor(tb_annualid$uid)
tb_annualid$year.uid <- as.factor(tb_annualid$year.uid)
tb_annualid$year <- as.factor(tb_annualid$year)

str(tb_annualid)

tb_annualid <- subset(tb_annualid, !is.na(nb_mass))

summary(tb_annualid$massaug)
summary(tb_annualid$massjun)



# Number of observations
nrow(tb_annualid)

nlevels(as.factor(tb_annualid$massaug))
(table(table(tb_annualid$massaug)))


# Number of individuals
nlevels(tb_annualid$uid)
# Number of years of observations
nlevels(tb_annualid$year)


## Pedigree
pedigree <- fn_ped("dam-sire")
summary(pedigree)

nrow(pedigree)

# Number of individuals with known fathers
nrow(subset(pedigree, !is.na(sire)))
# Number of individuals with known mothers
nrow(subset(pedigree, !is.na(dam)))




############## CHAPTER 1 ##############
library(ybamaRmot)

df <- read.csv("/Users/augustinbirot/Desktop/Work/PhD/research/research_projects/ybm_mass_evolution/data/tb_annualid.csv") #nolint

df <- subset(df, !is.na(nb_mass))

## Add colonies
for (i in 1:nrow(df)) { #nolint
  df[i, "colony"] <- tb_colony[which(tb_colony$col_area == #nolint
                                       df[i, "col_area"]),
                               "colony"]
} # Add colony variable from "tb_colony" data frame from "ybamaRmot" package


## Class colonies between "main" and "peripheral"
col_type <- c()     # Create col_type variable

for (i in 1:nrow(df)){ #nolint
  col_type[i] <- tb_colony[which(tb_colony$col_area ==
                                   df[i, "col_area"]), "main"]
} # Fill the variable from the 'tb_colony" data frame from ybamaRmot

col_type <- as.character(col_type)  # Convert it to "character" class

for (i in 1:length(col_type)) { #nolint
  if (col_type[i] == 1) {
    col_type[i] <- "main"
  } else {
    col_type[i] <- "peripheral"
  }
} # Assign "1" value to "main" and other values to "peripheral"

df$col_type <- col_type   # Add col_type variable to "df"                   #nolint


## Remove peripheral colonies (not studied here)
df <- subset(df, df$col_type == "main") #nolint



sub <- list(split(df, df$sex))[[1]]   # Split data according
#to sex

df_F <- sub$F


sub$F <- list(split(sub$F, sub$F$age_class))[[1]] # Split female data according
#to age_class

df_J <- sub$F$J
df_Y <- sub$F$Y
df_S <- sub$F$S
df_A <- sub$F$A



nlevels(as.factor(df_J$uid))
nlevels(as.factor(df_J$year.uid))
nlevels(as.factor(df_J$year));levels(as.factor(df_J$year))
nlevels(as.factor(df_J$year_recruit));levels(as.factor(df_J$year_recruit))

nlevels(as.factor(df_Y$uid))
nlevels(as.factor(df_Y$year.uid))
nlevels(as.factor(df_Y$year));levels(as.factor(df_Y$year))
nlevels(as.factor(df_Y$year_recruit));levels(as.factor(df_Y$year_recruit))

nlevels(as.factor(df_S$uid))
nlevels(as.factor(df_S$year.uid))
nlevels(as.factor(df_S$year));levels(as.factor(df_S$year))
nlevels(as.factor(df_S$year_recruit));levels(as.factor(df_S$year_recruit))

nlevels(as.factor(df_A$uid))
nlevels(as.factor(df_A$year.uid))
nlevels(as.factor(df_A$year));levels(as.factor(df_A$year))
nlevels(as.factor(df_A$year_recruit));levels(as.factor(df_A$year_recruit))



nlevels(as.factor(df_F$uid))
