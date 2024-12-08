library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)

#Section 03
coffee_dt <- fread("./extdata/coffee_sim.csv")
coffee_dt
summary(coffee_dt)
ggplot(coffee_dt, aes(x=cups_per_day, y=risk)) +geom_boxplot()
ggplot(coffee_dt, aes(x=packs_per_day, y=risk)) +geom_boxplot()
ggplot(coffee_dt, aes(packs_per_day, risk, fill=cups_per_day)) + geom_boxplot() + facet_wrap(~cups_per_day)
ggplot(coffee_dt, aes(cups_per_day, risk, fill=packs_per_day)) + geom_boxplot() + facet_wrap(~packs_per_day)

ggplot(coffee_dt, aes(cups_per_day, risk, fill = packs_per_day)) +geom_boxplot() +
  labs(x = "Cups of coffee per day",
       y = "Deaths per 1,000") +
  guides(fill = guide_legend(title = "Packs of cigarettes"))

# Section 04
dt <- data.table(pro_uptake = c(rnorm(3, 10100, 300), rnorm(4, 12100, 300),
                                rnorm(3, 9850, 300), rnorm(4, 11100, 300),
                                rnorm(4,8300, 300), rnorm(3,10050, 300),
                                rnorm(3, 12000, 300), rnorm(3, 10020, 300), rnorm(3, 10080, 300), rnorm(3, 10070, 300) ),
                 mutants = c(rep("WT",3), rep("T49A",4), rep("K227N",3), rep("A400V",4),
                             rep("L421P",4), rep("I500T",3), rep("N591D",3),
                             rep("A601T",3), rep("E684D",3), rep("G710R",3) ) )
summary_dt <- dt[, .(mean = mean(pro_uptake),
                     sd = sd(pro_uptake)),
                   by = "mutants"]
x_order <- summary_dt[order(mean), mutants]
summary_dt[, mutants := factor(mutants, levels = x_order)]
dt[, mutants := factor(mutants, levels = x_order)]
# get wt mean
wt <- summary_dt[mutants == "WT", mean]
# group mutants to larger and smaller than wt
summary_dt[, color := ifelse(mean > wt, "Larger",
                             ifelse(mean == wt, "WT", "smaller"))]
ggplot(summary_dt) +
  geom_bar(aes(mutants, mean, fill = color), stat="identity") +
  geom_errorbar(aes(mutants, ymax=mean+sd, ymin=mean-sd), width = 0.2) +
  geom_jitter(data = dt, aes(mutants, pro_uptake)) +
  theme(axis.text.x = element_text(size=6))

# Section 05
fatality_dt <- fread('extdata/belgium_infection_fatality_rate_june2020.csv')
fatality_dt
fatality_dt <- melt(fatality_dt, id.vars ="age_group",
                    value.name = "fatality_rate", variable.name = "sex")
ggplot(fatality_dt, aes(age_group, fatality_rate, fill=sex)) +
  geom_col(position="dodge") +
  geom_text(aes(label=fatality_rate),
            position=position_dodge(width=0.9), vjust=-0.25, size=2) +
  theme(axis.text.x = element_text(size=6))
# Likely, there are more older women than polder men in Belgium, and since older people have a higher fatality rate in general, women are 'more' likely to die of covid simply because there were more older women to start with.
# This is an example of simpsons paradox because

# Section 06
datavizitis_smoking_dt <- fread("./extdata/datavizitis_smoking.csv")
datavizitis_smoking_dt
ggplot(datavizitis_smoking_dt[hospitalized == "Yes"], aes(x=cigarettes_per_day, y=datavizitis_severity)) + geom_point() +  geom_smooth(method="lm")
ggplot(datavizitis_smoking_dt, aes(x=cigarettes_per_day, y=datavizitis_severity)) + geom_point(aes(color=hospitalized)) +  geom_smooth(method="lm", aes(color=hospitalized))
# Section 07
titanic <- fread("./extdata/titanic.csv")
class(titanic)
titanic[, .(survival_rate = sum(survived==1)/.N),]
titanic$survived <- as.factor(titanic$survived)
titanic$pclass <- as.factor(titanic$pclass)
ggplot(titanic, aes(survived, age)) +
  geom_boxplot()
titanic
ggplot(titanic, aes(x = pclass, fill = survived)) +
  geom_bar(position = "fill")
ggplot(titanic, aes(pclass, age)) +
  geom_boxplot()

library(cowplot)
p1 <- ggplot(titanic, aes(factor(survived), age)) +
  geom_boxplot() +
  facet_wrap(~factor(pclass))
n_class_surv_dt <- titanic[, .N,by=.(pclass, survived)]
p2 <- ggplot(n_class_surv_dt, aes(factor(survived), N)) +
  geom_col() +
  facet_wrap(~factor(pclass))
plot_grid(
  p1, p2,
  labels = "AUTO", ncol = 1
)