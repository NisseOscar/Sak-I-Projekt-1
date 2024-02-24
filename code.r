########### RISK MODELS AND RESERVING IN NON-LIFE INSURANCE, PROJECT 1 2024 ##########

############################### Pre-Config ################################


# Load the necessary packages
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(MASS)
library(stats)
library(moments)
library(fitdistrplus)
library(predint)
library(reshape2)

# Load the text file
claims <- read.table("Projekt1_Grupp8.txt", header = TRUE,sep=";")
claims %>% head()

# Add some columns which will be used later
claims$t <- claims$ClaimDay /365
claims$s <- claims$ClaimDay %% 365 / 365
claims %>% head()

#### Assignment 1-2 ########


########## Claim type 1 ##########
set.seed(1337)
claims1 <- claims %>% filter(claims$ClaimType == 1)
claims1 %>% head()

# Series of cumulative amount of claims
q <-claims1 %>% mutate(ClaimCost = cumsum(ClaimCost)) %>% 
    ggplot(aes(x = ClaimDay, y = ClaimCost)) +
    geom_line() +
    labs(title = "Cumulative claim cost as a function of days for claim type 1", x = "Days", y = "Cumulative claim cost")
ggsave("plots/claim_type1/cumulative_claimcost.png", plot = q, device = "png")

# Histogram over claim cost
q <-claims1 %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 1000) +
    labs(title = "Histogram over claim cost for claim type 1", x = "Claim cost", y = "Frequency")
ggsave("plots/claim_type1/histogram_claimcost.png", plot = q, device = "png", width = 6, height = 4)

############ Analyse Large Claims ############ 
claims1_large <- filter(claims1, ClaimCost > 100000)
q <- claims1_large %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 10000) +
    labs(title = "Histogram over claim cost for claim type 1", x = "Claim cost", y = "Frequency") 
ggsave("plots/claim_type1/histogram_claimcost_large.png", plot = q, device = "png", width = 6, height = 4)

# QQ-plots normality
q <- ggplot(claims1_large, aes(sample = ClaimCost)) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type1/qqplot_normality.png", plot = q, device = "png", width = 5, height = 5)

# QQ-plots lognormality
q <- ggplot(claims1_large, aes(sample = log(ClaimCost))) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type1/qqplot_lognormality.png", plot = q, device = "png", width = 5, height = 5)

# kolmorogov smirnov test
ks.test(claims1_large$ClaimCost, "pnorm", mean(claims1_large$ClaimCost), sd(claims1_large$ClaimCost))
#lognormal
ks.test(log(claims1_large$ClaimCost), "pnorm", mean(log(claims1_large$ClaimCost)), sd(log(claims1_large$ClaimCost)))

############ Study if there is a time dependency ############
q <-claims1_large %>% ggplot(aes(x = t, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(title = "Time-dependency of large claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type1/claimcost_arrivaltime_t.png", plot = q, device = "png")

summary(lm(ClaimCost ~ t, data = claims1_large))

# Study if there is a time dependency
q <-claims1_large %>% ggplot(aes(x = s, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(title = "Time-dependency of large claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type1/claimcost_arrivaltime_s.png", plot = q, device = "png")

summary(lm(ClaimCost ~ poly(s, 1), data = claims1_large))
summary(lm(ClaimCost ~ poly(s, 2), data = claims1_large))
summary(lm(ClaimCost ~ poly(s, 3), data = claims1_large))

# Join both together 
summary(lm(ClaimCost ~ s+t, data = claims1_large))

# Fit linear model
fit <- lm(ClaimCost ~ s, data = claims1_large)
 
############### Study the small claims ###############
claims1_small <- filter(claims1, ClaimCost <= 100000)

# Histogram over claim cost
q <-claims1_small %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 100) +
    labs(title = "Histogram over claim cost for claim type 1", x = "Claim cost", y = "Frequency")
ggsave("plots/claim_type1/histogram_claimcost_small.png", plot = q, device = "png", width = 6, height = 4)

### QQ Plots
# Lognormal distribution
q <- ggplot(claims1_small, aes(sample = log(ClaimCost))) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type1/qqplot_lognormality_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(log(claims1_small$ClaimCost), "pnorm", mean(log(claims1_small$ClaimCost)), sd(log(claims1_small$ClaimCost)))
# Gamma distribution
q <- ggplot(claims1_small, aes(sample = ClaimCost)) + stat_qq(distribution = qgamma, dparams = list(shape = 1, rate = 1)) + stat_qq_line(distribution = qgamma, dparams = list(shape = 1, rate = 1))
ggsave("plots/claim_type1/qqplot_gamma_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims1_small$ClaimCost, "pgamma", 1, 1)
# Weibull
q <- ggplot(claims1_small, aes(sample = ClaimCost)) + stat_qq(distribution = qweibull, dparams = list(shape = 1, scale = 1)) + stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 1))
ggsave("plots/claim_type1/qqplot_weibull_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims1_small$ClaimCost, "pweibull", 1, 1)
# Normal
q <- ggplot(claims1_small, aes(sample = ClaimCost)) + stat_qq(distribution = qnorm, dparams = list(mean = 0, sd = 1)) + stat_qq_line(distribution = qnorm, dparams = list(mean = 0, sd = 1))
ggsave("plots/claim_type1/qqplot_normal_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims1_small$ClaimCost, "pnorm", 0, 1)

######### Test Time dependency #########
q <-claims1_small %>% ggplot(aes(x = t, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family =  Gamma(link = "log")),se = FALSE)+
    labs(title = "Time-dependency of small claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type1/claimcost_arrivaltime_t_small.png", plot = q, device = "png")

# Fit lognormal distribution
summary(glm(ClaimCost ~ t, data = claims1_small, family = Gamma(link = "log")))

q <-claims1_small %>% ggplot(aes(x = s, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family =  Gamma(link = "log")), se = FALSE)+
    labs(title = "Time-dependency of small claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type1/claimcost_arrivaltime_s_small.png", plot = q, device = "png")

# Fit lognormal distribution
summary(glm(ClaimCost ~ s, data = claims1_small, family = Gamma(link = "log")))
claims1_small$summer <- 1*(claims1_small$s > 4/12 & claims1_small$s < 8/12)
summary(glm(ClaimCost ~ s+summer, data = claims1_small, family = Gamma(link = "log")))


########## Arrival Data ##########
# Histogram arrival time
q <-claims1 %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 0.1) +
    labs(title = "Histogram over arrival time for claim type 1", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_t.png", plot = q, device = "png")

# Histogram arrival time
q <-claims1 %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for claim type 1", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_s.png", plot = q, device = "png")

############ Arrival time of small claims
q <-claims1_small %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_small_t.png", plot = q, device = "png")

# As a function of s
q <-claims1_small %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_small_s.png", plot = q, device = "png")

############## Arrival time of large claims
q <-claims1_large %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_large_t.png", plot = q, device = "png")

# Arrival time of small claims
q <-claims1_large %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type1/histogram_arrivaltime_large_s.png", plot = q, device = "png")


##### Seem to follow the same intensity Check for fit

claims1$summer <- 1*(claims1$s > 4/12 & claims1$s < 8/12)

# Do poisson regression
summary(glm(ClaimCost ~ s+summer, data = claims1, family = poisson(link = "log")))

# Check intesnity by month
claims1 %>% group_by((s*12)%%12 %/%1) %>% summarise(n = n())

########## Claim type 2 ##########
claims2 <- claims %>% filter(claims$ClaimType == 2)
claims2 %>% head()

# Series of cumulative amount of claims
q <-claims2 %>% mutate(ClaimCost = cumsum(ClaimCost)) %>% 
    ggplot(aes(x = ClaimDay, y = ClaimCost)) +
    geom_line() +
    labs(title = "Cumulative claim cost as a function of days for claim type 2", x = "Days", y = "Cumulative claim cost")
ggsave("plots/claim_type2/cumulative_claimcost_2.png", plot = q, device = "png")

# Histogram over claim cost
q <-claims2 %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 1000) +
    labs(title = "Histogram over claim cost for claim type 2", x = "Claim cost", y = "Frequency")
ggsave("plots/claim_type2/histogram_claimcost_2.png", plot = q, device = "png", width = 6, height = 4)

############ Analyse Large Claims ############
claims2_large <- filter(claims2, ClaimCost > 1000000)
q <- claims2_large %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 10000) +
    labs(title = "Histogram over claim cost for claim type 1", x = "Claim cost", y = "Frequency") 
ggsave("plots/claim_type2/histogram_claimcost_large.png", plot = q, device = "png", width = 6, height = 4)

# QQ-plots normality
q <- ggplot(claims2_large, aes(sample = ClaimCost)) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type2/qqplot_normality.png", plot = q, device = "png", width = 5, height = 5)

# QQ-plots lognormality
q <- ggplot(claims2_large, aes(sample = log(ClaimCost))) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type2/qqplot_lognormality.png", plot = q, device = "png", width = 5, height = 5)

# kolmorogov smirnov test
ks.test(claims2_large$ClaimCost, "pnorm", mean(claims2_large$ClaimCost), sd(claims2_large$ClaimCost))
#lognormal
ks.test(log(claims2_large$ClaimCost), "pnorm", mean(log(claims2_large$ClaimCost)), sd(log(claims2_large$ClaimCost)))

############ Study if there is a time dependency ############
q <-claims2_large %>% ggplot(aes(x = t, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(title = "Time-dependency of large claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type2/claimcost_arrivaltime_t.png", plot = q, device = "png")

summary(lm(ClaimCost ~ t, data = claims2_large))

# Study if there is a time dependency
q <-claims2_large %>% ggplot(aes(x = s, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(title = "Time-dependency of large claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type2/claimcost_arrivaltime_s.png", plot = q, device = "png")

summary(lm(ClaimCost ~ poly(s, 1), data = claims2_large))
summary(lm(ClaimCost ~ poly(s, 2), data = claims2_large))
summary(lm(ClaimCost ~ poly(s, 3), data = claims2_large))

# Join both together 
summary(lm(ClaimCost ~ s+t, data = claims2_large))

# Fit linear model
fit <- lm(ClaimCost ~ s, data = claims2_large)
 
############### Study the small claims ###############
claims2_small <- filter(claims2, ClaimCost <= 1000000)

# Histogram over claim cost
q <-claims2_small %>% ggplot(aes(x = ClaimCost)) +
    geom_histogram(binwidth = 100) +
    labs(title = "Histogram over claim cost for claim type 1", x = "Claim cost", y = "Frequency")
ggsave("plots/claim_type2/histogram_claimcost_small.png", plot = q, device = "png", width = 6, height = 4)

### QQ Plots
# Lognormal distribution
q <- ggplot(claims2_small, aes(sample = log(ClaimCost))) + stat_qq() + stat_qq_line()
ggsave("plots/claim_type2/qqplot_lognormality_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(log(claims2_small$ClaimCost), "pnorm", mean(log(claims2_small$ClaimCost)), sd(log(claims2_small$ClaimCost)))
# Gamma distribution
q <- ggplot(claims2_small, aes(sample = ClaimCost)) + stat_qq(distribution = qgamma, dparams = list(shape = 1, rate = 1)) + stat_qq_line(distribution = qgamma, dparams = list(shape = 1, rate = 1))
ggsave("plots/claim_type2/qqplot_gamma_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims2_small$ClaimCost, "pgamma", 1, 1)
# Weibull
q <- ggplot(claims2_small, aes(sample = ClaimCost)) + stat_qq(distribution = qweibull, dparams = list(shape = 1, scale = 1)) + stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 1))
ggsave("plots/claim_type2/qqplot_weibull_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims2_small$ClaimCost, "pweibull", 1, 1)
# Normal
q <- ggplot(claims2_small, aes(sample = ClaimCost)) + stat_qq(distribution = qnorm, dparams = list(mean = 0, sd = 1)) + stat_qq_line(distribution = qnorm, dparams = list(mean = 0, sd = 1))
ggsave("plots/claim_type2/qqplot_normal_small.png", plot = q, device = "png", width = 5, height = 5)
ks.test(claims2_small$ClaimCost, "pnorm", 0, 1)

######### Test Time dependency 
q <-claims2_small %>% ggplot(aes(x = t, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family =  Gamma(link = "log")),se = FALSE)+
    labs(title = "Time-dependency of small claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type2/claimcost_arrivaltime_t_small.png", plot = q, device = "png")

# Fit lognormal distribution
summary(glm(ClaimCost ~ t, data = claims2_small, family = Gamma(link = "log")))

q <-claims2_small %>% ggplot(aes(x = s, y = ClaimCost)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family =  Gamma(link = "log")), se = FALSE)+
    labs(title = "Time-dependency of small claims", x = "Arrival time", y = "Claim cost")
ggsave("plots/claim_type2/claimcost_arrivaltime_s_small.png", plot = q, device = "png")

# Fit lognormal distribution
summary(glm(ClaimCost ~ s, data = claims2_small, family = Gamma(link = "log")))
claims2_small$summer <- 1*(claims2_small$s > 4/12 & claims2_small$s < 8/12)
summary(glm(ClaimCost ~ s+summer, data = claims2_small, family = Gamma(link = "log")))


### Arrival data 
# Histogram arrival time
q <-claims2 %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 0.1) +
    labs(title = "Histogram over arrival time for claim type 1", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_t.png", plot = q, device = "png")

# Histogram arrival time
q <-claims2 %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for claim type 1", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_s.png", plot = q, device = "png")

############ Arrival time of small claims
q <-claims2_small %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_small_t.png", plot = q, device = "png")

# As a function of s
q <-claims2_small %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_small_s.png", plot = q, device = "png")

############## Arrival time of large claims
q <-claims2_large %>% ggplot(aes(x = t)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_large_t.png", plot = q, device = "png")

# Arrival time of small claims
q <-claims2_large %>% ggplot(aes(x = s)) +
    geom_histogram(binwidth = 1/100) +
    labs(title = "Histogram over arrival time for small claims", x = "Arrival time", y = "Frequency")
ggsave("plots/claim_type2/histogram_arrivaltime_large_s.png", plot = q, device = "png")

# Check intesnity by month
claims1 %>% group_by((s*12)%%12 %/%1) %>% summarise(n = n())


### Analyese the Covriances between the insurances
set.seed(1337)

claim_agg <- data.frame(ClaimDay = 1:3650)

# Pivot claims basedon claim Size
claim_agg$tot_claim_1 <- sapply(claim_agg$ClaimDay, function(x) sum(claims$ClaimCost[claims$ClaimType == 1 & claims$ClaimDay == x]))
claim_agg$tot_claim_2 <- sapply(claim_agg$ClaimDay, function(x) sum(claims$ClaimCost[claims$ClaimType == 2 & claims$ClaimDay == x]))
q <- claim_agg %>% ggplot(aes(x = tot_claim_1, y = tot_claim_2)) +
    geom_point() +
    labs(title = "Covariance between total claims claim type 1 and 2", x = "Claim type 1", y = "Claim type 2") +
    geom_smooth(aes(),method = "lm", se = TRUE)
ggsave("plots/covariance/daily_claim_size.png", plot = q, device = "png")

# Pivot claims baaed on number of claim
claim_agg$claim_count_1 <- sapply(claim_agg$ClaimDay, function(x) sum(claims$ClaimType == 1 & claims$ClaimDay == x))
claim_agg$claim_count_2 <- sapply(claim_agg$ClaimDay, function(x) sum(claims$ClaimType == 2 & claims$ClaimDay == x))
q <- claim_agg %>% ggplot(aes(x = claim_count_1, y = claim_count_2)) +
    geom_point() +
    labs(title = "Covariance between number of daily claims", x = "Claim type 1", y = "Claim type 2") +
    geom_smooth(aes(), method = "lm", se = TRUE)
ggsave("plots/covariance/daily_claim_number.png", plot = q, device = "png")

# Study sum of claims
claim_agg$avg_claim_1 <- claim_agg$tot_claim_1 / claim_agg$claim_count_1
claim_agg$avg_claim_2 <- claim_agg$tot_claim_2 / claim_agg$claim_count_2
q <- claim_agg %>% ggplot(aes(x = avg_claim_1, y = avg_claim_2)) +
    geom_point() +
    labs(title = "Covariance between total claim claim type 1 and 2", x = "Claim type 1", y = "Claim type 2") +
    geom_smooth(aes(), method = "lm", se = FALSE)
ggsave("plots/covariance/daily_avg_claim_size.png", plot = q, device = "png")

# Study average claim size
fit <- lm(avg_claim_2 ~ avg_claim_1, data = claim_agg)
summary(fit)

# Study affect of count
fit <- lm(claim_count_1 ~ claim_count_2, data = claim_agg)
summary(fit)
claim_agg$residual <- residuals(fit)

# Study residual over time
q <- claim_agg %>% ggplot(aes(x = ClaimDay, y = residual)) +
    geom_point() +
    labs(title = "Residuals over time", x = "Time", y = "Residuals")+
    geom_smooth( method = "lm", se = TRUE)
ggsave("plots/covariance/count_residual_overTime.png", plot = q, device = "png")
fit <- lm(residual ~ ClaimDay, data = claim_agg)
summary(fit)

# add data
claim_agg$s <- claim_agg$ClaimDay %% 365 / 365
claim_agg$t <- claim_agg$ClaimDay / 365
claim_agg$summer <- (claim_agg$s > 4 / 12 & claim_agg$s < 8 / 12)

# Study a poisson fit
fit <- glm(claim_count_2 ~ claim_count_1 + summer, data = claim_agg, family = poisson(link = "log"))
summary(fit)

# Plot poisson fit histogram
q <- claim_agg %>% ggplot(aes(x = claim_count_2,fill=summer)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Histogram over claim count for claim type 2", x = "Claim count", y = "Frequency")+
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    theme(legend.position = "bottom")
ggsave("plots/covariance/histogram_claim_count2.png", plot = q, device = "png")

# Plot poisson fit histogram
q <- claim_agg %>% ggplot(aes(x = claim_count_1,fill=summer)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Histogram over claim count for claim type 1", x = "Claim count", y = "Frequency")+
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    theme(legend.position = "bottom")
ggsave("plots/covariance/histogram_claim_count1.png", plot = q, device = "png")

### Create Function for simulating future claims 
set.seed(1337)

#### Parameters
# Probability for large claim
p_1_large <- sum(1*(claims1$ClaimCost > 100000)) / nrow(claims1)
p_2_large <- sum(1*(claims2$ClaimCost > 1000000)) / nrow(claims2)

# Normal fit of large claims
fit_1_large <- fitdist(claims1$ClaimCost[claims1$ClaimCost > 100000], "norm")
fit_2_large <- fitdist(claims2$ClaimCost[claims2$ClaimCost > 1000000], "norm")

# Fit Lognormal fit for small claims(
fit_1_small <- fitdist(claims1$ClaimCost[claims1$ClaimCost <= 100000], "lnorm")
fit_2_small <- fitdist(claims2$ClaimCost[claims2$ClaimCost <= 1000000], "lnorm")

# fit binomial distribution
# Fit quasi-poisson distribution
qpoi_fit_1 <- glm(claim_count_1 ~ summer + claim_count_2, data = claim_agg, family = quasipoisson(link = "log"))
qpoi_fit_2 <- glm(claim_count_2 ~ summer , , data = claim_agg, family = quasipoisson(link = "log"))
phi_1 <- summary(qpoi_fit_1)$dispersion[1]
phi_2 <- summary(qpoi_fit_2)$dispersion[1]


# Function for simulating data over the years
simulate_claims <- function(years = 1){
    # Create a dataframe to store the results
    results <- data.frame(
        ClaimDay = rep(1:(years*365))
    )
    results$s <- results$ClaimDay %% 365 / 365
    results$t <- results$ClaimDay / 365
    results$summer <- (results$s > 4 / 12 & results$s < 8 / 12)
    # Simulate the number of claims of branch 1
    results$lambda2 <- predict(qpoi_fit_2, newdata = results, type = "response")
    # Simulate quasi-poisson distribution
    results$claim_count_2 <- sapply(1:nrow(results), function(i) rqpois(1, results$lambda2[i], phi = phi_2)$y)
    # Simulate number of claims of branch 2
    results$lambda1 <- predict(qpoi_fit_1, newdata = results, type = "response")
    results$claim_count_1 <- sapply(1:nrow(results), function(i) rqpois(1, results$lambda1[i], phi = phi_1)$y)
    # Simulate if large claim 
    results$n_large_claim1 <- rbinom(n = nrow(results), size = results$claim_count_1, prob = p_1_large)
    results$n_large_claim2 <- rbinom(n = nrow(results), size = results$claim_count_2, prob = p_2_large)
    # Number of small claims
    results$n_small_claim1 <- results$claim_count_1 - results$n_large_claim1
    results$n_small_claim2 <- results$claim_count_2 - results$n_large_claim2

    # Create long format results
    long_results <- data.frame()
    for (i in 1:nrow(results)){
        # Add large claims from branch 1
        if (results$n_large_claim1[i] > 0){
            sample = rnorm(n = results$n_large_claim1[i], mean = fit_1_large$estimate[1], sd = fit_1_large$estimate[2])
            long_results <- rbind(long_results, data.frame(ClaimDay = results$ClaimDay[i], ClaimType = 1, claims = sample))
        }
        # Add small claims from branch 1
        if (results$n_small_claim1[i] > 0){
            sample = rlnorm(n = results$n_small_claim1[i], meanlog = fit_1_small$estimate[1], sdlog = fit_1_small$estimate[2])
            long_results <- rbind(long_results, data.frame(ClaimDay = results$ClaimDay[i], ClaimType = 1, claims = sample))
        }
        # Add large claims from branch 2
        if (results$n_large_claim2[i] > 0){
            sample = rnorm(n = results$n_large_claim2[i], mean = fit_2_large$estimate[1], sd = fit_2_large$estimate[2])
            long_results <- rbind(long_results, data.frame(ClaimDay = results$ClaimDay[i], ClaimType = 2, claims = sample))
        }
        # Add small claims from branch 2
        if (results$n_small_claim2[i] > 0){
            sample = rlnorm(n = results$n_small_claim2[i], meanlog = fit_2_small$estimate[1], sdlog = fit_2_small$estimate[2])
            long_results <- rbind(long_results, data.frame(ClaimDay = results$ClaimDay[i], ClaimType = 2, claims = sample))
        }
    }
    return (long_results)
}

# See if output distributions match
results <- simulate_claims(10)
# Create histogram over simulated count of claims
q <- results %>% filter(ClaimType==1) %>% group_by(ClaimDay) %>% summarise(claim_count_1 = n())  %>%
    ggplot(aes(x = claim_count_1)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Histogram over claim count for claim type 1", x = "Claim count", y = "Frequency")
ggsave("plots/simulation/histogram_claim_count1.png", plot = q, device = "png")

# Create histogram over simulated count of claims
q <- results %>% filter(ClaimType==2) %>% group_by(ClaimDay) %>% summarise(claim_count_2 = n())  %>%
    ggplot(aes(x = claim_count_2)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Histogram over claim count for claim type 2", x = "Claim count", y = "Frequency")
ggsave("plots/simulation/histogram_claim_count2.png", plot = q, device = "png")

# Evaluate the sum of simulations
set.seed(1337)
# Create histogram over simulated total claim cost
res <- data.frame()
for (i in 1:1000){
    sim_claim <- simulate_claims(10)
    sim_claim_1 <- sim_claim[sim_claim$ClaimType == 1,]
    sim_claim_2 <- sim_claim[sim_claim$ClaimType == 2,]
    # Calculate small claims 
    tot_claims_small_1 <- sum(sim_claim_1$claims[sim_claim_1$claims <= 100000])
    tot_claims_small_2 <- sum(sim_claim_2$claims[sim_claim_2$claims <= 1000000])
    tot_claims_large_1 <- sum(sim_claim_1$claims[sim_claim_1$claims > 100000])
    tot_claims_large_2 <- sum(sim_claim_2$claims[sim_claim_2$claims > 1000000])
    # add to res
    res <- rbind(res, data.frame(tot_claims_small_1, tot_claims_small_2, tot_claims_large_1, tot_claims_large_2))
} 

# Calculate tot_claims
res$tot_claims_1 <- res$tot_claims_small_1 + res$tot_claims_large_1
res$tot_claims_2 <- res$tot_claims_small_2 + res$tot_claims_large_2
res$tot_claims <- res$tot_claims_1 + res$tot_claims_2

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_1)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 1", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims1$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost1.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_2)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 2", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims2$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost2.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_small_1)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 1", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims1_small$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost_small1.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_small_2)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 2", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims2_small$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost_small2.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_large_1)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 1", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims1_large$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost_large1.png", plot = q, device = "png")

# Plot distribution
q <- res %>% ggplot(aes(x = tot_claims_large_2)) +
    geom_histogram(bins = 40) +
    labs(title = "Histogram over total claim cost for branch 2", x = "Total claim cost", y = "Frequency")+
    geom_vline(xintercept = sum(claims2_large$ClaimCost ), color = "red")
ggsave("plots/simulation/histogram_total_claim_cost_large2.png", plot = q, device = "png")

#################### Reinsuranecs ####################
set.seed(1337)

########## Reinsurance Cover 1 ##########
# Estimate top 10 percentile
q_1_10 <- c()
q_2_10 <- c()
exp_cost_1 <- c()
exp_cost_2 <- c()
q_10 <- c()
for (i in 1:100){
    sim_claim <- simulate_claims(1)
    sim_claim_1 <- sim_claim[sim_claim$ClaimType == 1,]
    sim_claim_2 <- sim_claim[sim_claim$ClaimType == 2,]
    q_1_10s <- quantile(sim_claim_1$claims, 0.9)
    q_2_10s <- quantile(sim_claim_2$claims, 0.9)
    exp_cost_1 <- rbind(exp_cost_1, sum(sim_claim_1$claims[sim_claim_1$claims > q_1_10s]-q_1_10s))
    exp_cost_2 <- rbind(exp_cost_2, sum(sim_claim_2$claims[sim_claim_2$claims > q_2_10s]-q_2_10s))
    q_1_10 <- rbind(q_1_10, q_1_10s)
    q_2_10 <- rbind(q_2_10, q_2_10s)
}
quantiles = data.frame(q_1_10, q_2_10)
colnames(quantiles) <- c("b1", "b2")

# Calculate cut-off points
cut_off_1 <- mean(q_1_10)
cut_off_2 <- mean(q_2_10)

# Assume that the reinsurer prices a reinsurance contract according to 120% of the expected cost for the reinsurer given the given cut-off.
expected_cost_1 <- mean(exp_cost_1)
expected_cost_2 <- mean(exp_cost_2)

# Create histogram over the simulated 90% percentile
q <- quantiles %>% ggplot(aes(x = b1)) +
    geom_histogram(bins=20) +
    labs(title = "Histogram over 90% quantile for branch 1", x = "Estimated 90% quantile", y = "Frequency")+
    geom_vline(xintercept = cut_off_1, color = "red")
ggsave("plots/reinsurance1/histogram_over_90p_quantile_1.png", plot = q, device = "png")

# Create histogram over the simulated 90% percentile
q <- quantiles %>% ggplot(aes(x = b2)) +
    geom_histogram(bins=20) +
    labs(title = "Histogram over 90% quantile for branch 2", x = "Estimated 90% quantile", y = "Frequency")+
    geom_vline(xintercept = cut_off_2, color = "red")
ggsave("plots/reinsurance1/histogram_over_90p_quantile_2.png", plot = q, device = "png")

# Show effect of 120% Reinsurance from cut-off point
sim_claim <- simulate_claims(1)
sim_claim_1 <- sim_claim[sim_claim$ClaimType == 1, ]
sim_claim_2 <- sim_claim[sim_claim$ClaimType == 2, ]
sim_claim_1$covered_by_reinsurance_1 <- (cut_off_1 < sim_claim_1$claims)
q <- sim_claim_1 %>% ggplot(aes(x = claims, fill = covered_by_reinsurance_1)) +
    geom_histogram(bins = 50) +
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    labs(title = "Histogram over total claim cost for branch 1 with Reinsurance Cover 1", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggplot2::ggsave("plots/reinsurance1/histogram_total_claim_cost_reinsurance1.png", plot = q, device = "png")

sim_claim_2$covered_by_reinsurance_2 <- (cut_off_2 < sim_claim_2$claims)
q <- sim_claim_2 %>% ggplot(aes(x = claims, fill = covered_by_reinsurance_2)) +
    geom_histogram(bins = 50) +
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    labs(title = "Histogram over total claim cost for branch 2 with Reinsurance Cover 2", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggplot2::ggsave("plots/reinsurance1/histogram_total_claim_cost_reinsurance2.png", plot = q, device = "png")

reinsurance_price_1_1 = 1.2 * expected_cost_1
reinsurance_price_1_2 = 1.2 * expected_cost_2

# For each branch, illustrate how the distribution of total claim cost changes upon buying XL-reinsurance
total_cost <- c()
total_cost_with_reinsurance <- c()
for (i in 1:100){
    sim_claim <- simulate_claims(1)
    sim_claim_1 <- sim_claim[sim_claim$ClaimType == 1, ]
    sim_claim_2 <- sim_claim[sim_claim$ClaimType == 2, ]
    sim_claim_1$covered_by_reinsurance_1 <- (cut_off_1 < sim_claim_1$claims)
    sim_claim_2$covered_by_reinsurance_2 <- (cut_off_2 < sim_claim_2$claims)
    total_cost <- rbind(total_cost, sum(sim_claim$claims))
    # Simulate cost with reinsurance
    sim_claim_1$claims[sim_claim_1$covered_by_reinsurance_1] <- q_1_10
    sim_claim_2$claims[sim_claim_2$covered_by_reinsurance_2] <- q_2_10
    total_cost_w_reinsurance <- sum(sim_claim_1$claims) + sum(sim_claim_2$claims) + reinsurance_price_1_1 + reinsurance_price_1_2
    # Add total expected cost
    total_cost_with_reinsurance <- rbind(total_cost_with_reinsurance, total_cost_w_reinsurance)
}

tot_cost$with_reinsurance <- tot_cost$with_reinsurance + reinsurance_price_1_1 + reinsurance_price_1_2

# Create histogram over the simulated 90% percentile
tot_cost <- data.frame(total_cost, total_cost_with_reinsurance)
colnames(tot_cost) <- c("without_reinsurance", "with_reinsurance")

# Plot histogram over total claims with and without reinsurance as 2 hues
q <- melt(tot_cost) %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_histogram(binwidth = 5000000) +
    scale_fill_manual(values = c("with_reinsurance" = "#363636", "without_reinsurance" = "#606060")) +
    labs(title = "Histogram over total claim cost with and without Reinsurance Cover 1", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance1/histogram_total_claim_cost_with_reinsurance.png", plot = q, device = "png")

# Plot histogram over savings
tot_cost$savings <- tot_cost$without_reinsurance - tot_cost$with_reinsurance
q <- tot_cost %>% ggplot(aes(x = savings)) +
    geom_histogram(binwidth = 10000000) +
    labs(title = "Histogram over savings from Reinsurance Cover 1", x = "Savings", y = "Frequency")
ggsave("plots/reinsurance1/histogram_savings.png", plot = q, device = "png")

########## Reinsurance Cover 2 ##########
# Simulate total claim cost for the 2 branches
tot_claims_1 <- c()
tot_claims_2 <- c()
for (i in 1:100){
    sim_claims <- simulate_claims(1)
    sim_claims_1 <- sim_claims[sim_claims$ClaimType == 1,]
    sim_claims_2 <- sim_claims[sim_claims$ClaimType == 2,]
    tot_claims_1 <- rbind(tot_claims_1, sum(sim_claims_1$claims))
    tot_claims_2 <- rbind(tot_claims_2, sum(sim_claims_2$claims))
}

# Calculate cut-off points
cut_off_1 <- quantile(tot_claims_1, 0.9)
cut_off_2 <- quantile(tot_claims_2, 0.9)

tot_claims = data.frame(tot_claims_1, tot_claims_2)
colnames(tot_claims) <- c("b1", "b2")

# Calculate with reinsurance
tot_claims$covered_by_reinsurance_2_1 <- (cut_off_1 < tot_claims$b1)
tot_claims$covered_by_reinsurance_2_2 <- (cut_off_2 < tot_claims$b2)

q <- tot_claims %>% ggplot(aes(x = b1, fill = covered_by_reinsurance_2_1)) +
    geom_histogram(bins=20) +
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    labs(title = "Histogram over total claim cost for branch 1 with Reinsurance Cover 2", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance2/histogram_total_claim_cost_reinsurance2_1.png", plot = q, device = "png")

q <- tot_claims %>% ggplot(aes(x = b2, fill = covered_by_reinsurance_2_2)) +
    geom_histogram(bins=20) +
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    labs(title = "Histogram over total claim cost for branch 2 with Reinsurance Cover 2", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance2/histogram_total_claim_cost_reinsurance2_2.png", plot = q, device = "png")

# Calculate reinsurance premium
reinsurance_price_2_1 = 1.2 * (mean(tot_claims$b1[tot_claims$covered_by_reinsurance_2_1])-cut_off_1)
reinsurance_price_2_2 = 1.2 * (mean(tot_claims$b2[tot_claims$covered_by_reinsurance_2_2])-cut_off_2)

# Simulate the total
total_cost <- c()
total_cost_with_reinsurance <- c()
for (i in 1:100){
    sim_claims <- simulate_claims(1)
    sim_claims_1 <- sim_claims[sim_claims$ClaimType == 1,]
    sim_claims_2 <- sim_claims[sim_claims$ClaimType == 2,]
    tot_claims_1 <- sum(sim_claims_1$claims)
    tot_claims_2 <- sum(sim_claims_2$claims)
    tot_claims_1_with_reinsurance <- min(tot_claims_1, cut_off_1)+reinsurance_price_2_1
    tot_claims_2_with_reinsurance <- min(tot_claims_2, cut_off_2)+reinsurance_price_2_2
    total_cost <- rbind(total_cost, tot_claims_1 + tot_claims_2)
    total_cost_with_reinsurance <- rbind(total_cost_with_reinsurance, tot_claims_1_with_reinsurance + tot_claims_2_with_reinsurance)
}

# histogram over insurance cost
tot_cost <- data.frame(total_cost, total_cost_with_reinsurance)
colnames(tot_cost) <- c("with_reinsurance", "without_reinsurance")

# Plot histogram over total claims with and without reinsurance as 2 hues
q <- melt(tot_cost) %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_histogram(bins=20) +
    scale_fill_manual(values = c("with_reinsurance" = "#363636", "without_reinsurance" = "#606060")) +
    labs(title = "Histogram over total claim cost with and without Reinsurance Cover 2", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance2/histogram_total_claim_cost_with_reinsurance.png", plot = q, device = "png")

# Plot histogram over savings
tot_cost$savings <- tot_cost$with_reinsurance - tot_cost$without_reinsurance
q <- tot_cost %>% ggplot(aes(x = savings)) +
    geom_histogram(bins=20) +
    labs(title = "Histogram over savings from Reinsurance Cover 2", x = "Savings", y = "Frequency")
ggsave("plots/reinsurance2/histogram_savings.png", plot = q, device = "png")


########## Reinsurance Cover 3 ##########
# Simulate total claim cost for the 2 branches
tot_claims <- c()
for (i in 1:100) {
    simulate_data <- simulate_claims(1)
    tot_claims <- rbind(tot_claims, sum(simulate_data$claims))
}

# Calculate cut-off points
cut_off <- quantile(tot_claims, 0.9)
tot_claims = data.frame(tot_claims)
colnames(tot_claims) <- c("total_claim_cost")

# Calculate with reinsurance
tot_claims$covered_by_reinsurance_3 <- (cut_off < tot_claims$total_claim_cost)

q <- tot_claims %>% ggplot(aes(x = total_claim_cost, fill = covered_by_reinsurance_3)) +
    geom_histogram(bins=15) +
    scale_fill_manual(values = c("FALSE" = "#363636", "TRUE" = "#606060")) +
    labs(title = "Histogram over total claim cost with Reinsurance Cover 3", x = "Total claim cost", y = "Frequency")+
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance3/histogram_total_claim_cost_reinsurance3.png", plot = q, device = "png")

# Calculate reinsurance premium
reinsurance_price_3 <- 1.2 * (mean(tot_claims$total_claim_cost[tot_claims$covered_by_reinsurance_3]) - cut_off)

# Simulate this total
total_cost <- c()
total_cost_with_reinsurance <- c()
for (i in 1:100){
    simulate_data <- simulate_claims(1)
    tot_claims <- sum(simulate_data$claims)
    total_cost_wth_r <- min(tot_claims, cut_off) + reinsurance_price_3
    total_cost <- rbind(total_cost, tot_claims)
    total_cost_with_reinsurance <- rbind(total_cost_with_reinsurance, total_cost_wth_r)
}
reinsurance_price_3
#tot_claims histogram over insurance cost
tot_cost <- data.frame(total_cost, total_cost_with_reinsurance)
colnames(tot_cost) <- c("with_reinsurance", "without_reinsurance")

# Plot histogram over total claims with and without reinsurance as 2 hues
q <- melt(tot_cost) %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_histogram(bins = 25) +
    scale_fill_manual(values = c("with_reinsurance" = "#363636", "without_reinsurance" = "#606060")) +
    labs(title = "Histogram over total claim cost with and without Reinsurance Cover 3", x = "Total claim cost", y = "Frequency") +
    theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("plots/reinsurance3/histogram_total_claim_cost_with_reinsurance.png", plot = q, device = "png")

# Plot histogram over savings
tot_cost$savings <- tot_cost$with_reinsurance - tot_cost$without_reinsurance
q <- tot_cost %>% ggplot(aes(x = savings)) +
    geom_histogram(binwidth = 1000000) +
    labs(title = "Histogram over savings from Reinsurance Cover 3", x = "Savings", y = "Frequency")
ggsave("plots/reinsurance3/histogram_savings.png", plot = q, device = "png")
