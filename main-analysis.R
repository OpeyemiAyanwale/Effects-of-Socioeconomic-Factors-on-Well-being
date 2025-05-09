library(dplyr)
library(ggplot2)
library(brms)
library(shinystan)
library(tidybayes)
library(xtable)
library(knitr)
library(MASS)


### Charts font size
theme_set(
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title size, bold, centered
      axis.title.x = element_text(size = 16),  # X-axis label size
      axis.title.y = element_text(size = 16),  # Y-axis label size
      axis.text.x = element_text(size = 13),   # X-axis tick labels size
      axis.text.y = element_text(size = 13)    # Y-axis tick labels size
    )
)

### Load data
df <- read.csv("germany.csv", sep=';')
df_USA <- read.csv("usa.csv", sep=';')
df_china <- read.csv("china.csv", sep=';')
df_trend <- read.csv("filtered_germany_data.csv")
View(df)


# Select columns by name
df_germ <- df[, c( "Q288R", "Q53", "Q49", "Q47", "Q50")]
df_usa <- df_USA[, c("Q49", "Q47", "Q50")]
df_chi <- df_china[, c("Q49", "Q47", "Q50")]

# Rename columns
colnames(df_germ) <-c('income_level', 'access_to_health', 'satisfaction_with_life', 'state_of_health', 'satisfaction_with_financial_status')
colnames(df_usa) <-c('satisfaction_with_life', 'state_of_health', 'satisfaction_with_financial_status')
colnames(df_chi) <-c('satisfaction_with_life', 'state_of_health', 'satisfaction_with_financial_status')

# Check for missing data
sum(is.na(df_germ))
sum(is.na(df_usa))
sum(is.na(df_chi))
summary(df_germ)

# Remove rows with any negative values which are missing values
df_germ <- df_germ[apply(df_germ, 1, function(row) all(row >= 0)), ]
df_usa <- df_usa[apply(df_usa, 1, function(row) all(row >= 0)), ]
df_chi <- df_chi[apply(df_chi, 1, function(row) all(row >= 0)), ]
df_trend <- df_trend[apply(df_trend, 1, function(row) all(row >= 0)), ]

summary(df_trend)

# recoding variable state of health

df_germ <- df_germ %>%
  mutate(state_of_health = case_when(
    state_of_health == 5 ~ 1,
    state_of_health == 4 ~ 2,
    state_of_health == 3 ~ 3,
    state_of_health == 2 ~ 4,
    state_of_health == 1 ~ 5,
    TRUE ~ state_of_health # Preserve any other values if they exist
  ))

df_usa <- df_usa %>%
  mutate(state_of_health = case_when(
    state_of_health == 5 ~ 1,
    state_of_health == 4 ~ 2,
    state_of_health == 3 ~ 3,
    state_of_health == 2 ~ 4,
    state_of_health == 1 ~ 5,
    TRUE ~ state_of_health # Preserve any other values if they exist
  ))

df_chi <- df_chi %>%
  mutate(state_of_health = case_when(
    state_of_health == 5 ~ 1,
    state_of_health == 4 ~ 2,
    state_of_health == 3 ~ 3,
    state_of_health == 2 ~ 4,
    state_of_health == 1 ~ 5,
    TRUE ~ state_of_health # Preserve any other values if they exist
  ))

# Check the summary to confirm the transformation
summary(df_germ)

str(df_germ)


######################### DESCRIPTIVE ANALYSIS #####################################

## Relationship between satisfaction with life and income
# Create a contingency table
table_data <- as.data.frame(table(df_germ$income_level, df_germ$satisfaction_with_life))

# Heatmap
ggplot(table_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Level of income", y = "Satisfaction with life", fill = "Frequency") 


cor(df_germ$satisfaction_with_life, df_germ$income_level, method = 'spearman')

## Relationship between satisfaction with life and access to healthcare

# Create a contingency table
table_data1 <- as.data.frame(table(df_germ$access_to_health, df_germ$satisfaction_with_life))

# Heatmap
ggplot(table_data1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Access to healthcare", y = " Satisfaction with life", fill = "Frequency") 

cor(df_germ$satisfaction_with_life, df_germ$ access_to_health, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data2 <- as.data.frame(table(df_germ$state_of_health, df_germ$satisfaction_with_life))

# Heatmap
ggplot(table_data2, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "State of health", y = "Satisfaction with life", fill = "Frequency")


cor(df_germ$satisfaction_with_life, df_germ$state_of_health, method = 'spearman')

## Relationship between satisfaction with life and Satisfaction with financial situation

# Create a contingency table
table_data3 <- as.data.frame(table(df_germ$satisfaction_with_financial_status, df_germ$satisfaction_with_life))

# Heatmap
ggplot(table_data3, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Satisfaction with financial situation ", y = "Satisfaction with life", fill = "Frequency") 

cor(df_germ$satisfaction_with_life, df_germ$satisfaction_with_financial_status, method = 'spearman')


############## OBJECTIVE 1 #######################
set.seed(200)

#### PRIOR DEFINITION #####

prior_def <- prior(normal(0, 2), class = "b") +
  prior(normal(0, 1), class = "Intercept")


### Bayesian analysis ####

fit_cm_linear <- brm(
  satisfaction_with_life ~ 1 + income_level + access_to_health + state_of_health + satisfaction_with_financial_status,
  data = df_germ,
  family = cumulative("probit"),
  prior = prior_def,
  cores = 4
)



### Model diagnostic
plot(fit_cm_linear)
summary(fit_cm_linear)

### extract into latext table
table1 = summary(fit_cm_linear)$fixed
xtable(table1)


### Effects of each parameter on Satisfacion with life
plot(conditional_effects(fit_cm_linear, 'income_level', categorical = TRUE))
plot(conditional_effects(fit_cm_linear, 'access_to_health', categorical = TRUE))
plot(conditional_effects(fit_cm_linear, 'state_of_health', categorical = TRUE))
plot(conditional_effects(fit_cm_linear, 'satisfaction_with_financial_status', categorical = TRUE))

### Compute the empirical CDF to compare how each parameter affect Satisfacion with life
#### Extract posterior samples
posterior_samples <- as_draws_df(fit_cm_linear)

ggplot() +
  stat_ecdf(data = data.frame(x = posterior_samples$b_income_level, var = "Income Level"), 
            aes(x, color = var), geom = "step") +
  stat_ecdf(data = data.frame(x = posterior_samples$b_access_to_health, var = "Access to Health"), 
            aes(x, color = var), geom = "step") +
  stat_ecdf(data = data.frame(x = posterior_samples$b_state_of_health, var = "State of Health"), 
            aes(x, color = var), geom = "step") +
  stat_ecdf(data = data.frame(x = posterior_samples$b_satisfaction_with_financial_status, var = "Financial Satisfaction"), 
            aes(x, color = var), geom = "step") +
  scale_color_manual(values = c("Income Level" = "blue", "Access to Health" = "red", 
                                "State of Health" = "green", "Financial Satisfaction" = "purple")) +
  labs(title = "Cumulative Distributions of Posterior Estimates",
       x = "Coefficient Value",
       y = "Cumulative Probability",
       color = "Predictor") +
  theme_minimal()

############### OBJECTIVE 2 ###########################
sub_germ <- df_germ[, c('satisfaction_with_life', 'state_of_health', 'satisfaction_with_financial_status')]
sub_germ$country <- 'Germany'
df_usa$country <- 'USA'
df_chi$country <- 'China'
list_of_dfs <- list(sub_germ, df_usa, df_chi)
main_df <- do.call(rbind, list_of_dfs)
main_df$country <- as.factor(main_df$country)
summary(main_df)
## Descriptive analysis

# Calculate proportions for each country and satisfaction level
df_prop <- main_df %>%
  group_by(country, satisfaction_with_life) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

df_prop2 <- main_df %>%
  group_by(country, satisfaction_with_financial_status) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

df_prop3 <- main_df %>%
  group_by(country, state_of_health) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

# Create heatmap based on proportions
ggplot(df_prop, aes(x = country, y = factor(satisfaction_with_life), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "Satisfaction with Life"
  ) 

# Create heatmap based on proportions
ggplot(df_prop2, aes(x = country, y = factor(satisfaction_with_financial_status), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "Satisfaction with financial situation"
  )

# Create heatmap based on proportions
ggplot(df_prop3, aes(x = country, y = factor(state_of_health), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "State of health"
  )

# Calculate medians by Country
median_table <- main_df %>%
  group_by(country) %>%
  summarize(
    Median_satisfaction_with_life = levels(as.factor(satisfaction_with_life))[median(as.numeric(satisfaction_with_life), na.rm = TRUE)],
    Median_state_of_health = levels(as.factor(state_of_health))[median(as.numeric(state_of_health), na.rm = TRUE)],
    Median_satisfaction_with_financial_status = levels(as.factor(satisfaction_with_financial_status))[median(as.numeric(satisfaction_with_financial_status), na.rm = TRUE)]
  )
median_table
## Bayesian analysis ##

### Prior definition
prior_def_multilevel <- prior(normal(0, 2), class = "b") +         # Priors for fixed effects
  prior(normal(0, 2), class = "Intercept") +              # Prior for intercept
  prior(normal(0, 1), class = "sd")                 # Prior for random effects              

fit_multilevel <- brm(
  formula = satisfaction_with_life ~ state_of_health + satisfaction_with_financial_status + (1 | country),
  data = main_df, 
  family = cumulative(link = "probit"),
  prior = prior_def_multilevel1, 
  control = list(adapt_delta = 0.99999, max_treedepth = 20),
  chains = 4, iter = 2000, cores = 4
)

### Model diagnostic
plot(fit_multilevel)
summary(fit_multilevel)

### extract into latext table
table2 = summary(fit_multilevel)$fixed
xtable(table2, caption = "Summary of multilevel model", label = "tab:multi")

plot(conditional_effects(fit_multilevel, 'state_of_health', categorical = TRUE))
plot(conditional_effects(fit_multilevel, 'satisfaction_with_financial_status', categorical = TRUE))
plot(conditional_effects(fit_multilevel, 'country', categorical = TRUE, re_formula = NULL))



# Extract posterior draws
post_samples <- as_draws_df(fit_multilevel)

post_samples<- post_samples[, c( "r_country[Germany,Intercept]", "r_country[China,Intercept]", "r_country[USA,Intercept]")]


# Rename columns for clarity
colnames(post_samples) <- c("Germany", "China", "USA")


# Compute pairwise differences
post_samples <- post_samples %>%
  mutate(
    diff_Germany_USA = Germany - USA,
    diff_Germany_China = Germany - China,
    diff_USA_China = USA - China
  )

# Compute pairwise differences
post_samples <- post_samples %>%
  mutate(
    diff_Germany_USA = Germany - USA,
    diff_Germany_China = Germany - China,
  )

# Reshape for ggplot
post_long <- post_samples %>%
  pivot_longer(
    cols = c(diff_Germany_USA, diff_Germany_China),
    names_to = "Comparison",
    values_to = "Difference"
  )

# Check the structure
head(post_long)

# Plot density of pairwise differences
ggplot(post_long, aes(x = Difference, fill = Comparison)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ Comparison, scales = "free_x") +
  labs(title = "Pairwise Differences in Life Satisfaction Between Countries",
       x = "Difference in Estimated Satisfaction",
       y = "Density") +
  theme_minimal()

################## OBJECTIVE 3 ##########################
## Descriptive analysis
table_data <- as.data.frame(table(df_trend$year, df_trend$satisfaction_life))

df_prop <- df_trend %>%
  group_by(year, satisfaction_life) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

ggplot(df_prop, aes(x = factor(year), y = factor(satisfaction_life), fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "",
    x = "Years",
    y = "Satisfaction with Life",
    fill = "Proportion"
  ) 
### Bayesian analysis
df_trend$year <- as.factor(df_trend$year)
prior_trend <- 
  prior(normal(0, 5), class = "b") +              # Weak prior for the effect of year
  prior(normal(0, 5), class = "Intercept")        # Weak prior for the intercept


bayesian_trend_model <- brm(
  formula = satisfaction_life ~ 1 + year,                           
  data = df_trend,                  
  family = cumulative(link = "probit"),  
  prior = prior_trend,
  chains = 4,                           
  iter = 2000,                         
  warmup = 1000,                      
  cores = 4                         
)
plot(bayesian_trend_model)

### extract into latext table
table3 = summary(bayesian_trend_model)$fixed
xtable(table3)


trend_plot <- conditional_effects(bayesian_trend_model, "year", categorical = TRUE)

# Create the plot
plot(trend_plot, xlab = "Wave")


# Convert the plot to a ggplot object
trend_plot_gg <- plot(trend_plot, plot = FALSE)[[1]] + 
  ggtitle("Trend of Satisfaction with Life in Germany Across WVS Waves") +
  xlab("Year") +
  ylab("Predicted Satisfaction with Life") +
  theme_minimal(base_size = 14)

# Show the plot
print(trend_plot_gg)


################## Comparing first model to a frequentist approach an ordinal probit regression #########


# Fit ordinal probit model
fit_frequentist <- polr(
  factor(satisfaction_with_life) ~ income_level + access_to_health + state_of_health + satisfaction_with_financial_status, 
  data = df_germ, 
  method = "probit"
)

# Get summary
summary_fit <- summary(fit_frequentist)
summary_fit
# Extract coefficient table
coef_table <- coef(summary_fit)

# Compute p-values using Wald test (z = Estimate / SE)
p_values <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))

# Add p-values to the coefficient table
coef_table <- cbind(coef_table, "p-value" = p_values)

# Convert to data frame
coefs_df <- as.data.frame(coef_table)

# Compute confidence intervals for predictors only
ci <- confint.default(fit_frequentist)  # Not using confint() to match predictors
ci_df <- as.data.frame(ci)
colnames(ci_df) <- c("l_95_CI", "u_95_CI")

# Ensure row alignment
predictor_names <- rownames(coefs_df)  # Get predictor names
ci_df <- ci_df[predictor_names, ]  # Select matching rows

# Merge everything into one table
coefs_df <- cbind(coefs_df, ci_df)

# Round values for better readability
coefs_df <- round(coefs_df, 4)

# Create LaTeX table
print(
  xtable(coefs_df, caption = "Probit Model Results", label = "tab:polr_results"),
  include.rownames = TRUE,
  sanitize.colnames.function = identity
)


version
toBibtex(citation("xtable"))

