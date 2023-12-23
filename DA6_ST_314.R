install.packages(c("tidyverse", "infer"));
library(tidyverse);
library(infer);
microbrews = read_csv(file.choose());

ggplot(microbrews, aes(x = abv)) +
  geom_histogram(bins = 16) +
  labs(x = "Alcohol by Volume (%)",
       title = "ABV for craft beers");


point_est = mean(microbrews$abv);
hyp_val = 5; #From Null Hypotheis
SE_of_point_est = sd(microbrews$abv)/sqrt(nrow(microbrews));
test_stat = (point_est - hyp_val)/SE_of_point_est;

#pt(test_stat, df) will get us proportion to 
#  the left of the t-dist with df degrees of freedom

#with abs(test_stat) we make sure that the test_stat 
#  is on the right side of the curve (this works 
#  since the curve is centered at 0)

#then we calculate 1-pt(abs(test_stat), df)
#  to get the extreme to the right

#finally, since the alternative hypothesis is 
#  2-sided we multiply this value by 2

p_val = 2*(1-pt(abs(test_stat), nrow(microbrews)-1))

t.test(microbrews$abv, mu=5, alternative = "two.sided", conf.level = 1-0.01)


### PART 2: CO2 Emmisions
population = read_csv(file.choose())
mu_co2 = mean(population$CombCO2)
n = 45
sample <- population %>%
  sample_n(size = n);

x_bar = mean(sample$CombCO2);
SE_co2 = sd(sample$CombCO2)/sqrt(nrow(sample));
t_co2 = (x_bar - mu_co2) / SE_co2;

p_co2 = 2*(1-pt(abs(t_co2), nrow(sample)-1))


# Creates a dataframe of 10000 samples
sample_mean45 <- population %>%
  rep_sample_n(size = n, reps = 10000, replace = TRUE) %>%
  group_by(replicate) %>%
  summarise(mean = mean(CombCO2), sd = sd(CombCO2));

# plots the sample means
ggplot(sample_mean45, aes(x = mean)) +
  geom_histogram() + 
  labs(x = "Sample Means",
       title = "Distribution of 10000 sample means");

# creates a column for test statistics
sample_mean45 = sample_mean45 %>%
  mutate(t = (mean-mu_co2)/(sd/sqrt(n)))

#plots the test statistics
ggplot(sample_mean45, aes(x = t)) +
  geom_histogram() + 
  labs(x = "Sample Test Statistics",
       title = "Distribution of 10000 sample test statistics");

mean(sample_mean45$t);
sd(sample_mean45$t);

# creates a column for p-values (2-sided)
sample_mean45 = sample_mean45 %>%
  mutate(p = 2*(1-pt(abs(t), n-1)));

ggplot(sample_mean45, aes(x = p)) +
  geom_histogram(binwidth = 0.05,
                 boundary = 0,
                 color = "black",
                 fill = c(rep("#D55E00", 1), rep("#999999", 19))) +
  labs(x = "Sample P-Values",
       title = "Distribution of P-Values from 10000\nRandom Samples from the Population");

mean(sample_mean45$p <= 0.05) * 100
