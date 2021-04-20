##### To code-along with lecture 005

# Load packages
library(pacman)

#workhorse packages for display options/efficient sims
p_load(
  broom,
  latex2exp, ggplot2, ggthemes, viridis, extrafont,
  kableExtra,
  dplyr, magrittr, knitr, parallel
)


p_load(tidyverse, Ecdat) #Ecdat brings with it the 'Caschool' dataset
# Select and rename desired variables; assign to new dataset; format as tibble
test_df <- Caschool %>% select(
  test_score = testscr, ratio = str, income = avginc, enrollment = enrltot
) %>% as_tibble()
# View first 2 rows of the dataset
head(test_df, 2)

##### Produce our first plot
# Model 1: test ~ ratio + income
test_df %<>% mutate(e1 = lm(test_score ~ ratio + income, data = test_df) %>% residuals())

#build custom theme (you don't need to - just delete 'theme_axes_serif' if you don't do this)

theme_axes_serif <- theme_void() + theme(
  text = element_text(family = "MathJax_Main"),
  axis.title = element_text(size = 22),
  axis.title.x = element_text(hjust = .95, margin = margin(0.15, 0, 0, 0, unit = "lines")),
  axis.title.y = element_text(vjust = .95, margin = margin(0, 0.15, 0, 0, unit = "lines")),
  axis.line = element_line(
    color = "grey70",
    size = 0.25,
    arrow = arrow(angle = 30, length = unit(0.15, "inches")
    )),
  plot.margin = structure(c(1, 0, 1, 0), unit = "lines", valid.unit = 3L, class = "unit"),
  legend.position = "none"
)

# Plot
ggplot(data = test_df, aes(x = income, y = e1)) +
  geom_point(size = 3, alpha = 0.5, color = red_pink) +
  labs(x = "Income", y = TeX("\\textit{e}")) +
  theme_axes_serif

#############-++++Simulation data++++-###############

## The actual generation of the data uses a special loop - you don't need to really know how this works to get results - just run this code to get the data
#for the underlying plots


# Parameters
b0 <- 1
b1 <- 10
s2 <- 1
# Sample size
n <- 30
# Number of iterations
n_iter <- 1e4
# Set seed
set.seed(1234)
# The simulation
sim_df <- mclapply(X = 1:n_iter, FUN = function(i, size) {
  # Generate data
  sample_df <- tibble(
    x = runif(size, 0.5, 1.5),
    y = b0 + b1 * x + rnorm(size, 0, sd = s2 * x^2)
  )
  # OLS
  ols <- felm(y ~ x, data = sample_df)
  # WLS: Correct weights
  wls_t <- lm(y ~ x, data = sample_df, weights = 1/x^2)
  # WLS: Correct weights
  wls_f <- lm(y ~ x, data = sample_df, weights = 1/x)
  # Save results
  iter_df <- rbind(
    summary(ols, robust = F) %>% coef() %>% magrittr::extract(2,1:2),
    summary(ols, robust = T) %>% coef() %>% magrittr::extract(2,1:2),
    summary(wls_t) %>% coef() %>% magrittr::extract(2,1:2),
    summary(wls_f) %>% coef() %>% magrittr::extract(2,1:2)
  ) %>%
    as_tibble() %>%
    mutate(
      model = c("OLS Hom.", "OLS Het.", "WLS T", "WLS F"),
      iter = i
    )
  # Return the data
  return(iter_df)
}, mc.cores = 3, size = n) %>% bind_rows()
# Change names
names(sim_df) <- c("coef", "se", "model", "iter")

####### sim_df
sim_df %>% head(3)