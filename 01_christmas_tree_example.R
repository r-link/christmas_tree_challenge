###############################################################################
#
#     Christmas Tree Challenge example script
#
#     Contact: roman.link@plant-ecology.de
#
###############################################################################

# load packages
library(tidyverse)
library(rlang)

# load script
source("R/grow_tree.R")

# 1. Minimal example ----------------------------------------------------------
# define simple angle, length and survival functions
angle_fun1  <- function(angle) angle + c(-0.1 * pi, 0.1 * pi) 
# angle: parent angle +- 0.1pi 
length_fun1 <- function(length) 0.85 * length
# length: 85% of parent length
surv_fun1   <- function() 0.9
# survival probability: 90% for both children

# define random seed (for reproducibility)
set.seed(12345)

# grow your first tree (using 15 iterations)
tree1 <- grow_tree(n_iter     = 10, 
                   angle_fun  = angle_fun1(angle), 
                   length_fun = length_fun1(length), 
                   surv_fun   = surv_fun1(),
                   verbose    = FALSE) # do not print progress

# inspect the output
tree1

# plot with the plot_tree function
plot_tree(tree1, only_living = TRUE)
# ony_living excludes all dead segments from the plot

# 2. More realistic tree ------------------------------------------------------
# angle defined as a function of generation and parent angle;
# symmetric bifurcation with increasing average angle and variability in
# angles higher up in the canopy
angle_fun2  <- function(angle, generation){
  # weights that get bigger which each generation
  w  <- 1 - 1/generation
  # get absolute deviations from parent segment angle
  a0 <- rnorm(1, 0.12 * w * pi, 0.05 * w * pi)
  # return new angles with randomized order
  angle + sample(c(-a0, a0))
}

# length as a function of parent length, decreasing on average by 10 percent
# with stochastic variation
length_fun2 <- function(length) rnorm(2, 0.9, 0.05) * length
# survival function as a function of generation and number of iterations -
# one of the branches survives with a probability of 95%, the 'survival' 
# probability of the other branch increases with tree age (looks more
# realistic because it seems like broken off branches)
surv_fun2   <- function(generation, n_iter) c(.75 - (n_iter - generation[1]) / (2 * n_iter), .95)

# set random seed 
set.seed(999)

# grow tree for 25 iterations
tree2 <- grow_tree(n_iter = 25, 
                   angle_fun2(angle, generation), 
                   length_fun2(length), 
                   surv_fun2(generation, 25), # note that n_iter has to be 
                                        # specified explicitly because it
                                        # is not in the treetab
                    verbose = FALSE)

# plot tree
plot_tree(tree2)


# 3. Modifying plots of trees -------------------------------------------------
# clean the second tree
tree2_clean <- clean_tree(tree2)

# inspect the clean tree
tree2_clean

# create plot of clean tree manually
p1 <- tree2_clean %>% 
  ggplot(aes(x = x, y = y, group = id, 
             size = 1 / generation, 
             color = generation)) +
  geom_line(lineend = "round") +
  scale_color_tree() +
  theme_void() +
  theme(legend.position = "none") + 
  coord_equal()

# set random seed
set.seed(13579)
# extract 30 random basal ends of the terminal branches
baubles <- filter(tree2_clean, 
                  generation == 25,
                  pos == 0) %>% 
  sample_n(30)

# add baubles to the tree
p1 +
  # red baubles with black outlines
  geom_point(data = baubles, aes(x = x, y = y), 
             fill = "#AA1243", col = 1, pch = 21, size = 5) +
  # white transparent shine (with a small offset to avoid the center)
  geom_point(data = baubles, aes(x = x, y = y), 
             col = "white", alpha = 0.3, size = 1,
             position = position_nudge(x = -0.08, y = 0.08)
  )
