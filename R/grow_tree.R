###############################################################################
#
#     Christmas Tree Challenge - model scripts
#
#     Contact: roman.link@plant-ecology.de
#
###############################################################################

# init_tree() - function to initiate trees ------------------------------------
# this function generates the first segment of the tree starting from its
# root and defines the shape of the output tibbles
init_tree <- function(root, length0, angle0){
  # prepare output
  out <- tibble(
    generation = 1, 
    id = "1",
    length = length0,
    angle  = angle0,    # angle is defined as the angle to the y-axis 
    parent_id     = NA, #   (counter-clockwise!)
    parent_length = NA,
    parent_angle  = NA,
    x0 = root[1], 
    y0 = root[2],
    x1 = x0 - sin(angle) * length, 
    y1 = y0 + cos(angle) * length,
    surv_prob = 1,
    surv = TRUE)
  # return output as an object of class treetab (modified tibble)
  return(structure(out, class = c("treetab", class(out))))
}

# update_tree() - function to update trees ------------------------------------
# branch is supposed to be a treetab object containing data from a single 
#        generation
# angle_fun, length_fun and surv_fun are quasiquotated calls to user-specified
#        function calls
update_tree <- function(branch, angle_fun, length_fun, surv_fun){
  # abort if no living branches remain
  if(sum(branch$surv) == 0 | is.null(branch)) return(NULL) else
  # duplicate tibble (all branches bifurcate)
  bind_rows(branch, branch) %>% 
    # filter out dead branches
    filter(surv) %>% 
    # do automatic updates (generation, root position and parent properties)
    mutate(generation = generation + 1,
           parent_id = id, 
           parent_length = length, 
           parent_angle = angle,
           x0 = x1,
           y0 = y1) %>% 
    # group by parent to run user-specified update functions separately
    # for each node
    group_by(parent_id) %>% 
    # update ID, perform user-specified updates and reset end position
    mutate(id = paste0(id, 1:2),
           angle     = !!angle_fun,  # user specified functions evaluated with !!
           length    = !!length_fun,
           surv_prob = !!surv_fun,
           surv      = runif(2) < surv_prob,
           x1 = x0 - sin(angle) * length, 
           y1 = y0 + cos(angle) * length
           ) %>% 
    ungroup
}

# grow_tree() - function to grow trees ----------------------------------------
# n_iter - number of iterations
# angle_fun, length_fun, surv_fun - calls to user-specified functions that return
#   angle (radians), length and survival (TRUE or FALSE) - calls to any
#   type of function that returns either one value for both children of each node
#   or two values (one for each child) of the required class
# verbose and steps regulate if and how often the progress is printed
grow_tree <- function(n_iter, 
                      angle_fun, length_fun, surv_fun, 
                      root = c(0, 0), length0 = 1, angle0 = 0,
                      verbose = TRUE, steps = 1){
  # catch expressions for update functions
  angle_fun  <- enquo(angle_fun)
  length_fun <- enquo(length_fun)
  surv_fun   <- enquo(surv_fun)
  
  # preallocate list for output
  out      <- vector(mode = "list", length = n_iter)
  
  # initiate tree
  out[[1]] <- init_tree(root, length0, angle0)
  
  # check if the user-specified functions produce the desired output
   angle <- eval_tidy(angle_fun, data = out[[1]])
   if (!is.numeric(angle) | !(length(angle) %in% c(1,2))) 
     stop("angle_fun() does not return numeric of length 1 or 2.")
   length <- eval_tidy(length_fun, data = out[[1]])
   if (!is.numeric(length) | !(length(length) %in% c(1,2))) 
     stop("length_fun() does not return numeric of length 1 or 2.")
   surv_prob <- eval_tidy(surv_fun, data = out[[1]])
   if (!is.numeric(surv_prob) | any(surv_prob < 0 | surv_prob > 1) |
       !(length(surv_prob) %in% c(1,2))) 
     stop("surv_fun() does not return numeric vector (0 <= x <= 1) of length 1 or 2.")
   
  # loop over output to add new levels
  for (i in 2:n_iter){
    # print progress
    if (verbose){ if (i %% steps == 0) cat(i, "\n") }
    # update tree
    out[[i]] <- update_tree(out[[i - 1]], angle_fun, length_fun, surv_fun)
    # break loop if no more living branches exist
    if(sum(out[[i]]$surv) == 0) {
      cat("No surviving branches in iteration", i, "\n")
      break
      }
  }
  # bind output list into a single tibble
  bind_rows(out)
}

# clean_tree() -- function that prepares the trees for plotting ---------------
# this function prepares treedat objects for plotting (getting them into
# long table format)
# if only_living == TRUE, segments with surv == FALSE are excluded
clean_tree <- function(tree, only_living = TRUE){
  # check object type
  if (!is(tree, "treetab")) stop("Object is not a tree")
  # if necessary, remove dead segments
  if (only_living) tree <- filter(tree, surv)
  # get coordinates into long table format
  out <- tree %>% 
    gather(key, val, x0:y1) %>% 
    mutate(
      pos = parse_number(key),
      key = gsub("0|1", "", key)) %>% 
    spread(key, val)
  return(structure(out, class = c("clean_tree", class(out))))
}

# scale_color_tree() - starting point for tree color scales -------------------
scale_color_tree <- function(...){
  scale_color_gradientn(
    colours = c("#654321", "#804000", "#254117", "#347c2c"), ...) 
} 

# plot_tree() - function that automates simple tree plots ---------------------
# uses a tree object as input, converting to clean_tree if necessary
# the only_living argument is ignored if the input is already a clean_tree
plot_tree <- function(tree, only_living = TRUE){
  # check object type
  if (!inherits(tree, "treetab")) stop("Object is not a tree")
  # convert to clean_tree if necessary
  if(!is(tree, "clean_tree")){
    cat("Tree converted to class 'clean_tree' for plotting.")
    tree <- clean_tree(tree, only_living = only_living)
    }
  # make simple plot
  ggplot(tree, aes(x = x,
                   y = y,
                   group = id, 
                   size  = 1 / generation, 
                   color = generation)) +
    geom_line(lineend = "round") +
    scale_color_tree() +
    theme_void() +
    theme(legend.position = "none") + 
    coord_equal()
}

