suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

# Get the disinct levels of a few of the variables
vars <- starwars %>% 
          summarise_all(n_distinct) %>% 
          tidyr::gather("field", "n_levels") %>% 
          filter(n_levels < 20) %>% 
          mutate(levels = purrr::map(field, ~pull(distinct(select(starwars, .))))) 

vars

# assign some global variables to use in next pipeline
hair_color <- "black"
eye_color <- "black"
gender <- "female"

# for each row, create an expression and evaluate it.
vars2 <- vars %>% 
     mutate(my_expr = map2_chr(field, levels, ~paste0(.x, ' %in% c("', paste(.y[1:3], collapse = '","'), '")'))) %>% 
     mutate(my_expr = map(my_expr, ~rlang::parse_expr(.))) %>% 
     mutate(result = map_lgl(my_expr, ~eval(., envir = .GlobalEnv)))
     
# This works!
vars2$my_expr
vars2$result


# However to get this to work I had to create my expression as a string and then parse it.
# I can't seem to do the same thing using tidy eval. 
# I want to skip the step of creating a string and then parsing it.

vars %>% 
     mutate(my_expr = map2(field, levels, ~rlang::expr(!!.x %in% !!.y[1:3]))) 
     
vars %>% 
     rowwise() %>% 
     mutate(my_expr = rlang::expr(!!field %in% !!levels[1:3])) 
     
    
# I seems that rlang::expr does not know where to look up the unquoted variables. (???)


################################
     
     #extra code
#      
# df <- starwars %>% 
#      select(name, skin_color) %>% 
#      dplyr::mutate(my_expr = c(rlang::expr(paste(name, "has", skin_color, "skin.")))) %>% 
#      dplyr::mutate(evaluated = eval(my_expr)) #purrr::map(expr, ~(eval(.))))  
# df
# 
# # create variables in the global enviroment given a random value
# set.seed(1)
# pivot_vars %>% 
#      mutate(random_value = map_chr(levels, ~.[round(runif(1, min= 1, max = length(.)))])) %>% 
#      {walk2(.$field, .$random_value, ~assign(.x, .y, envir = .GlobalEnv))}