library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(shinyAce)
library(rlang)
library(shinyWidgets)
library(purrr)
library(tools)

options(shiny.autoreload = TRUE)

# variables --------------------------------------------------------------------
cnf <- config::get()

safe_ggplot_env <- {
  
  safe_base_functions_names <- c(
    getGroupMembers("Math"),
    getGroupMembers("Arith"),
    getGroupMembers("Compare"),
    getGroupMembers("Logic"),
    getGroupMembers("Summary"),
    "{", "(", "ifelse", "::", "c", "[", "[[", "$"
  )
  
  safe_base_functions <- 
    map(safe_base_functions_names, ~ get(., "package:base")) %>% 
    set_names(safe_base_functions_names)
  
  ggplot_functions <- as.list(environment(ggplot2::ggplot))
  
  rlang::new_environment(
    data = c(safe_base_functions, ggplot_functions), 
    parent = rlang::empty_env()
  )
}


# functions --------------------------------------------------------------------
is_categorical <- function(x){
  is.factor(x) || is.logical(x) || is.character(x)
}

from_vars_to_choice_list <- function(x){
  if (is_empty(x)) return(list(NA))
  if (length(x) == 1) return(list(x[1]))
  x
}
