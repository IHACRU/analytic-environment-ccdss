# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(knitr)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals -------------------------

path_input  <- "./data-unshared/derived/dto-ccdss-v2012.csv"
path_output <- ""
# ---- utility-functions -----------------------

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for the origin of the data
# ds <- readr::read_csv(path_input)
ds <- readRDS( gsub(".csv$",".rds",path_input) )
# ---- inspect-data ----------------------------
# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
ds %>% glimpse()
ds %>% dplyr::summarize_all(n_distinct) %>% t()
# ds %>% dplyr::distinct(variable, index,location, unit_count) %>% View()
# ---- tweak-data ------------------------------
ds %>% distinct(view)
factor_levels_view <- c(
  "Population"
  ,"Condition"
  ,"Mortality"
  ,"Hospitalization"
  ,"Physician"
  ,"General"
  ,"Specialist"
)

ds <- ds %>% 
  dplyr::mutate(
    view = factor(view, levels = factor_levels_view)
  )

# ---- basic-table -----------------------------




# ---- dev-b-0 ---------------------------------

index_mapping <- c(
  # index               color     linetype    shape 
   "Population"        , "black", "solid"
  ,"Prevalence"        , "blue",  "solid"
  ,"Incidence"         , "red"  , "solid"
  ,"Mortality with"    , "green", "solid"
  ,"Mortality without" , "violet", "dotted"
)
ds_index_mapping <- matrix(index_mapping,5,3,byrow=T) %>% as.data.frame()
names(ds_index_mapping) <- c("index","color","linetype")
ds_index_mapping
# create a function to map a target value to values of aesthetic attributes
map_attribute <- function(d,target, attribute){
  (target_values  <- d[,target] %>% as.character() )
  (mapping_values <- d[,attribute] %>% as.character() )
   map <- mapping_values 
   names(map) <-target_values
   return(map)
 }
map_attribute(ds_index_mapping, "index","color")  
# # what we need to get:
# color_values <- c(
#   
#    "Population"        = "black" 
#   ,"Incidence"         = "red"   
#   ,"Prevalence"        = "blue"  
#   ,"Mortality with"    = "brown" 
#   ,"Mortality without" = "brown" 
# )
# index_color <- map_attribute(ds_index_mapping,"index", "color")
  
# ---- dev-b-1 ---------------------------------

# select a data for a single unit (shows how unit is concieved)
value_over_age <- function(
  d               # generic product of the ellis island
  ,filter_disease  # condition in focus
  # ,filter_view     #
  ,filter_index    #
  ,filter_unit     # the units of the counts 
  ,filter_year     # fix to this point in time
  ,color_variable  # chose what variable to map to color
  ,attribute_map
){
  # values for testing
  # filter_disease = "Diabetes"
  # filter_view    = "Condition"
  # filter_index   <-  c(
  # "Population"
  # ,"Prevalence"
  # ,"Incidence"
  # ,"Mortality with"
  # ,"Mortality without"
  # )
  # filter_view    = c("Population","Condition","Mortality")
  # filter_unit    = "person"
  # filter_year    = 2003
  # color_variable = "index"
  # d <- ds
  # attribute_map <- ds_index_mapping
# browser()
  filter_criteria_index <- lazyeval::interp(~ which_column %in%  filter_index,    which_column = as.name("index"))
  
  # select data to display
  d1 <- d %>% 
    dplyr::mutate(
      # index = as.character(view),
      index = ordered(index, levels = ds_index_mapping[,"index"] %>% as.character() )
    ) %>%
    dplyr::filter(disease %in% filter_disease) %>% 
    # dplyr::filter(index %in% filter_index) %>% 
    dplyr::filter_(.dots =filter_criteria_index) %>%
    dplyr::filter(unit_count %in% filter_unit) %>% 
    dplyr::filter(year %in% filter_year) %>% 
    dplyr::mutate_(
      color_groups = color_variable
    ) 

  d1 %>% dplyr::glimpse() %>% print()
  g1 <- d1 %>% 
    ggplot(aes(x=age,y=value, color=color_groups, fill=color_groups))+
    geom_line(aes(group=variable))+
    geom_point(shape = 21, color = "white" )+
    scale_fill_manual( values = map_attribute(attribute_map, "index","color") )+
    scale_color_manual(values = map_attribute(attribute_map, "index","color") )+
    scale_y_continuous(labels = scales::comma)+
    facet_grid(. ~ sex)+
    theme_bw()+
    theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))
  g1
  return(g1)
}
# Usage demonstration
g1 <- ds %>% value_over_age(
  filter_disease = "Diabetes"
  # ,filter_view    = "Condition"
  # ,filter_view    = c("Population","Condition","Mortality")
  ,filter_index  = c("Population","Prevalence","Incidence","Mortality with","Mortality without")
  ,filter_unit    = "person"
  ,filter_year    = 2003
  ,color_variable = "index"
  ,attribute_map = ds_index_mapping
)
g1

ds %>% distinct(view)

# uses plot_coef() to create a matrix of plots
# matrix_coef <- function(
#   x,
#   term_group,
#   model_type,
#   process,
#   info_index="BIC"
# ){
  # x <- ds_catalog
  # model_type="aefb"
  # process ="mmse"
  # # # term_group = "level"
  # # # term_group = "error"
  # term_group ="info_index"
  ls <- list()
  ls[["Population"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                  # ,filter_view    = c("Population","Condition","Mortality")
                                                  ,filter_index  = c("Population","Prevalence","Incidence","Mortality with","Mortality without")
                                                  ,filter_unit    = "person"
                                                  ,filter_year    = 2003
                                                  ,color_variable = "index"
                                                  ,attribute_map  = ds_index_mapping
  )
  ls[["Condition"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                 # ,filter_view    = "Condition"
                                                 ,filter_index  = c("Prevalence","Incidence" )
                                                 ,filter_unit    = "person"
                                                 ,filter_year    = 2003
                                                 ,color_variable = "index"
                                                 ,attribute_map  = ds_index_mapping
  )
  ls[["Mortality"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                 # ,filter_view    = "Mortality"
                                                 ,filter_index  = c("Incidence", "Mortality with","Mortality without")
                                                 ,filter_unit    = "person"
                                                 ,filter_year    = 2003
                                                 ,color_variable = "index"
                                                 ,attribute_map  = ds_index_mapping
  )
  baseSize <- 10
  pm <- GGally::ggmatrix(
    ls,
    ncol = 1, nrow = length(ls),
    title = "Title",
    # xAxisLabels = names()
    yAxisLabels = names(ls),
    legend = 1
  ) + theme(
    legend.position = "right"#,
    # strip.text.x = element_text(size=baseSize+2)
    
  )
  pm
# }










# developmental below this point






# ---- dev-b-2 ---------------------------------
g2 <- d2 %>% 
  ggplot(aes(x=age, y=value, fill = condition_present, color = condition_present))+
  geom_point(shape = 21, size =1, fill = NA)+
  geom_line(aes(group=variable))+
  # geom_area()+
  facet_grid(. ~ sex, scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))
g2

# ---- dev-b-3 ---------------------------------
# select a data for a single unit (shows how unit is concieved)
value_over_age <- function(
  d               # generic product of the ellis island
  ,filter_disease  # condition in focus
  ,filter_view     #
  ,filter_unit     # the units of the counts 
  ,filter_year     # fix to this point in time
  ,color_variable  # chose what variable to map to color
){
  # values for testing
  # filter_disease = "Diabetes"
  # filter_view    = "Condition"
  # filter_unit    = "person"
  # filter_year    = 2003
  # color_variable = "index"
  # d <- ds
  
  # select data to display
  d1 <- d %>% 
    dplyr::filter(disease == filter_disease) %>% 
    # dplyr::filter(index == filter_index) %>% 
    dplyr::filter(view == filter_view) %>%
    dplyr::filter(unit_count == filter_unit) %>% 
    dplyr::filter(year == filter_year) %>% 
    dplyr::mutate_(
      color_groups = color_variable
    )
  d1 %>% dplyr::glimpse() %>% print()
  g1 <- d1 %>% 
    ggplot(aes(x=age,y=value, color=color_groups))+
    geom_line(aes(group=variable))+
    scale_color_manual(values =  map_attribute(ds_index_mapping,"index","color") )+
    facet_grid(. ~ sex)+
    theme_bw()+
    theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))
  g1
  return(g1)
  # return(d1)
  
}
# d1<- ds %>% value_over_age(
g1 <- ds %>% value_over_age(
  # filter_disease = "Mental_Illness"
  filter_disease = "Diabetes"
  # ,filter_view    = "Population"
  ,filter_view    = "Condition"
  ,filter_unit    = "person"
  ,filter_year    = 2003
  ,color_variable = "index"
)

# uses plot_coef() to create a matrix of plots
matrix_coef <- function(
  x,
  term_group,
  model_type,
  process,
  info_index="BIC"
){
  # x <- ds_catalog
  # model_type="aefb"
  # process ="mmse"
  # # # term_group = "level"
  # # # term_group = "error"
  # term_group ="info_index"
  ls <- list()
  ls[["Population"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                  ,filter_view    = "Population"
                                                  ,filter_unit    = "person"
                                                  ,filter_year    = 2003
                                                  ,color_variable = "index"
  )
  ls[["Condition"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                 ,filter_view    = "Condition"
                                                 ,filter_unit    = "person"
                                                 ,filter_year    = 2003
                                                 ,color_variable = "index"
  )
  ls[["Mortality"]] <- ds %>% value_over_age(    filter_disease = "Diabetes"
                                                 ,filter_view    = "Mortality"
                                                 ,filter_unit    = "person"
                                                 ,filter_year    = 2003
                                                 ,color_variable = "index"
  )
  baseSize <- 10
  pm <- GGally::ggmatrix(
    ls,
    ncol = 1, nrow = length(ls),
    title = "Title",
    # xAxisLabels = names()
    yAxisLabels = names(ls),
    legend = 1
  ) + theme(
    legend.position = "right"#,
    # strip.text.x = element_text(size=baseSize+2)
    
  )
  pm
}

# ---- dev-b-4 ---------------------------------


# facets within graph
# select a data for a single unit (shows how unit is concieved)
value_over_age <- function(
  d               # generic product of the ellis island
  ,d_index_mapping
  ,filter_disease  # condition in focus
  ,filter_view     #
  ,filter_unit     # the units of the counts 
  ,filter_year     # fix to this point in time
  ,color_variable  # chose what variable to map to color
){
  # values for testing
  # filter_disease = "Diabetes"
  # filter_view    = "Condition"
  # filter_unit    = "person"
  # filter_year    = 2003
  # color_variable = "index"
  # d <- ds
  # d_index_mapping <- ds_index_mapping
  
  # select data to display
  d1 <- d %>% 
    dplyr::filter(disease == filter_disease) %>% 
    dplyr::filter(index == d_index_mapping[,"index"]) %>%
    # dplyr::filter(view == filter_view) %>%
    dplyr::filter(unit_count == filter_unit) %>% 
    dplyr::filter(year == filter_year) %>% 
    dplyr::mutate_(
      color_groups = color_variable
    )
  d1 %>% dplyr::glimpse() %>% print()
  g1 <- d1 %>% 
    ggplot(aes(x=age,y=value, color=color_groups))+
    geom_line(aes(group=variable))+
    scale_color_manual(values =  map_attribute(ds_index_mapping,"index","color") )+
    facet_grid(view ~ sex, scales = "free_y")+
    theme_bw()+
    theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))
  g1
  return(g1)
  # return(d1)
  
}
# d1<- ds %>% value_over_age(
g1 <- ds %>% value_over_age(
  # filter_disease = "Mental_Illness"
  filter_disease = "Diabetes"
  # ,filter_view    = "Population"
  ,filter_view    = "Condition"
  ,filter_unit    = "person"
  ,filter_year    = 2003
  ,color_variable = "index"
)

# ---- dev-b-5 ---------------------------------







# ---- dev-a-0 ---------------------------------
# see ./data-unshared/contents.md for origin of the data
# see http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html for data dictionary
# d %>% glimpse()

# ---- dev-a-1 ---------------------------------
  # let us focus a the most basic graph we can think of
  # that represents what we are after: trends over time
  d1 <- ds %>% 
    dplyr::filter(disease == "Mental_Illness") %>% 
    dplyr::filter(index   == "Hospitalization") %>% 
    dplyr::filter(unit_count == "person") %>% 
    dplyr::filter(condition_present)
  
  d1 %>% dplyr::summarize_all(n_distinct) %>% t()
  
  g1 <- d1 %>% 
    ggplot(aes(x=year,y=value)) +
    geom_line(aes(group = age, color = quarter), stat="identity", size =3)+
    geom_text(aes(label = period))+
    scale_y_continuous(labels = scales::comma)+
    scale_color_brewer(type="qual")+
    # facet_grid(. ~ sex)+
    facet_grid(sex ~ condition_present)+
    labs(color = "Age group")+
    theme_minimal()
  g1
  
# ---- dev-a-2 ---------------------------------
  # make the graph above more flexible / turn it into a function
  # elemental_line <- function(
  #   d 
  # ){
  
  select_disease <- c("Mental_Illness")
  # select_disease <- c("MoodAnxiety_Disorder")
  # select_index <- c("Incidence")
  # select_index <- c("Prevalence")
  # select_index <- c("Mortality")
  select_index <- c("Hospitalization")
  # select_index <- c("Population")
  # select_location <- c("Population")
  select_unit_count <- c("person")
  select_condition_present <- c(TRUE)
  filter_criteria_disease           <- lazyeval::interp(~ which_column %in% select_disease,           which_column = as.name("disease"))
  filter_criteria_index             <- lazyeval::interp(~ which_column %in% select_index,             which_column = as.name("index"))
  filter_criteria_location          <- lazyeval::interp(~ which_column %in% select_location,          which_column = as.name("location"))
  filter_criteria_unit_count        <- lazyeval::interp(~ which_column %in% select_unit_count,        which_column = as.name("unit_count"))
  filter_criteria_condition_present <- lazyeval::interp(~ which_column %in% select_condition_present, which_column = as.name("condition_present"))
  d1 <- d %>% 
    dplyr::filter_(filter_criteria_disease) %>% 
    dplyr::filter_(filter_criteria_index) %>% 
    dplyr::filter_(filter_criteria_unit_count)
  # d1 %>% distinct(disease,year,sex,age,variable)  
  #   
  d1 %>% glimpse
  
  g1 <- d1 %>% 
    # dplyr::filter_(filter_criteria_location) %>% 
    # dplyr::filter_(filter_criteria_unit_count) %>%
    # dplyr::filter_(filter_criteria_condition_present) %>% 
    ggplot(aes_string(x="year",y="value")) +
    geom_line(aes_string(group = "age", color = "quarter"), stat="identity", size =3)+
    geom_text(aes_string(label = "period"))+
    scale_y_continuous(labels = scales::comma)+
    scale_color_brewer(type="qual")+
    # facet_grid(. ~ sex)+
    facet_grid(sex ~ condition_present)+
    labs(color = "Age group")+
    theme_minimal()
  g1
  
  # }
  # Usase
  # d %>% elemental_line()
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/ccdss/ccdss-v2012.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

