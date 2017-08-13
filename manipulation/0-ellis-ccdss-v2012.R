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

path_input  <- "./data-unshared/raw/ccdss/ccdss-scsmc-eng.csv"
path_save <- "./data-unshared/derived/dto-ccdss-v2012.csv"
  
  age_quarter_period <- c(
     "1 to 4"  , 1 , 1      
    ,"5 to 9"  , 1 , 2   
    ,"10 to 14", 1 , 3     
    ,"15 to 19", 1 , 4     
    ,"20 to 24", 1,  5     
    ,"25 to 29", 2 , 1     
    ,"30 to 34", 2 , 2     
    ,"35 to 39", 2 , 3     
    ,"40 to 44", 2 , 4     
    ,"45 to 49", 2 , 5     
    ,"50 to 54", 3 , 1     
    ,"55 to 59", 3 , 2     
    ,"60 to 64", 3 , 3     
    ,"65 to 69", 3 , 4     
    ,"70 to 74", 3 , 5     
    ,"75 to 79", 4 , 1     
    ,"80 to 84", 4 , 2     
    ,"85+"     , 4 , 3
  )
ds_aqp <- matrix(age_quarter_period,18,3,byrow = T) %>% as.data.frame()
names(ds_aqp) <- c("age","quarter", "period")
# ---- utility-functions -----------------------

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for origin of the data
ds_wide <- readr::read_csv(path_input)
# ---- inspect-data ----------------------------
# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
# ds %>% glimpse()

# ---- tweak-data ------------------------------
ds_wide <- ds_wide %>% 
  dplyr::rename_(  
    "disease" = "Disease"                     
    ,"year"    = "`Fiscal Year`"
    ,"sex"     = "Gender"
    ,"age"     = "`Age Group`"
  )
# ds <- ds%>% 
# dplyr::filter(disease == "Mental_Illness") %>% 
# dplyr::filter(disease == "MoodAnxiety_Disorder") #%>%
# dplyr::filter(year == 2007)
# d <- ds
# ds <- d
ds_wide %>% glimpse()
ds_wide %>% distinct(disease) %>% kable()
static_variables <- c("disease","year","sex","age") # 828 vs 720
# static_variables <- c("disease","year","sex","age","Population") # 828 vs 720
# static_variables <- c("disease","year","sex","age","population","incident_cases","prevalent_cases")
dynamic_variables <- colnames(ds_wide) %>% setdiff( static_variables)
ds_long <- ds_wide %>% 
  tidyr::gather_("variable", "value", dynamic_variables) #%>% 
# d <- ds_long
ds_long %>% head()
ds_long %>% distinct(variable) %>% kable() 
# ds_long %>% distinct(variable) %>% readr::write_csv("./data-unshared/derived/rename_stencil.csv")
# d <- ds_long
rename_stencil_1 <- readr::read_csv("./data-public/raw/ccdss/rename_stencil_1.csv")
rename_stencil_2   <- readr::read_csv("./data-public/raw/ccdss/rename_stencil_2.csv")
rename_stencil_3   <- readr::read_csv("./data-public/raw/ccdss/rename_stencil_3.csv")
rename_stencil <- rename_stencil_3
# rename_stencil <- rename_stencil_2

rename_stencil %>% kable()
# structure for use
ds <- ds_long %>% 
  dplyr::left_join(rename_stencil, by = "variable") %>% 
  dplyr::left_join(ds_aqp, by = "age") %>% 
  dplyr::select_(.dots = c(
    "disease","year","sex","age","quarter","period",
    "condition_present",
    "variable","index","location","view", "unit_count","value"
  )) %>% 
  dplyr::mutate(
    value   = as.numeric(value),
    quarter = factor(quarter, levels = c(1:4),labels = c("0 - 25","26-50","51-75","76-85+")),
    period  = factor(period,  levels = c(1:5))
  )
ds %>% glimpse()

# final tweaks
proper_order_of_age_groups <- ds_aqp[,"age"] %>% as.character()
ds <- ds %>% 
  dplyr::mutate(
    age = factor(age, 
                 levels = proper_order_of_age_groups,
                 labels = proper_order_of_age_groups ),
    sex = factor(sex, levels = c("M","F"), labels=c("Men","Women"))
  )
ds %>% glimpse()
str(ds)

# ---- save-to-disk --------------------- 
# save the created and groomed dataset for further use by subsequent reports
readr::write_csv(ds, path_save)
ds %>% saveRDS( gsub(".csv$",".rds",path_save) )

# ---- basic-graph -----------------------------
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

