---
title: "Data cleaning Study 4"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)     # create plots with ggplot, manipulate data, etc.
```

```
## Warning: package 'stringr' was built under R version 4.2.3
```

```r
library(broom.mixed)   # convert regression models into nice tables
library(modelsummary)  # combine multiple regression models into a single table
library(lme4)          # model specification / estimation 
library(lmerTest)      # provides p-values in the output
library(ggpubr)        # stile feature of ggplot
library(gghalves)      # do special plots in ggplot
library(kableExtra)    # for tables
```

## Import data

```r
d <- read_csv("data/qualtrics.csv")
names(d)
```

```
##   [1] "StartDate"               "EndDate"                
##   [3] "Status"                  "IPAddress"              
##   [5] "Progress"                "Duration (in seconds)"  
##   [7] "Finished"                "RecordedDate"           
##   [9] "ResponseId"              "RecipientLastName"      
##  [11] "RecipientFirstName"      "RecipientEmail"         
##  [13] "ExternalReference"       "LocationLatitude"       
##  [15] "LocationLongitude"       "DistributionChannel"    
##  [17] "UserLanguage"            "consent"                
##  [19] "attention_check"         "drug_know"              
##  [21] "drug_accept"             "planet_know"            
##  [23] "planet_accept"           "vertebrate_know"        
##  [25] "vertebrate_accept"       "brain_know"             
##  [27] "brain_accept"            "boson_know"             
##  [29] "boson_accept"            "orbit_know"             
##  [31] "orbit_accept"            "material_know"          
##  [33] "material_accept"         "speed_know"             
##  [35] "speed_accept"            "cloning_know"           
##  [37] "cloning_accept"          "magnetic_know"          
##  [39] "magnetic_accept"         "drug"                   
##  [41] "drug_confirm"            "planet"                 
##  [43] "planet_confirm"          "vertebrate"             
##  [45] "vertebrate_confirm"      "brain"                  
##  [47] "brain_confirm"           "boson"                  
##  [49] "boson_confirm"           "orbit"                  
##  [51] "orbit_confirm"           "material"               
##  [53] "material_confirm"        "speed"                  
##  [55] "speed_confirm"           "cloning"                
##  [57] "cloning_confirm"         "magnetic"               
##  [59] "magnetic_confirm"        "Q1...60"                
##  [61] "CMQ_1"                   "CMQ_2"                  
##  [63] "CMQ_3"                   "CMQ_4"                  
##  [65] "CMQ_5"                   "SICBS"                  
##  [67] "BCTI_intro"              "BCTI_apollo"            
##  [69] "BCTI_cancer"             "BCTI_viruses"           
##  [71] "BCTI_climatechange"      "BCTI_flatearth"         
##  [73] "BCTI_autism"             "BCTI_polio"             
##  [75] "BCTI_alien"              "BCTI_hydorxychlor"      
##  [77] "BCTI_evolution"          "wgm_scientists"         
##  [79] "wgm_sciencegeneral"      "pew"                    
##  [81] "reason_agreement"        "reason_agreement_3_TEXT"
##  [83] "reason_followup"         "Q1...84"                
##  [85] "education"               "comments"               
##  [87] "PROLIFIC_PID"            "drug_link1"             
##  [89] "drug_link2"              "planet_link1"           
##  [91] "planet_link2"            "vertebrate_link1"       
##  [93] "vertebrate_link2"        "brain_link1"            
##  [95] "brain_link2"             "boson_link1"            
##  [97] "boson_link2"             "orbit_link1"            
##  [99] "orbit_link2"             "material_link1"         
## [101] "material_link2"          "speed_link1"            
## [103] "speed_link2"             "cloning_link1"          
## [105] "cloning_link2"           "magnetic_link1"         
## [107] "magnetic_link2"
```


```r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 107
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-08-13… "2024-… "IP A… "97.184.… "100"    "304"                  "True"  
## 4 "2024-08-13… "2024-… "IP A… "172.59.… "100"    "309"                  "True"  
## 5 "2024-08-13… "2024-… "IP A… "207.172… "100"    "213"                  "True"  
## 6 "2024-08-13… "2024-… "IP A… "206.85.… "100"    "405"                  "True"  
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 100 more variables: RecordedDate <chr>, ResponseId <chr>,
## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
## #   DistributionChannel <chr>, UserLanguage <chr>, consent <chr>,
## #   attention_check <chr>, drug_know <chr>, drug_accept <chr>,
## #   planet_know <chr>, planet_accept <chr>, vertebrate_know <chr>, …
```

```r
# delete first two rows
d <- d %>% 
  slice(3: nrow(.)) 
```

## Attention check

```r
# attention check
table(d$attention_check)
```

```
## 
##       2       3 Never 1 
##       1       1     198
```

There are 2 failed attention checks. 


```r
# filter to only valid attention check responses
d <- d %>% filter(attention_check == "Never 1")
```

## Add demographics

## Recode demographics


```r
prolific_demographics <- read_csv("data/prolific_demographics.csv")

d <- left_join(d, prolific_demographics, by = c("PROLIFIC_PID" = "Participant id"))
```



```r
d <- d %>% 
  mutate(gender = case_when(Sex == "Male" ~ "male", 
                            Sex == "Female" ~  "female", 
                            .default = NA)
         ) %>% 
  rename(age = Age)
```

## Clean and re-shape data


```r
# check all names and their order
names(d)
```

```
##   [1] "StartDate"                     "EndDate"                      
##   [3] "Status.x"                      "IPAddress"                    
##   [5] "Progress"                      "Duration (in seconds)"        
##   [7] "Finished"                      "RecordedDate"                 
##   [9] "ResponseId"                    "RecipientLastName"            
##  [11] "RecipientFirstName"            "RecipientEmail"               
##  [13] "ExternalReference"             "LocationLatitude"             
##  [15] "LocationLongitude"             "DistributionChannel"          
##  [17] "UserLanguage"                  "consent"                      
##  [19] "attention_check"               "drug_know"                    
##  [21] "drug_accept"                   "planet_know"                  
##  [23] "planet_accept"                 "vertebrate_know"              
##  [25] "vertebrate_accept"             "brain_know"                   
##  [27] "brain_accept"                  "boson_know"                   
##  [29] "boson_accept"                  "orbit_know"                   
##  [31] "orbit_accept"                  "material_know"                
##  [33] "material_accept"               "speed_know"                   
##  [35] "speed_accept"                  "cloning_know"                 
##  [37] "cloning_accept"                "magnetic_know"                
##  [39] "magnetic_accept"               "drug"                         
##  [41] "drug_confirm"                  "planet"                       
##  [43] "planet_confirm"                "vertebrate"                   
##  [45] "vertebrate_confirm"            "brain"                        
##  [47] "brain_confirm"                 "boson"                        
##  [49] "boson_confirm"                 "orbit"                        
##  [51] "orbit_confirm"                 "material"                     
##  [53] "material_confirm"              "speed"                        
##  [55] "speed_confirm"                 "cloning"                      
##  [57] "cloning_confirm"               "magnetic"                     
##  [59] "magnetic_confirm"              "Q1...60"                      
##  [61] "CMQ_1"                         "CMQ_2"                        
##  [63] "CMQ_3"                         "CMQ_4"                        
##  [65] "CMQ_5"                         "SICBS"                        
##  [67] "BCTI_intro"                    "BCTI_apollo"                  
##  [69] "BCTI_cancer"                   "BCTI_viruses"                 
##  [71] "BCTI_climatechange"            "BCTI_flatearth"               
##  [73] "BCTI_autism"                   "BCTI_polio"                   
##  [75] "BCTI_alien"                    "BCTI_hydorxychlor"            
##  [77] "BCTI_evolution"                "wgm_scientists"               
##  [79] "wgm_sciencegeneral"            "pew"                          
##  [81] "reason_agreement"              "reason_agreement_3_TEXT"      
##  [83] "reason_followup"               "Q1...84"                      
##  [85] "education"                     "comments"                     
##  [87] "PROLIFIC_PID"                  "drug_link1"                   
##  [89] "drug_link2"                    "planet_link1"                 
##  [91] "planet_link2"                  "vertebrate_link1"             
##  [93] "vertebrate_link2"              "brain_link1"                  
##  [95] "brain_link2"                   "boson_link1"                  
##  [97] "boson_link2"                   "orbit_link1"                  
##  [99] "orbit_link2"                   "material_link1"               
## [101] "material_link2"                "speed_link1"                  
## [103] "speed_link2"                   "cloning_link1"                
## [105] "cloning_link2"                 "magnetic_link1"               
## [107] "magnetic_link2"                "Submission id"                
## [109] "Status.y"                      "Custom study tncs accepted at"
## [111] "Started at"                    "Completed at"                 
## [113] "Reviewed at"                   "Archived at"                  
## [115] "Time taken"                    "Completion code"              
## [117] "Total approvals"               "Covid-19 vaccine opinions"    
## [119] "Covid-19 vaccination"          "Vaccine opinions 2"           
## [121] "age"                           "Sex"                          
## [123] "Ethnicity simplified"          "Country of birth"             
## [125] "Country of residence"          "Nationality"                  
## [127] "Language"                      "Student status"               
## [129] "Employment status"             "gender"
```

Clean wide format data.


```r
# Function to extract numbers from a string for BCTI and SICBS
extract_numbers <- function(string_var) {
  as.numeric(gsub("\\D", "", string_var))
}

# test the function
# d %>% 
#   mutate(across(starts_with("BCTI"), ~extract_numbers(.), .names = "{.col}_num")) %>% 
#   select(BCTI_autism, BCTI_autism_num)

transform_text <- function(x) {
  ifelse(str_detect(x, "^I agree"), "Yes", "No")
}

# clean and re-shape
d_wide <- d %>% 
  # add an easy to read participant identifier
  mutate(id = 1:nrow(.)) %>%
  # make some character variables numeric
  mutate(across(c(starts_with("BCTI"), starts_with("SICBS"), starts_with("wgm"), 
                  starts_with("pew")), ~extract_numbers(.)
  )
  ) %>% 
  mutate(across(starts_with("CMQ"), as.numeric)
  ) %>% 
  # clean the text for correction 
  mutate_at(vars(ends_with("_confirm")), ~ transform_text(.))%>%
  # add average conspiracy measures 
  rowwise() %>% 
  mutate(BCTI_avg = mean(c_across(starts_with("BCTI")
                                  ),
                         na.rm = TRUE
                         ),
         CMQ_avg = mean(c_across(starts_with("CMQ")
                                  ),
                         na.rm = TRUE
                         )
         ) %>% 
  ungroup() 
```

Code the reasons for why people accept the scientific consensus:


```r
d_wide <- d_wide %>%
  mutate(reason_agreement = case_when(
    grepl("Other:", reason_agreement) ~ "other",
    grepl("independently", reason_agreement) ~ "independent verification",
    grepl("trust scientists", reason_agreement) ~ "trust in scientists",
    TRUE ~ NA_character_  # This handles any NAs
  )) %>% 
  rename(other_reason_agreement = reason_agreement_3_TEXT)
```

Make long format data.

We use the initial acceptance question and the confirmation question (only participants who did not accept the consensus for the intial question answered the confirmation question) to make a final acceptance question. This final acceptance will take on the initial answer in case of initial acceptance, and the answer of the confirmation question in case of initial non-acceptance. The final question will simply be named `acceptance`. 


```r
d_long <- d_wide %>% 
  # bring to long format
  pivot_longer(cols = c(
    ends_with("know"),
    ends_with("accept"),
    ends_with("confirm"),
    ends_with("link1"),
    ends_with("link2")
  ), 
  names_to = "subject_question", values_to = "answer") %>% 
  # separate subject (e.g. water) and question (e.g. acceptance)
  separate_wider_delim(subject_question, "_", names = c("subject", "question")
                       ) %>%
  pivot_wider(names_from = question, values_from = answer) %>% 
  # make a click count variable of the sum of both link clicks
  mutate(
    # make link variables numeric
    across(contains("link"), ~as.numeric(.x)),
    # make sum variable
    link_clicks = link1 + link2) %>% 
# create better variable names
  rename(knowledge = know, 
         acceptance_initial = accept, 
         acceptance_confirmation = confirm) %>% 
  # make a final acceptance variable
  mutate(acceptance = ifelse(is.na(acceptance_confirmation), acceptance_initial, acceptance_confirmation))
```

Add knowledge and acceptance by-participant averages to wide format data. Also add sum of link clicks. 


```r
# make a version with averages per participant (and sum of link clicks)
d_avg <- d_long %>% 
  # make numeric versions
  mutate(acceptance_num = ifelse(acceptance == "Yes", 1, 0), 
         knowledge_num = ifelse(knowledge == TRUE, 1, 0), 
         acceptance_initial_num = ifelse(acceptance_initial == "Yes", 1, 0)
  ) %>% 
  group_by(id) %>% 
  # calculate by-participant averages
  summarize(avg_acceptance  = sum(acceptance_num)/n(), 
            avg_knowledge = sum(knowledge_num)/n(),
            avg_acceptance_initial  = sum(acceptance_initial_num)/n(), 
            sum_link_clicks = sum(link_clicks)
  )

# make data frame with average participant data and wide format
d_wide <- left_join(d_wide, d_avg, 
               by = "id")
```

## Check NAs


```r
d_wide %>% 
  summarize(across(c("wgm_scientists", "wgm_sciencegeneral", "pew", "reason_agreement", "BCTI_avg", "CMQ_avg", "SICBS", "avg_acceptance", "avg_knowledge"), 
                   ~sum(is.na(.x))
                   )
            )
```

```
## # A tibble: 1 × 9
##   wgm_scientists wgm_sciencegeneral   pew reason_agreement BCTI_avg CMQ_avg
##            <int>              <int> <int>            <int>    <int>   <int>
## 1              0                  0     0                0        0       0
## # ℹ 3 more variables: SICBS <int>, avg_acceptance <int>, avg_knowledge <int>
```


```r
d_wide %>% 
  summarize(across(c("wgm_scientists", "wgm_sciencegeneral", "pew", "reason_agreement", "BCTI_avg", "CMQ_avg", "SICBS"), 
                   ~sum(!is.na(.x))
                   )
            ) %>% 
  pivot_longer(everything(), 
               names_to = "outcome", 
               values_to = "n_valid")
```

```
## # A tibble: 7 × 2
##   outcome            n_valid
##   <chr>                <int>
## 1 wgm_scientists         198
## 2 wgm_sciencegeneral     198
## 3 pew                    198
## 4 reason_agreement       198
## 5 BCTI_avg               198
## 6 CMQ_avg                198
## 7 SICBS                  198
```

## Clean justifications


```r
# Extract justifications
# questions <- unique(data_long$subject)
# 
# justifications <- d_wide %>%
#   select(id, all_of(questions)) %>%
#   pivot_longer(all_of(questions),
#                names_to = "question",
#                values_to = "answer") %>%
#   drop_na(answer) %>%
#   arrange(id)
# 
# write_csv(justifications, "data/justifications.csv")
```


```r
justifications <- read_csv("data/justifications_coded.csv")
```

```
## Rows: 255 Columns: 4
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): question, answer, final_coding
## dbl (1): id
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Research questions
justifications <- justifications %>%
  mutate(category = final_coding)

write_csv(justifications, "data/justifications_clean.csv")
```


## Export data


```r
# wide format
write_csv(d_wide, "data/cleaned_wide.csv")

# long format
write_csv(d_long, "data/cleaned_long.csv")
```

