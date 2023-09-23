Schedule weekly 1-on-1 meetings
================
September 22, 2023

This script tries to schedule weekly 1-1 meeting based on a presumed
data format (a sign-up sheet in a special format). We will read this
data from a Google spreadsheet. Presumably, we want different students
to sign up on this spreadsheet. We want to output a list of 1-on-1
meeting slots.

Our example is at
<https://docs.google.com/spreadsheets/d/1KYSRe7Wjk7eMQ8e5zqr4U2Y1iC-FKI00Hk8UydrRc6g/edit?usp=sharing>
(what’s the md syntax of link?). Each student, for special reasons, is
called a duck 🦆. The big duck is the advisor. Technically we can
exclude the big duck column; but to be able to reuse this code in the
future, we put the advisor in a separate column.

``` r
library(googlesheets4, quietly = TRUE)
library(magrittr, quietly = TRUE) # %T>% 
library(tidyverse, quietly = TRUE)
library(ascii, quietly = TRUE) # print table in plain text
gs4_deauth()
```

# 1 Read data

First, we read the signup sheet. To make our life easier, we just read
the specified range.

``` r
signup_raw = range_read("https://docs.google.com/spreadsheets/d/1KYSRe7Wjk7eMQ8e5zqr4U2Y1iC-FKI00Hk8UydrRc6g/edit?usp=sharing",
           range = 'A1:M49',
           sheet = 'test'
           ) 
```

    ## ✔ Reading from "constraint programming practice".

    ## ✔ Range ''test'!A1:M49'.

Sanity check

``` r
head(signup_raw)
```

| day | time          | bigduck | duck1 | duck2 | duck3 | duck4 | duck5 | duck6 | duck7 | duck8 | duck9 | duck10 |
|:----|:--------------|:--------|:------|:------|:------|:------|:------|:------|:------|:------|:------|:-------|
| Mon | 10:00 - 10:30 | No      | Yes   | Yes   | Yes   | Yes   | No    | Yes   | Yes   | Maybe | Yes   | Yes    |
| Mon | 10:30 - 11:00 | Maybe   | Yes   | Yes   | Yes   | Yes   | No    | No    | Yes   | Maybe | Yes   | Yes    |
| Mon | 11:00 - 11:30 | Maybe   | Yes   | Yes   | No    | Yes   | Yes   | No    | Yes   | Yes   | No    | Yes    |
| Mon | 12:00 - 12:30 | Yes     | No    | Yes   | Yes   | Yes   | Yes   | No    | Yes   | Yes   | No    | Yes    |
| Mon | 12:30 - 13:00 | Yes     | No    | Yes   | Yes   | Yes   | Yes   | Yes   | Yes   | Yes   | Yes   | Maybe  |
| Mon | 13:30 - 14:00 | Yes     | Yes   | No    | Yes   | Yes   | Yes   | Yes   | No    | No    | Yes   | Maybe  |

# 2 Format data

Let’s first transform data into a format that’s easier to work with.

``` r
signup <- 
signup_raw %>% 
 mutate(across(everything(), ~replace(., . ==  'Yes' , 2))) %>% 
 mutate(across(everything(), ~replace(., . ==  'No' , 0))) %>% 
 mutate(across(everything(), ~replace(., . ==  'Maybe' , 1))) %>% 
 # transform each column into a number 
 # this assumes the 3 to last column is signups 
 mutate(across(3:length(.), as.numeric)) %>% 
 mutate(
   # some formatting tricks
   daytrick = as.numeric(as.factor(day)) * 100, 
   timeslot = daytrick + as.numeric(as.factor(time))) %>% 
 relocate(timeslot, .after = 'time') %>% 
  # rm columns which we don't need
 select(-daytrick)
```

``` r
signup
```

| day | time          | timeslot | bigduck | duck1 | duck2 | duck3 | duck4 | duck5 | duck6 | duck7 | duck8 | duck9 | duck10 |
|:----|:--------------|---------:|--------:|------:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|
| Mon | 10:00 - 10:30 |      101 |       0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     1 |     2 |      2 |
| Mon | 10:30 - 11:00 |      102 |       1 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |     1 |     2 |      2 |
| Mon | 11:00 - 11:30 |      103 |       1 |     2 |     2 |     0 |     2 |     2 |     0 |     2 |     2 |     0 |      2 |
| Mon | 12:00 - 12:30 |      104 |       2 |     0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     0 |      2 |
| Mon | 12:30 - 13:00 |      105 |       2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Mon | 13:30 - 14:00 |      106 |       2 |     2 |     0 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |      1 |
| Mon | 14:00 - 15:30 |      107 |       2 |     2 |     0 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |      2 |
| Mon | 15:30 - 16:00 |      108 |       2 |     2 |     0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |      2 |
| Mon | 16:00 - 16:30 |      109 |       1 |     2 |     2 |     0 |     2 |     0 |     2 |     2 |     2 |     0 |      2 |
| Mon | 16:30 - 17:00 |      110 |       1 |     0 |     2 |     0 |     0 |     2 |     2 |     2 |     2 |     0 |      2 |
| Mon | 17:00 - 17:30 |      111 |       1 |     1 |     2 |     2 |     0 |     2 |     2 |     0 |     0 |     2 |      2 |
| Mon | 17:30 - 18:00 |      112 |       1 |     2 |     2 |     2 |     0 |     2 |     2 |     2 |     0 |     2 |      1 |
| Tue | 10:00 - 10:30 |      301 |       2 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     2 |     2 |      1 |
| Tue | 10:30 - 11:00 |      302 |       2 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     2 |     2 |      2 |
| Tue | 11:00 - 11:30 |      303 |       2 |     2 |     2 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |      2 |
| Tue | 12:00 - 12:30 |      304 |       2 |     0 |     2 |     2 |     0 |     2 |     2 |     0 |     0 |     1 |      2 |
| Tue | 12:30 - 13:00 |      305 |       2 |     0 |     0 |     2 |     0 |     2 |     2 |     2 |     2 |     1 |      2 |
| Tue | 13:30 - 14:00 |      306 |       2 |     2 |     2 |     2 |     2 |     2 |     1 |     0 |     2 |     2 |      0 |
| Tue | 14:00 - 15:30 |      307 |       0 |     2 |     2 |     2 |     2 |     2 |     1 |     2 |     2 |     0 |      0 |
| Tue | 15:30 - 16:00 |      308 |       0 |     2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     0 |      2 |
| Tue | 16:00 - 16:30 |      309 |       0 |     2 |     2 |     0 |     2 |     2 |     2 |     2 |     2 |     0 |      2 |
| Tue | 16:30 - 17:00 |      310 |       0 |     0 |     2 |     0 |     2 |     0 |     2 |     2 |     2 |     2 |      2 |
| Tue | 17:00 - 17:30 |      311 |       0 |     1 |     2 |     0 |     2 |     2 |     2 |     2 |     0 |     2 |      2 |
| Tue | 17:30 - 18:00 |      312 |       0 |     2 |     2 |     2 |     1 |     2 |     2 |     2 |     2 |     2 |      1 |
| Wed | 10:00 - 10:30 |      401 |       2 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     2 |     0 |      2 |
| Wed | 10:30 - 11:00 |      402 |       2 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |     0 |     2 |      2 |
| Wed | 11:00 - 11:30 |      403 |       2 |     2 |     2 |     2 |     2 |     2 |     0 |     2 |     1 |     2 |      2 |
| Wed | 12:00 - 12:30 |      404 |       2 |     0 |     2 |     2 |     2 |     2 |     0 |     0 |     1 |     2 |      2 |
| Wed | 12:30 - 13:00 |      405 |       2 |     0 |     2 |     2 |     2 |     2 |     2 |     0 |     1 |     2 |      2 |
| Wed | 13:30 - 14:00 |      406 |       0 |     0 |     0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |      0 |
| Wed | 14:00 - 15:30 |      407 |       0 |     2 |     0 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Wed | 15:30 - 16:00 |      408 |       0 |     2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Wed | 16:00 - 16:30 |      409 |       2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     0 |      2 |
| Wed | 16:30 - 17:00 |      410 |       2 |     2 |     0 |     0 |     2 |     0 |     2 |     0 |     2 |     0 |      2 |
| Wed | 17:00 - 17:30 |      411 |       2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     0 |      2 |
| Wed | 17:30 - 18:00 |      412 |       2 |     2 |     2 |     2 |     1 |     2 |     2 |     2 |     2 |     2 |      0 |
| Thu | 10:00 - 10:30 |      201 |       2 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     2 |     0 |      2 |
| Thu | 10:30 - 11:00 |      202 |       2 |     2 |     2 |     2 |     2 |     0 |     0 |     0 |     0 |     0 |      2 |
| Thu | 11:00 - 11:30 |      203 |       2 |     2 |     2 |     2 |     2 |     2 |     0 |     0 |     0 |     2 |      0 |
| Thu | 12:00 - 12:30 |      204 |       2 |     0 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |     2 |      0 |
| Thu | 12:30 - 13:00 |      205 |       2 |     0 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |      2 |
| Thu | 13:30 - 14:00 |      206 |       2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     1 |      2 |
| Thu | 14:00 - 15:30 |      207 |       0 |     2 |     0 |     2 |     2 |     0 |     2 |     2 |     1 |     1 |      2 |
| Thu | 15:30 - 16:00 |      208 |       0 |     2 |     2 |     0 |     2 |     0 |     2 |     2 |     1 |     2 |      1 |
| Thu | 16:00 - 16:30 |      209 |       0 |     2 |     1 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Thu | 16:30 - 17:00 |      210 |       0 |     0 |     2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Thu | 17:00 - 17:30 |      211 |       2 |     1 |     2 |     2 |     2 |     2 |     2 |     2 |     0 |     2 |      2 |
| Thu | 17:30 - 18:00 |      212 |       2 |     2 |     2 |     2 |     1 |     2 |     2 |     0 |     0 |     2 |      2 |

# 3 Compute availabilities

we compute each duck’s availability shared with the big duck.

``` r
duck_avails <- 
signup %>% 
  select(-time) %>% 
  # if the duck shares the slot with the big duck
  # (maybe) we can constrain from the big duck?
  mutate(across(4:length(.), ~ (. > 0 & bigduck > 0))) %>% 
  #print() %>%
  # and if the duck is available after this half an hour slot
  mutate(across(4:length(.), ~ (. & lead(.)))) %>% 
  #print() %>%
  mutate_at(4:length(.), ~replace_na(.,FALSE))

head(duck_avails)
```

| day | timeslot | bigduck | duck1 | duck2 | duck3 | duck4 | duck5 | duck6 | duck7 | duck8 | duck9 | duck10 |
|:----|---------:|--------:|:------|:------|:------|:------|:------|:------|:------|:------|:------|:-------|
| Mon |      101 |       0 | FALSE | FALSE | FALSE | FALSE | FALSE | FALSE | FALSE | FALSE | FALSE | FALSE  |
| Mon |      102 |       1 | TRUE  | TRUE  | FALSE | TRUE  | FALSE | FALSE | TRUE  | TRUE  | FALSE | TRUE   |
| Mon |      103 |       1 | FALSE | TRUE  | FALSE | TRUE  | TRUE  | FALSE | TRUE  | TRUE  | FALSE | TRUE   |
| Mon |      104 |       2 | FALSE | TRUE  | TRUE  | TRUE  | TRUE  | FALSE | TRUE  | TRUE  | FALSE | TRUE   |
| Mon |      105 |       2 | FALSE | FALSE | TRUE  | TRUE  | TRUE  | TRUE  | FALSE | FALSE | TRUE  | TRUE   |
| Mon |      106 |       2 | TRUE  | FALSE | TRUE  | TRUE  | TRUE  | TRUE  | FALSE | FALSE | TRUE  | TRUE   |

# 4 Compute heuristics

We then compute heuristics based on the number of possible arrangements

``` r
duck_avail_heuristics <- duck_avails %>% 
  select(4:length(.)) %>% 
  # separate days
  split(., as.factor(signup$day)) %>% 
  # the possible arranges on each day
  # remove the first one because 
  lapply(\(x) sapply(x, sum))  %T>% 
  # print each duck's possible arranges on each day
  print() %>%
  # bind them together
  abind::abind(along = 0) %>% 
  as_tibble() %>% 
  # sort
  sapply(\(x) sum(x)) %>% sort()
```

    ## $Mon
    ##  duck1  duck2  duck3  duck4  duck5  duck6  duck7  duck8  duck9 duck10 
    ##      6      7      6      7      7      8      5      5      5     11 
    ## 
    ## $Thu
    ##  duck1  duck2  duck3  duck4  duck5  duck6  duck7  duck8  duck9 duck10 
    ##      3      4      6      6      4      2      1      2      4      3 
    ## 
    ## $Tue
    ##  duck1  duck2  duck3  duck4  duck5  duck6  duck7  duck8  duck9 duck10 
    ##      2      3      5      2      3      5      1      2      5      4 
    ## 
    ## $Wed
    ##  duck1  duck2  duck3  duck4  duck5  duck6  duck7  duck8  duck9 duck10 
    ##      5      6      6      8      3      4      4      6      3      6

how many possible arranges for each duck?

If any duck is zero… you may want to talk to the student…

``` r
duck_avail_heuristics
```

    ##  duck7  duck8  duck1  duck5  duck9  duck6  duck2  duck3  duck4 duck10 
    ##     11     15     16     17     17     19     20     23     23     24

``` r
# we remove 0 to avoid other problems
indices<-which(duck_avail_heuristics>0)
duck_avail_heuristics_confirmed <- duck_avail_heuristics[indices]
```

transform into a table form

``` r
duck_avail_heuristics_table = tibble(
  duck = names(duck_avail_heuristics_confirmed),
  avails = duck_avail_heuristics_confirmed
)

duck_avail_heuristics_table
```

| duck   | avails |
|:-------|-------:|
| duck7  |     11 |
| duck8  |     15 |
| duck1  |     16 |
| duck5  |     17 |
| duck9  |     17 |
| duck6  |     19 |
| duck2  |     20 |
| duck3  |     23 |
| duck4  |     23 |
| duck10 |     24 |

another heuristics might be how unique an available slot is?

``` r
slot_uniqueness_heuristics <- 
 duck_avails %>% 
  select(-day, -bigduck) %>% 
  reframe(timeslot = timeslot, slot_uniqueness = rowSums(across(2:length(.)))) %>% 
  filter(slot_uniqueness > 0) %>% 
  arrange(slot_uniqueness)

slot_uniqueness_heuristics
```

| timeslot | slot_uniqueness |
|---------:|----------------:|
|      110 |               4 |
|      409 |               4 |
|      202 |               4 |
|      108 |               5 |
|      109 |               5 |
|      304 |               5 |
|      305 |               5 |
|      410 |               5 |
|      201 |               5 |
|      203 |               5 |
|      204 |               5 |
|      102 |               6 |
|      103 |               6 |
|      105 |               6 |
|      303 |               6 |
|      401 |               6 |
|      104 |               7 |
|      106 |               7 |
|      107 |               7 |
|      111 |               7 |
|      112 |               7 |
|      302 |               7 |
|      402 |               7 |
|      403 |               7 |
|      404 |               7 |
|      412 |               7 |
|      411 |               8 |
|      205 |               8 |
|      211 |               8 |
|      301 |               9 |

# 5 Make the 1-on-1 meetings

``` r
get_a_schedule = function(max_iter = 100, 
                          duck_ordered_table, 
                          slot_order_table,
                          use_duck_order = TRUE){
  meetings <- NULL
  scheduled_ducks <- c()
  iter <- 1
  # if we arranged a meeting for each duck?
  
  while(iter < max_iter && length(scheduled_ducks)!=nrow(duck_ordered_table)){
    
    scheduled_slots <- meetings$timeslot
    
    if(is.null(scheduled_slots)) scheduled_slots = c()
    
    # the first available slot
    current_slot = slot_order_table %>% 
      filter(!(timeslot %in% scheduled_slots))
    # print(current_slot)
    # print(current_slot)
    
    # pull out who is available
    available_ducks = duck_avails %>% 
                      # remove scheduled ducks
                      select(-scheduled_ducks) %>% 
                      # who is available
                      filter(timeslot == current_slot$timeslot[1]) %>% 
                      select_if(~ any(. == TRUE)) 
    
   # print(available_ducks)
    
    if(use_duck_order==TRUE){
    # try to schedule more difficult ducks
      next_duck <- duck_ordered_table %>% 
      filter(!(duck %in% scheduled_ducks)) %>% 
      filter(duck %in% colnames(available_ducks)) %>% 
      slice(1) %>% pull(duck)
    }else{
      next_duck <- colnames(available_ducks)[1]
    }
   
    # if this slot is available for next duck
    if(!identical(next_duck, character(0))){
      
      scheduled_ducks <- c(scheduled_ducks, next_duck)
      
      meetings <- meetings %>% 
      rbind(tibble(duck = c(next_duck,next_duck),  
                   timeslot = c(current_slot$timeslot[1], current_slot$timeslot[1] + 1)))
     # print(meetings)
    }
    iter = iter + 1
  }
  if(length(scheduled_ducks)==nrow(duck_ordered_table))
     return(meetings)
  else{
    warning('return a partial schedule.  perhaps try to shuffle around ducks or timeslots')
    return(meetings)
  }
}
```

Deterministic schedule based on heuristics

``` r
a_schedule = get_a_schedule(duck_ordered_table = duck_avail_heuristics_table,
                            slot_order_table = slot_uniqueness_heuristics)
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(scheduled_ducks)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(scheduled_ducks))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
a_schedule %>% 
  left_join(signup %>% select(timeslot, day, time), by = 'timeslot') %>% 
  arrange(timeslot, duck)
```

| duck   | timeslot | day | time          |
|:-------|---------:|:----|:--------------|
| duck7  |      102 | Mon | 10:30 - 11:00 |
| duck7  |      103 | Mon | 11:00 - 11:30 |
| duck4  |      105 | Mon | 12:30 - 13:00 |
| duck4  |      106 | Mon | 13:30 - 14:00 |
| duck6  |      108 | Mon | 15:30 - 16:00 |
| duck6  |      109 | Mon | 16:00 - 16:30 |
| duck5  |      110 | Mon | 16:30 - 17:00 |
| duck5  |      111 | Mon | 17:00 - 17:30 |
| duck2  |      201 | Thu | 10:00 - 10:30 |
| duck1  |      202 | Thu | 10:30 - 11:00 |
| duck2  |      202 | Thu | 10:30 - 11:00 |
| duck1  |      203 | Thu | 11:00 - 11:30 |
| duck3  |      204 | Thu | 12:00 - 12:30 |
| duck3  |      205 | Thu | 12:30 - 13:00 |
| duck10 |      303 | Tue | 11:00 - 11:30 |
| duck10 |      304 | Tue | 12:00 - 12:30 |
| duck9  |      304 | Tue | 12:00 - 12:30 |
| duck9  |      305 | Tue | 12:30 - 13:00 |
| duck8  |      409 | Wed | 16:00 - 16:30 |
| duck8  |      410 | Wed | 16:30 - 17:00 |

``` r
b_schedule = get_a_schedule(duck_ordered_table = duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)),],
                            slot_order_table = slot_uniqueness_heuristics)
```

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), : return
    ## a partial schedule. perhaps try to shuffle around ducks or timeslots

``` r
b_schedule %>% 
  left_join(signup %>% select(timeslot, day, time), by = 'timeslot') %>% 
  arrange(timeslot, duck)
```

| duck   | timeslot | day | time          |
|:-------|---------:|:----|:--------------|
| duck7  |      102 | Mon | 10:30 - 11:00 |
| duck7  |      103 | Mon | 11:00 - 11:30 |
| duck4  |      105 | Mon | 12:30 - 13:00 |
| duck4  |      106 | Mon | 13:30 - 14:00 |
| duck10 |      108 | Mon | 15:30 - 16:00 |
| duck10 |      109 | Mon | 16:00 - 16:30 |
| duck6  |      110 | Mon | 16:30 - 17:00 |
| duck6  |      111 | Mon | 17:00 - 17:30 |
| duck3  |      201 | Thu | 10:00 - 10:30 |
| duck2  |      202 | Thu | 10:30 - 11:00 |
| duck3  |      202 | Thu | 10:30 - 11:00 |
| duck2  |      203 | Thu | 11:00 - 11:30 |
| duck5  |      204 | Thu | 12:00 - 12:30 |
| duck5  |      205 | Thu | 12:30 - 13:00 |
| duck9  |      304 | Tue | 12:00 - 12:30 |
| duck9  |      305 | Tue | 12:30 - 13:00 |
| duck8  |      409 | Wed | 16:00 - 16:30 |
| duck8  |      410 | Wed | 16:30 - 17:00 |

# 6 evaluate a schedule

We check how many ‘Maybe’ we have to do.

``` r
eval_schedule = function(signup, schedule){
  
  signup_longer <- signup %>% 
                    pivot_longer(cols = 5:length(.), names_to = 'duck', values_to = 'goodness')
  
  result_goodness <- 
    schedule %>% 
    left_join(signup_longer, by = c('duck', 'timeslot')) %>% 
    arrange(timeslot, duck) 
  
  advisor_col = colnames(result_goodness)[5]
  
  value = mean(result_goodness[[advisor_col]]) + mean(result_goodness$goodness)
  
  cat('1 = Maybe, the advisor\'s maybe:')
  print(ascii(result_goodness  %>% filter(!!sym(advisor_col) != 2)), type = "rest")
   
  cat('1 = Maybe, students\' maybe:')
  print(ascii(result_goodness  %>% filter(goodness != 2)), type = "rest")
  
  cat(paste0('overall utility is ', value, ' (higher is better) '))
}
```

``` r
eval_schedule(signup, a_schedule)
```

    ## 1 = Maybe, the advisor's maybe:
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## |   | duck  | timeslot | day | time          | bigduck | goodness |
    ## +===+=======+==========+=====+===============+=========+==========+
    ## | 1 | duck7 | 102.00   | Mon | 10:30 - 11:00 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 2 | duck7 | 103.00   | Mon | 11:00 - 11:30 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 3 | duck6 | 109.00   | Mon | 16:00 - 16:30 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 4 | duck5 | 110.00   | Mon | 16:30 - 17:00 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 5 | duck5 | 111.00   | Mon | 17:00 - 17:30 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## 1 = Maybe, students' maybe:
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## |   | duck  | timeslot | day | time          | bigduck | goodness |
    ## +===+=======+==========+=====+===============+=========+==========+
    ## | 1 | duck9 | 304.00   | Tue | 12:00 - 12:30 | 2.00    | 1.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 2 | duck9 | 305.00   | Tue | 12:30 - 13:00 | 2.00    | 1.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## overall utility is 3.65 (higher is better)

``` r
eval_schedule(signup, b_schedule)
```

    ## 1 = Maybe, the advisor's maybe:
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## |   | duck   | timeslot | day | time          | bigduck | goodness |
    ## +===+========+==========+=====+===============+=========+==========+
    ## | 1 | duck7  | 102.00   | Mon | 10:30 - 11:00 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 2 | duck7  | 103.00   | Mon | 11:00 - 11:30 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 3 | duck10 | 109.00   | Mon | 16:00 - 16:30 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 4 | duck6  | 110.00   | Mon | 16:30 - 17:00 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 5 | duck6  | 111.00   | Mon | 17:00 - 17:30 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## 1 = Maybe, students' maybe:
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## |   | duck  | timeslot | day | time          | bigduck | goodness |
    ## +===+=======+==========+=====+===============+=========+==========+
    ## | 1 | duck9 | 304.00   | Tue | 12:00 - 12:30 | 2.00    | 1.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 2 | duck9 | 305.00   | Tue | 12:30 - 13:00 | 2.00    | 1.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## overall utility is 3.61111111111111 (higher is better)
