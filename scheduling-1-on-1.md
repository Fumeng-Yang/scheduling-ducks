Schedule weekly 1-on-1 meetings
================
September 24, 2023

We face scheduling problems when creating weekly 1-1 meetings with a set
of students. This is a problem of a large-ish group. Here, our needs are
to find an hour slot for each student, although the actual meeting time
might be 40-50 minutes. We assume both the advisor and students are
busy, and thus it is difficult to create a schedule (otherwise, we won’t
need such a script).

This script tries to schedule weekly 1-1 meetings between an advisor and
each student based on a presumed data format (a sign-up sheet in a
special format with ‘Yes’, ‘No’, and ‘Maybe’; it needs everyone to fill
in all slots). We will read this data from a Google spreadsheet.
Presumably, we want different students to sign up on this spreadsheet.
If we need to locate an hour, then signing up for half an hour slots
will make thing easier. We want to output a list of 1-on-1 meeting
slots.

Our example is at
<https://docs.google.com/spreadsheets/d/1KYSRe7Wjk7eMQ8e5zqr4U2Y1iC-FKI00Hk8UydrRc6g/edit?usp=sharing>.
Each student, for special reasons, is called a duck 🦆. The big duck is
the advisor. Technically we can exclude the big duck column; but to be
able to reuse this code and the spreadsheet in the future, we put the
advisor in a separate column.

``` r
library(googlesheets4, quietly = TRUE)
library(magrittr, quietly = TRUE) # %T>% 
library(tidyverse, quietly = TRUE)
library(ascii, quietly = TRUE) # print table in plain text
gs4_deauth()

TIME_INDEX = 3 # the index of first name
NEED_SLOT = 2
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
 # this assumes the TIME_INDEX to last column is signups 
 mutate(across(TIME_INDEX:length(.), as.numeric)) %>% 
 mutate(
   # some formatting tricks
   daytrick = as.numeric(as.factor(day)) * 100, 
   timeslot = daytrick + as.numeric(as.factor(time))) %>% 
 relocate(timeslot, .after = 'time') %>% 
  # rm columns which we don't need
 select(-daytrick)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(TIME_INDEX:length(.), as.numeric)`.
    ## Caused by warning:
    ## ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(TIME_INDEX)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(TIME_INDEX))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

``` r
head(signup)
```

| day | time          | timeslot | bigduck | duck1 | duck2 | duck3 | duck4 | duck5 | duck6 | duck7 | duck8 | duck9 | duck10 |
|:----|:--------------|---------:|--------:|------:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|
| Mon | 10:00 - 10:30 |      101 |       0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     1 |     2 |      2 |
| Mon | 10:30 - 11:00 |      102 |       1 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |     1 |     2 |      2 |
| Mon | 11:00 - 11:30 |      103 |       1 |     2 |     2 |     0 |     2 |     2 |     0 |     2 |     2 |     0 |      2 |
| Mon | 12:00 - 12:30 |      104 |       2 |     0 |     2 |     2 |     2 |     2 |     0 |     2 |     2 |     0 |      2 |
| Mon | 12:30 - 13:00 |      105 |       2 |     0 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |     2 |      1 |
| Mon | 13:30 - 14:00 |      106 |       2 |     2 |     0 |     2 |     2 |     2 |     2 |     0 |     0 |     2 |      1 |

# 3 Compute availabilities

we compute each duck’s availability shared with the big duck.

``` r
duck_avails <- 
signup %>% 
  select(-time) %>% 
  # if the duck shares the slot with the big duck
  # (maybe) we can constrain from the big duck?
  mutate(across((TIME_INDEX + 1):length(.), ~ (. > 0 & bigduck > 0))) %>% 
  # print() %>%
  # and if the duck is available after this half an hour slot
  # if we need only half an hour and signup for half hour slots, we can skip this line
  mutate(across((TIME_INDEX + 1):length(.), ~ (. & lead(.)))) %>% 
  #print() %>%
  mutate_at((TIME_INDEX + 1):length(.), ~replace_na(.,FALSE))

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
  select((TIME_INDEX + 1):length(.)) %>% 
  # separate days
  split(., as.factor(signup$day)) %>% 
  # the possible arranges on each day
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

how many possible arrangements for each duck?

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

The idea is very simple. We iterate over each available slot, check if a
student is available, schedule the duck’s meeting. Then we remove
scheduled timeslot and the student, and try next one.

``` r
get_a_schedule = function(max_iter = nrow(duck_ordered_table) * 10, 
                          duck_ordered_table, 
                          slot_ordered_table,
                          use_duck_order = TRUE){
  meetings <- NULL
  scheduled_ducks <- c()
  iter <- 1
  # if we arranged a meeting for each duck?
  
  while(iter < max_iter && length(scheduled_ducks)!=nrow(duck_ordered_table)){
    
    scheduled_slots <- meetings$timeslot
    
    if(is.null(scheduled_slots)) scheduled_slots = c()
    
    # the first available slot
    current_slot = slot_ordered_table %>% 
      filter(!(timeslot %in% scheduled_slots))
    # print(current_slot)
    # print(current_slot)
    
    # pull out who is available
    available_ducks = duck_avails %>% 
                      # remove scheduled ducks
                      select(-all_of(scheduled_ducks)) %>% 
                      # who is available
                      filter(timeslot == current_slot$timeslot[1]) %>% 
                      select_if(~ any(. == TRUE)) 
    
   # print(available_ducks)
    
    if(use_duck_order==TRUE){
    # try to schedule busier ducks
      next_duck <- duck_ordered_table %>% 
      filter(!(duck %in% scheduled_ducks)) %>% 
      filter(duck %in% colnames(available_ducks)) %>% 
      slice(1) %>% pull(duck)
    }else{
      next_duck <- colnames(available_ducks)[1]
    }
   
    # if this slot is available for next duck
    # schedule the meeting
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
    warning('returning a partial schedule.  perhaps try to shuffle around ducks or timeslots')
    return(meetings)
  }
}
```

Deterministic schedule based on heuristics

``` r
a_schedule = get_a_schedule(duck_ordered_table = duck_avail_heuristics_table,
                            slot_ordered_table = slot_uniqueness_heuristics)

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

So.. if this scheduling isn’t too hard. We can just randomize the order
and generate another schedule

``` r
b_schedule = get_a_schedule(duck_ordered_table = duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)),],
                            slot_ordered_table = slot_uniqueness_heuristics)
```

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

``` r
b_schedule %>% 
  left_join(signup %>% select(timeslot, day, time), by = 'timeslot') %>% 
  arrange(timeslot, duck)
```

| duck   | timeslot | day | time          |
|:-------|---------:|:----|:--------------|
| duck7  |      102 | Mon | 10:30 - 11:00 |
| duck7  |      103 | Mon | 11:00 - 11:30 |
| duck5  |      105 | Mon | 12:30 - 13:00 |
| duck5  |      106 | Mon | 13:30 - 14:00 |
| duck6  |      108 | Mon | 15:30 - 16:00 |
| duck6  |      109 | Mon | 16:00 - 16:30 |
| duck10 |      110 | Mon | 16:30 - 17:00 |
| duck10 |      111 | Mon | 17:00 - 17:30 |
| duck2  |      201 | Thu | 10:00 - 10:30 |
| duck2  |      202 | Thu | 10:30 - 11:00 |
| duck3  |      202 | Thu | 10:30 - 11:00 |
| duck3  |      203 | Thu | 11:00 - 11:30 |
| duck8  |      204 | Thu | 12:00 - 12:30 |
| duck8  |      205 | Thu | 12:30 - 13:00 |
| duck9  |      304 | Tue | 12:00 - 12:30 |
| duck9  |      305 | Tue | 12:30 - 13:00 |
| duck4  |      409 | Wed | 16:00 - 16:30 |
| duck4  |      410 | Wed | 16:30 - 17:00 |

# 6 Evaluate a schedule

We check how many ’Maybe’s we have to do.

``` r
eval_schedule = function(signup, schedule, message = TRUE){
  
  signup_longer <- signup %>% 
                   pivot_longer(cols = (TIME_INDEX + 2):length(.), names_to = 'duck', values_to = 'goodness')
  
  result_goodness <- 
    schedule %>% 
    left_join(signup_longer, by = c('duck', 'timeslot')) %>% 
    arrange(timeslot, duck) 
  
  advisor_col = colnames(result_goodness)[5]
  
  value = mean(result_goodness[[advisor_col]]) + mean(result_goodness$goodness)
  
  if(message){
      cat('1 = Maybe, the advisor\'s maybe:')
      if(nrow(result_goodness  %>% filter(!!sym(advisor_col) != 2)) > 0){
         print(ascii(result_goodness  %>% filter(!!sym(advisor_col) != 2)), type = "rest")
      }else{
         cat('none\n')
      }
      
      cat('1 = Maybe, students\' maybe:')
      if(nrow(result_goodness  %>% filter(goodness != 2)) > 0){
        print(ascii(result_goodness  %>% filter(goodness != 2)), type = "rest")
      }else{
        cat('none\n')
      }
      
      cat(paste0('overall utility is ', value, ' (higher is better) '))
  }
  
  return(value)
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

    ## [1] 3.65

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
    ## | 3 | duck6  | 109.00   | Mon | 16:00 - 16:30 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 4 | duck10 | 110.00   | Mon | 16:30 - 17:00 | 1.00    | 2.00     |
    ## +---+--------+----------+-----+---------------+---------+----------+
    ## | 5 | duck10 | 111.00   | Mon | 17:00 - 17:30 | 1.00    | 2.00     |
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

    ## [1] 3.611111

# 7 Generate a brunch and get the best

``` r
seed = seq(10, 100, length.out = 10)
current = 0
schedule = NULL

for(s in seed){
  set.seed(s)
  i_schedule = get_a_schedule(duck_ordered_table = duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)),],
                              slot_ordered_table = slot_uniqueness_heuristics)
  if(nrow(i_schedule) == 2 * nrow(duck_avail_heuristics_table)){
      #print(i_schedule)
      value = eval_schedule(signup, i_schedule, message = FALSE)
      if(value >= current){
         current = value
         schedule = i_schedule
      }
  }
}
```

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

    ## Warning in get_a_schedule(duck_ordered_table =
    ## duck_avail_heuristics_table[sample(nrow(duck_avail_heuristics_table)), :
    ## returning a partial schedule. perhaps try to shuffle around ducks or timeslots

``` r
eval_schedule(signup, schedule)
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
    ## | 4 | duck2 | 110.00   | Mon | 16:30 - 17:00 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## | 5 | duck2 | 111.00   | Mon | 17:00 - 17:30 | 1.00    | 2.00     |
    ## +---+-------+----------+-----+---------------+---------+----------+
    ## 1 = Maybe, students' maybe:none
    ## overall utility is 3.75 (higher is better)

    ## [1] 3.75

``` r
schedule %>% 
    left_join(signup %>% select(timeslot, day, time), by = 'timeslot') %>% 
    arrange(timeslot, duck)
```

| duck   | timeslot | day | time          |
|:-------|---------:|:----|:--------------|
| duck7  |      102 | Mon | 10:30 - 11:00 |
| duck7  |      103 | Mon | 11:00 - 11:30 |
| duck9  |      105 | Mon | 12:30 - 13:00 |
| duck9  |      106 | Mon | 13:30 - 14:00 |
| duck6  |      108 | Mon | 15:30 - 16:00 |
| duck6  |      109 | Mon | 16:00 - 16:30 |
| duck2  |      110 | Mon | 16:30 - 17:00 |
| duck2  |      111 | Mon | 17:00 - 17:30 |
| duck1  |      201 | Thu | 10:00 - 10:30 |
| duck1  |      202 | Thu | 10:30 - 11:00 |
| duck3  |      202 | Thu | 10:30 - 11:00 |
| duck3  |      203 | Thu | 11:00 - 11:30 |
| duck8  |      204 | Thu | 12:00 - 12:30 |
| duck8  |      205 | Thu | 12:30 - 13:00 |
| duck5  |      303 | Tue | 11:00 - 11:30 |
| duck10 |      304 | Tue | 12:00 - 12:30 |
| duck5  |      304 | Tue | 12:00 - 12:30 |
| duck10 |      305 | Tue | 12:30 - 13:00 |
| duck4  |      409 | Wed | 16:00 - 16:30 |
| duck4  |      410 | Wed | 16:30 - 17:00 |
