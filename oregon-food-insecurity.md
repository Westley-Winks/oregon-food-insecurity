Food Insecurity in Oregon 2020
================
Westley Winks
11/21/2021

# Step 1: Ask

-   What is the food insecurity rate in Oregon in 2020?
-   What types of households are most food insecure in Oregon?
-   Where are food insecure people in Oregon?
-   What is a general profile for a food insecure person in Oregon?

TODO: define food insecure, maybe define the business scenario (State of
Oregon figuring out where to put food resources)

# Step 2: Prepare

Where is data coming from?  
- CPS, Current Population Survey

TODO: further define where the data comes from, how it is collected,
what is in it, “Where is this data from and what does it cover?”,
limitations

Factors to be used will include:

### VARIABLES

-   HUFINAL = Final outcome of interview (completed or not)
-   HRFS12M1 = Food insecurity status, last 12 months
-   HRFS30D1 = Food insecurity status, last 30 days
-   GESTFIPS = Geographic FIPS code (Oregon is 41)
-   HRPOOR = Above or below 185% poverty level, determined from family
    income
-   HRNUMHOU = Household size
-   HRHTYPE = Household type
-   GTMETSTA = Metro status
-   PEMARITL = Marital status
-   PESEX = Sex (non-binary not represented)
-   PEAFEVER = Did you serve active duty? (1 is yes, 2 is no)
-   PEEDUCA = Highest level of school completed
-   PTDTRACE = Race
-   PRCITSHP = Citizenship status
-   PEMJOT = Do you have more than one job?
-   PEHRUSLT = How many hours per week do you work each week?
-   HRSUPINT = Whether or not they were interviewed for food security
    supplement survey
-   PRTAGE = Age

# Step 3: Process

In the full data set for 2020, there are 132,036 observations across 507
variables. There are major entities (ERS, USDA) that use all of this
data to get important stats and summaries of the nation. For simplicity,
I only want to pull out specific ones that will help answer the business
task described above. The selected variables are listed above. I am also
choosing to just look at responses from Oregon. I also want to put in
more meaningful variable names and definitions.

``` r
library(tidyverse)
library(janitor)
```

To clean the data, I first filtered the data. I `select`ed the variables
above. I also `filter`ed based on my critera of only looking at Oregon
and since I am interested in looking at food insecurity, I filtered out
people who didn’t complete the supplemental food security interview. I
also filtered out people who only partially completed the survey.

``` r
df <- read.csv("dec20pub.csv") %>% 
  select(HUFINAL, HRFS12M1, HRFS30D1, GESTFIPS, HRPOOR, HRNUMHOU, HRHTYPE, 
         GTMETSTA, PEMARITL, PESEX, PEAFEVER, PEEDUCA, PEMLR, PTDTRACE, 
         PRCITSHP, PEMJOT, PEHRUSLT, PRNMCHLD, HRSUPINT, PRTAGE) %>% 
  filter(GESTFIPS == 41) %>% # in Oregon
  filter(HRSUPINT == 1) %>% # interviewed for food security
  filter(HUFINAL %in% c(1, 201)) # fully completed survey
```

Second, I decoded all of the variables using a series of `case_when`’s.
The responses are recorded as numbers and what they represent is in a
helper document (data-definitions.pdf). I also grouped similar levels
together to reduce granularity. In the process, I renamed the variables
to more meaningful names.

``` r
df_2 <- df %>% 
  mutate(food_insec_12mo = case_when(
    HRFS12M1 == 1 ~ "Food Secure",
    HRFS12M1 %in% 2:3 ~ "Food Insecure",
    HRFS12M1 == -9 ~ "No Response"
  )) %>% 
  mutate(food_insec_30d = case_when(
    HRFS30D1 == 1 ~ "Food Secure",
    HRFS30D1 %in% 2:3 ~ "Food Insecure",
    HRFS30D1 == -9 ~ "No Response"
  )) %>% 
  mutate(pov_level = case_when(
    HRPOOR == 1 ~ "Below 185% Poverty",
    HRPOOR == 2 ~ "Above 185% Poverty or Income Not Reported"
  )) %>% 
  mutate(num_in_house = HRNUMHOU) %>% 
  mutate(house_type = case_when(
    HRHTYPE == 0 ~ "Non-Interview",
    HRHTYPE == 1 ~ "Husband Wife Primary Family",
    HRHTYPE == 2 ~ "Husband Wife Primary Family AF",
    HRHTYPE == 3 ~ "Single Father Family",
    HRHTYPE == 4 ~ "Single Mother Family",
    HRHTYPE == 5 ~ "Single Primary Family AF", 
    HRHTYPE == 6 ~ "Male Individual", 
    HRHTYPE == 7 ~ "Female Individual", 
    HRHTYPE == 8 ~ "Individual AF", 
    HRHTYPE == 9 ~ "Group Quarters with Family", 
    HRHTYPE == 10 ~ "Group Quarters without Family"
  )) %>% 
  mutate(met_status = case_when(
    GTMETSTA == 1 ~ "Metropolitan", 
    GTMETSTA == 2 ~ "Non-Metropolitan", 
    GTMETSTA == 3 ~ "Not Identified"
  )) %>% 
  mutate(marital_status = case_when(
    PEMARITL == 1 ~ "Married with Spouse Present",
    PEMARITL == 2 ~ "Married with Spouse Absent",
    PEMARITL == 3 ~ "Widowed", 
    PEMARITL == 4 ~ "Divorced", 
    PEMARITL == 5 ~ "Separated", 
    PEMARITL == 6 ~ "Never Married",
    TRUE ~ "Younger than 15"
  )) %>% 
  mutate(sex = case_when(
    PESEX == 1 ~ "Male",
    PESEX == 2 ~ "Female"
  )) %>% 
  mutate(active_duty = case_when(
    PEAFEVER == 1 ~ "Served on Active Duty",
    PEAFEVER == 2 ~ "Didn't Serve on Active Duty",
    TRUE ~ "Younger than 17"
  )) %>% 
  mutate(edu_level = case_when(
    PEEDUCA %in% c(31, 32, 33, 34, 35, 36, 37, 38) ~ "Less than high school", 
    PEEDUCA == 39 ~ "High School",
    PEEDUCA == 40 ~ "Some college", 
    PEEDUCA == 41 ~ "Associate's, vocational",
    PEEDUCA == 42 ~ "Associate's, academic", 
    PEEDUCA == 43 ~ "Bachelor's", 
    PEEDUCA == 44 ~ "Master's", 
    PEEDUCA == 45 ~ "Professional School",
    PEEDUCA == 46 ~ "Doctorate's", 
    TRUE ~ "Child"
  )) %>% 
  mutate(race = case_when(
    PTDTRACE == 1 ~ "White Only", 
    PTDTRACE == 2 ~ "Black Only", 
    PTDTRACE == 3 ~ "American Indian, Alaskan Native Only", 
    PTDTRACE == 4 ~ "Asian Only", 
    PTDTRACE == 5 ~ "Hawaiian/Pacific Islander Only", 
    PTDTRACE == 6 ~ "White-Black", 
    PTDTRACE == 7 ~ "White-AI", 
    PTDTRACE == 8 ~ "White-Asian", 
    PTDTRACE == 9 ~ "White-HP", 
    PTDTRACE == 10 ~ "Black-AI", 
    PTDTRACE == 11 ~ "Black-Asian", 
    PTDTRACE == 12 ~ "Black-HP", 
    PTDTRACE == 13 ~ "AI-Asian", 
    PTDTRACE == 14 ~ "AI-HP",
    PTDTRACE == 15 ~ "Asian-HP",
    TRUE ~ "3 or more races"
  )) %>% 
  mutate(citizenship_status = case_when(
    PRCITSHP %in% 1:3 ~ "Native",
    PRCITSHP == 4 ~ "Foreign Born Citizen", 
    PRCITSHP == 5 ~ "Foreign Born Non-Citizen"
  )) %>% 
  mutate(more_than_one_job = case_when(
    PEMJOT == 1 ~ "More than 1 job", 
    PEMJOT == 2 ~ "1 Job",
    TRUE ~ "Unemployed"
  )) %>% 
  mutate(hrs_worked_per_week = case_when(
    PEHRUSLT == -4 ~ "Varies", 
    PEHRUSLT == -1 ~ "Unemployed",
    TRUE ~ as.character(PEHRUSLT)
  )) %>% 
  mutate(num_children = case_when(
    PRNMCHLD == -1 ~ "Not a Parent", 
    PRNMCHLD == 0 ~ "Children Older than 18", 
    TRUE ~ as.character(PRNMCHLD)
  )) %>% 
  mutate(age = PRTAGE) %>% 
  mutate(emp_status = case_when(
    PEMLR %in% 1:2 ~ "Employed", 
    PEMLR %in% 3:4 ~ "Unemployed", 
    PEMLR %in% 5:7 ~ "Not in Labor Force", 
    TRUE ~ "Child or Military"
  ))

clean_df <- select(df_2, -starts_with(LETTERS, ignore.case = FALSE))

glimpse(clean_df)
```

    ## Rows: 1,574
    ## Columns: 17
    ## $ food_insec_12mo     <chr> "Food Secure", "Food Secure", "Food Secure", "Food…
    ## $ food_insec_30d      <chr> "Food Secure", "Food Secure", "Food Secure", "Food…
    ## $ pov_level           <chr> "Above 185% Poverty or Income Not Reported", "Abov…
    ## $ num_in_house        <int> 1, 2, 2, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 1, 3, 3, 3,…
    ## $ house_type          <chr> "Female Individual", "Single Mother Family", "Sing…
    ## $ met_status          <chr> "Metropolitan", "Metropolitan", "Metropolitan", "M…
    ## $ marital_status      <chr> "Never Married", "Separated", "Younger than 15", "…
    ## $ sex                 <chr> "Female", "Female", "Male", "Female", "Male", "Mal…
    ## $ active_duty         <chr> "Didn't Serve on Active Duty", "Didn't Serve on Ac…
    ## $ edu_level           <chr> "Master's", "Master's", "Child", "Associate's, voc…
    ## $ race                <chr> "White Only", "White Only", "3 or more races", "Wh…
    ## $ citizenship_status  <chr> "Native", "Native", "Native", "Native", "Native", …
    ## $ more_than_one_job   <chr> "1 Job", "More than 1 job", "Unemployed", "1 Job",…
    ## $ hrs_worked_per_week <chr> "50", "63", "Unemployed", "20", "Unemployed", "Une…
    ## $ num_children        <chr> "Children Older than 18", "1", "Children Older tha…
    ## $ age                 <int> 63, 44, 12, 75, 79, 50, 31, 76, 72, 46, 21, 62, 55…
    ## $ emp_status          <chr> "Employed", "Employed", "Child or Military", "Empl…

Cleaning summary: - Selected variables of interest - Filtered to keep
people in Oregon that fully completed the main survey and completed the
food security survey - Decoded the data using definitions described in
the data definitions document - Grouped the two levels of food
insecurity (very low and low) into one (Food Insecure) - Grouped
education levels that were less than completing high school into one
group - Grouped people with 3 or more races into one group - Grouped
citizenship status of native into one group regardless of where they
were born - Renamed variables

# Step 4: Analyze

## What was the food insecurity rate in Oregon in 2020?

First, let’s look at the food insecurity rate in Oregon last year. This
will be done by summarizing the data to determine how many (as a
percentage) of the respondents were food insecure at some point in 2020.

``` r
food_insec_rate_12mo <- clean_df %>% 
  count(food_insec_12mo, name = "count") %>% 
  mutate(rate = count/sum(count))
```

``` r
# ggplot(data = food_insec_rate_12mo, 
#        mapping = aes(x = "", 
#                      y = freq, 
#                      fill = food_insec_12mo, 
#                      label = round(freq, 2))) +
#   geom_bar(stat = "identity", 
#            width = 2, 
#            color = "white") + 
#   coord_polar("y", start = 0) +
#   theme_void() + 
#   geom_label(aes(label = 100*round(freq,4)), position = position_stack(vjust = 0.5)) +
#   guides(fill = guide_legend(title = "Food Security Status")) + 
#   labs(title = "Frequency of Food Security Status in Oregon 2020",
#        subtitle = "What percentage of people struggle to find their next meal?")
```

``` r
# clean_df %>%
#   count(food_insec_12mo, name = "count") %>%
#   mutate(rate = count/sum(count)) %>%
#     ggplot(mapping = aes(x = fct_reorder(food_insec_12m, rate), y = rate)) +
#     geom_col()
```

From this data, the food insecurity rate in Oregon 2020 was about **9%**
Looking at data from [Feeding
America](https://map.feedingamerica.org/county/2019/overall), the food
insecurity rate in Oregon was 11.5% in 2019. While the two values are
close, the discrepancy is likely due to Feeding America reporting
results from a model they developed based many years of CPS data.
COVID-19 also heavily impacted food insecurity in 2020 and this could be
the cause of an increase in food insecurity in 2020.

## What types of people are most food insecure?

### How much do food insecure people work?
