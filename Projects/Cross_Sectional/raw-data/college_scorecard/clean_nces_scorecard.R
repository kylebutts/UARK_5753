# https://collegescorecard.ed.gov/data/data-documentation/
library(tidyverse)
library(here)
raw <- read_csv(here(
  "Projects/Cross_Sectional/raw-data/college_scorecard/Most-Recent-Cohorts-Institution.csv"
))

scorecard <- raw |>
  filter(HIGHDEG >= 3) |> # must offer bachelor's degree
  filter(REGION %in% 2:8) |>
  select(
    institution = INSTNM,
    state = STABBR,
    region = REGION,
    ownership = CONTROL,
    hbcu = HBCU,
    predominetly_black_institution = PBI,
    admission_rate = ADM_RATE,
    sat_verbal_first_quartile = SATVR25,
    sat_verbal_median = SATVRMID,
    sat_verbal_third_quartile = SATVR75,
    sat_math_first_quartile = SATMT25,
    sat_math_median = SATMTMID,
    sat_math_third_quartile = SATMT75,
    online_only = DISTANCEONLY,
    n_students = UGDS,
    n_students_white = UGDS_WHITE,
    n_students_black = UGDS_BLACK,
    n_students_hispanic = UGDS_HISP,
    n_students_asian = UGDS_ASIAN,
    n_students_native = UGDS_AIAN,
    share_low_income = INC_PCT_LO, # < $30k
    avg_cost_attendance = COSTT4_A,
    tuition_in_state = TUITIONFEE_IN,
    tuition_out_of_state = TUITIONFEE_OUT,
    completion_rate = C150_4,
    completion_rate_white = C150_4_WHITE,
    completion_rate_black = C150_4_BLACK,
    completion_rate_hispanic = C150_4_HISP,
    completion_rate_asian = C150_4_ASIAN,
    completion_rate_native = C150_4_AIAN,
    load_default_rate_3yr = CDR3,
    mean_earnings_10yr_after = MN_EARN_WNE_P10,
    median_earnings_10yr_after = MD_EARN_WNE_P10,
  ) |>
  mutate(
    ownership = case_when(
      ownership == 1 ~ "Public",
      ownership == 2 ~ "Private nonprofit",
      ownership == 3 ~ "Private for-profit"
    ),
    region = case_when(
      region == 1,
      "New England (CT, ME, MA, NH, RI, VT)",
      region == 2,
      "Mid East (DE, DC, MD, NJ, NY, PA)",
      region == 3,
      "Great Lakes (IL, IN, MI, OH, WI)",
      region == 4,
      "Plains (IA, KS, MN, MO, NE, ND, SD)",
      region == 5,
      "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
      region == 6,
      "Southwest (AZ, NM, OK, TX)",
      region == 7,
      "Rocky Mountains (CO, ID, MT, UT, WY)",
      region == 8,
      "Far West (AK, CA, HI, NV, OR, WA)"
    )
  )

fs::dir_create(here("Projects/Cross_Sectional/data/college_scorecard/"))
write_csv(
  scorecard,
  file = here(
    "Projects/Cross_Sectional/data/college_scorecard/college_scorecard.csv"
  )
)
