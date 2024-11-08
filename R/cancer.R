library(tidyverse)
library(janitor)
library(cards)
library(gtsummary)
library(kableExtra)
library(gt)
library(flextable)
library(officer)
library(tfrmt)

cancer <- utils::read.csv('./data/The_Cancer_data_1500_V2.csv') %>% rename_all(tolower) %>% 
  clean_names() %>% mutate(diagnosis=ifelse(diagnosis==1,'Cancer','No Cancer'),
                           smoking=ifelse(smoking==1,'Smoker','Non-Smoker'),
                           gender=ifelse(gender==1,'Male','Female'),
                           )

cancer2 <- cancer %>% mutate(diagnosis = 'Overall')

cancer <- bind_rows(cancer,cancer2)

save(cancer, file = "./data/cancer.RData")

theme_gtsummary_compact() # reduce default padding and font size for a gt table

# build the ARD with the needed summary statistics using {cards}
ard <-
  ard_stack(
    cancer, 
    ard_continuous(variables = c(age, bmi)),
    # add_overall(last = TRUE),
    ard_categorical(variables = c(smoking, gender)),
    .by = diagnosis,      # split results by treatment arm
    .attributes = TRUE, # optionally include column labels in the ARD
    .overall = FALSE,
    .total_n = TRUE
  ) 



# use the ARD to create a demographics table using {gtsummary}
final <- tbl_ard_summary(
  cards = ard, 
  by = diagnosis, 
  type = c(age ~ "continuous2", bmi ~ "continuous2"),
  label = c(age ~ "Age", bmi ~ 'BMI', smoking ~ 'Smoking', gender ~ 'Gender'),
  statistic = c(age ~ c("{N}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
                   bmi ~ c("{N}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}")
                   )
  )|> 
  bold_labels() |> 
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") |> # add Ns to header
  modify_footnote(everything() ~ NA) |> # remove default footnote 
  # add_overall() |>
  as_flex_table() %>% 
  set_caption('Summary of Cancer\n \n \n ', fp_par(padding = 20, line_spacing
  = 2) )
  
flextable::save_as_docx(final, path = './data/sample.docx')

