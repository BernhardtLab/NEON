library(dplyr)
library(stringr)

resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'
df <- read.csv(paste0(resultspath,"cleanLengthMassRelationships.csv"), skip = 1)

names(df)<-c("Taxon","TaxonUsed","Regression","Reference")

df_clean <- df %>%
  mutate(
    a = case_when(
      str_detect(Regression, "10-") ~ 10^(-as.numeric(str_extract(Regression, "10-([0-9.]+)") %>% str_replace("10-", ""))),
      str_detect(Regression, "\\*") ~ as.numeric(str_extract(Regression, "=\\s*([0-9.eE-]+)") %>% str_remove("=\\s*")),
      TRUE ~ NA_real_
    ),
    b = as.numeric(str_extract(Regression, "BL([0-9.eE-]+)") %>% str_remove("BL")),
    EquationForm = ifelse(!is.na(a) & !is.na(b),
                          paste0("M = ", signif(a, 4), " × L^", signif(b, 4)),
                          NA)
  ) %>%
  filter(!is.na(a) & !is.na(b)) %>%
  select(Taxa, a, b, EquationForm, Reference)

head(df_clean)
