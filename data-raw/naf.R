library(readxl)
library(tricky)
library(purrr)
library(entreprises)
naf_hierarchy <- read_excel(path = "data-raw/naf2008_5_niveaux.xls")

naf_libelle <- read_excel(path = "data-raw/int_courts_naf_rev_2.xls") %>%
  set_standard_names() %>%
  select(-ligne) %>%
  filter(is.na(code) == FALSE) %>%
  mutate(code = sub(pattern = "^SECTION[[:blank:]]([[:alpha:]]+)", x = code, replacement = "\\1"))

get_unitelegale(siren = "382357721") %>%
  select(siren, activite_principale) %>%
  left_join(
    y = read_excel(path = "data-raw/naf2008_5_niveaux.xls"),
    by = c("activite_principale" = "NIV5")
    ) %>%
  left_join(y = naf_libelle, by = c("NIV1" = "code"))

