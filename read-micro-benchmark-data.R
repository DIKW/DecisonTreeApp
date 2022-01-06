# read micro benchmark data

# lees xls
library(readxl)
Huurdersoordeel_Woningcorporaties <- read_excel("data/Huurdersoordeel - Woningcorporaties.xlsx", skip = 1)

library(janitor)
library(dplyr)

df <- clean_names(Huurdersoordeel_Woningcorporaties)

# rename corporatie
df <- df %>% rename(corporatie=x1)

# alles numeriek maken
df <- df %>% mutate(across(-c(corporatie,
                              letter_huurdersoordeel_letters_3_klasse_2021,
                              deelletter_nieuwe_huurders_letters_3_klasse_2021,
                              deelletter_huurders_met_een_reparatieverzoek_letters_3_klasse_2021,
                              deelletter_vertrokken_huurders_letters_3_klasse_2021
                              ),as.numeric))

# missings numerics imputeren met mediaan
df <- df %>% mutate(across(where(is.numeric),~if_else(is.na(.x),median(.x, na.rm = T),.x)))

getMode <- function(x) {
  keys <- na.omit(unique(x))
  keys[which.max(tabulate(match(x, keys)))]
}

# oeps beetje een mess
summary(as.factor(df$letter_huurdersoordeel_letters_3_klasse_2021))

# missings characters imputeren met mode
#df <- df %>% mutate(across(where(is.character),~if_else(is.na(.x),getMode(.x, na.rm = T),.x)))

# beter is alle ruis weg te gooien..
df <- df %>% filter(letter_huurdersoordeel_letters_3_klasse_2021 %in% c('A','B','C'))

write.csv(df,'./data/micro-dataset.csv')
