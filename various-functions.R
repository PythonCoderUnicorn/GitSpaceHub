
#------- ROMAN NUMERALS 
library(purrr)

roman_numbers = c("I","II","III","IV","V","L","XL")

purrr::map_dbl(roman_numbers,  ~as.roman(.))
