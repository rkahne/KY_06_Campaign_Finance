library(tidyverse)
library(stringr)

fec_headers <- names(read_csv('./data/indiv_header_file.csv'))
full_fec <- read_delim('./data/itcont.txt', delim = '|', col_names = fec_headers)

amy_mcgrath <- full_fec %>% 
  mutate(MCGRATH = str_detect(MEMO_TEXT, 'MCGRATH')) %>% 
  filter(CMTE_ID == 'C00401224',
         MCGRATH == T)

write_csv(amy_mcgrath, './data/MCGRATH_actblue.csv')

amy_mcgrath %>% 
  summarize(total = sum(TRANSACTION_AMT))
