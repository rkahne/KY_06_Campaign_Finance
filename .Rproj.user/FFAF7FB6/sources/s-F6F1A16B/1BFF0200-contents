library(tidyverse)
library(stringr)
library(scales)
library(forcats)
library(magrittr)
library(treemap)

thomas_raw <- read_csv("./data/THOMAS_efile-2017-10-16T10-23-45.csv")
mcgrath_raw <- read_csv("./data/MCGRATH_efile-2017-10-16T12-59-30.csv")
mcgrath_processed <- read_csv("./data/MCGRATH_schedule_a-2017-10-16T09-59-31.csv")
mcgrath_actblue <- read_csv('./data/MCGRATH_actblue.csv')


q3 <- thomas_raw %>% 
  mutate(candidate = 'Thomas',
         contributor_zip = as.character(contributor_zip),
         contribution_receipt_date = as.Date(contribution_receipt_date, format = '%m/%d/%Y')) %>%
  select(-load_timestamp) %>% 
  bind_rows(mcgrath_raw %>% 
              mutate(candidate = 'McGrath') %>%
              select(-load_timestamp))


## Total Contribution
q3 %>% 
  group_by(committee_name) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  ggplot(aes(x = committee_name, y = total, label = dollar_format()(total))) +
  geom_bar(stat = 'identity') +
  geom_label() +
  theme_minimal() + 
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = '', y = 'Total Raised', title = 'Total Raised by Candidate', subtitle = 'As of 10/16/17')

## Total Contribution By State
q3 %>% 
  filter(candidate == 'McGrath',
         conduit_committee_name != 'ACTBLUE' | is.na(conduit_committee_name),
         contributor_last_name != 'ActBlue') %>% 
  select(contributor_state,contribution_receipt_amount) %>%
  bind_rows(mcgrath_actblue %>% 
              select(contributor_state = STATE, contribution_receipt_amount = TRANSACTION_AMT)) %>% 
  group_by(contributor_state) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  bind_rows(tibble(contributor_state = 'PAC', total = 55162.3)) %>% 
  filter(total > 1000, !is.na(contributor_state)) %>% 
  ggplot(aes(x = fct_reorder(contributor_state, total), y = total, label = dollar_format()(round(total)))) +
  geom_bar(stat = 'identity') +
  geom_text(angle = 90, aes(hjust = 0)) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(), limits = c(0, 150000)) +
  labs(title = 'Amy McGrath Raised By State',
       subtitle = 'States > $1000 raised',
       x = '', y = '')

q3 %>% 
  filter(candidate == 'Thomas') %>% 
  group_by(contributor_state) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  arrange(desc(total)) %>% 
  ggplot(aes(x = fct_reorder(contributor_state, total), y = total, label = dollar_format()(round(total)))) +
  geom_bar(stat = 'identity') +
  geom_text(angle = 90, aes(hjust = 0)) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(), limits = c(0, 110000)) +
  labs(title = 'Reggie Thomas Raised By State',
       x = '', y = '')

## Contribution Amounts
mcgrath_raw %<>% 
  mutate(cancelled = pmap_lgl(list(contributor_first_name, contributor_last_name, contribution_receipt_amount),
                              function(fname, lname, contribution){
                                ifelse(nrow(filter(mcgrath_raw, contributor_first_name == fname, 
                                       contributor_last_name == lname, 
                                       contribution_receipt_amount == -1 * contribution)) > 0, T, F)
                              }))
thomas_raw %<>% 
  mutate(cancelled = pmap_lgl(list(contributor_first_name, contributor_last_name, contribution_receipt_amount),
                              function(fname, lname, contribution){
                                ifelse(nrow(filter(thomas_raw, contributor_first_name == fname, 
                                                   contributor_last_name == lname, 
                                                   contribution_receipt_amount == -1 * contribution)) > 0, T, F)
                              }))

mcgrath_actblue %<>% 
  mutate(cancelled = map2_lgl(NAME, TRANSACTION_AMT,
                              function(name, contribution){
                                ifelse(nrow(filter(mcgrath_actblue, NAME == name, 
                                                   TRANSACTION_AMT == -1 * contribution)) > 0, T, F)
                              }))

mcgrath_raw %>% 
  filter(cancelled == F) %>%
  mutate(name = paste(contributor_first_name, contributor_middle_name, contributor_last_name, contributor_suffix)) %>% 
  group_by(name) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  mutate(act_blue = str_detect(name, ignore.case('ACTBLUE'))) %>% 
  filter(act_blue == F) %>% 
  select(-act_blue) %>% 
  bind_rows(mcgrath_actblue %>% 
              filter(cancelled == F) %>% 
              select(name = NAME, TRANSACTION_AMT) %>% 
              group_by(name) %>% 
              summarize(total = sum(TRANSACTION_AMT))) %>% 
  mutate(candidate = 'McGrath') %>% 
  bind_rows(thomas_raw %>% 
              filter(cancelled == F) %>%
              mutate(name = paste(contributor_first_name, contributor_middle_name, contributor_last_name, contributor_suffix)) %>% 
              group_by(name) %>% 
              summarize(total = sum(contribution_receipt_amount)) %>% 
              mutate(candidate = 'Thomas')) %>% 
  filter(total < 10000, total > 0, str_detect(name, 'Wrage') == F) %>% 
  bind_rows(tibble(name = c('Stephen Wrage', 'Alexandra Wrage'), total = c(5400, 5400), candidate = c('McGrath', 'McGrath'))) %>% 
  ggplot(aes(x = candidate, y = total)) +
  geom_jitter(alpha = 0.1) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = 'Individual Contribution Amounts', x = '', y = '')




indiv_contributors <- mcgrath_raw %>% 
  filter(cancelled == F) %>%
  mutate(name = paste(contributor_first_name, contributor_middle_name, contributor_last_name, contributor_suffix)) %>% 
  group_by(name) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  mutate(act_blue = str_detect(name, ignore.case('ACTBLUE'))) %>% 
  filter(act_blue == F) %>% 
  select(-act_blue) %>% 
  bind_rows(mcgrath_actblue %>% 
              filter(cancelled == F) %>% 
              select(name = NAME, TRANSACTION_AMT) %>% 
              group_by(name) %>% 
              summarize(total = sum(TRANSACTION_AMT))) %>% 
  mutate(candidate = 'McGrath') %>% 
  bind_rows(thomas_raw %>% 
              filter(cancelled == F) %>%
              mutate(name = paste(contributor_first_name, contributor_middle_name, contributor_last_name, contributor_suffix)) %>% 
              group_by(name) %>% 
              summarize(total = sum(contribution_receipt_amount)) %>% 
              mutate(candidate = 'Thomas')) %>% 
  filter(total < 10000, total > 0, str_detect(name, 'Wrage') == F) %>% 
  bind_rows(tibble(name = c('Stephen Wrage', 'Alexandra Wrage'), total = c(5400, 5400), candidate = c('McGrath', 'McGrath')))



thomas_raw %>% 
  filter(cancelled == F) %>% 
  mutate(contributor_occupation = str_to_lower(contributor_occupation)) %>% 
  group_by(contributor_occupation) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  treemap(dtf = ., index = 'contributor_occupation', vSize = 'total',
          title = 'Reggie Thomas Contributors By Occupation')

mcgrath_raw %>% 
  filter(conduit_committee_name != 'ACTBLUE' | is.na(conduit_committee_name),
         contributor_last_name != 'ActBlue', cancelled == F,
         !memo_text %in% c('* Earmarked Contribution: See Below', 
                          '* Earmarked Contribution: See Below **US passport on file.')) %>% 
  group_by(contributor_occupation) %>% 
  summarize(total = sum(contribution_receipt_amount)) %>% 
  ungroup() %>% 
  bind_rows(mcgrath_actblue %>% 
              filter(cancelled == F) %>% 
              rename(contributor_occupation = OCCUPATION) %>% 
              group_by(contributor_occupation) %>% 
              summarize(total = sum(TRANSACTION_AMT)) %>% 
              ungroup()) %>% 
  mutate(contributor_occupation = str_to_lower(contributor_occupation)) %>% 
  group_by(contributor_occupation) %>% 
  summarize(total = sum(total)) %>% 
  filter(total > 0) %>% 
  treemap(index = 'contributor_occupation', vSize = 'total',
          title = 'Amy McGrath Contributors By Occupation')



