library(tidyverse)
# read in the landscape metrics for 2006 and 2018
land_2006 <- read_csv("combined_dat_2006.csv")
land_2018 <- read_csv("combined_dat_2018.csv")
# select only rows with landscape level metrics
landscape_2006 <- land_2006 %>% filter(level == "landscape")
landscape_2018 <- land_2018 %>% filter(level == "landscape")
# save to csv files
write_csv(landscape_2006, "landscape_2006.csv")
write_csv(landscape_2018, "landscape_2018.csv")

# Select only quadrant number, metrics and values
l_2006 <- landscape_2006 %>% select(Quadrat, metric, value)
l_2018 <- landscape_2018 %>% select(Quadrat, metric, value)
# rename column value, so that we can join tables for both years later
l_r_2006 <- l_2006 %>% rename(value_2006 = value)
l_r_2018 <- l_2018 %>% rename(value_2018 = value)

# join the tables on quadrat and metric as a key
landscape_joined <- full_join(l_r_2006, l_r_2018)
write_csv(landscape_joined, 'landscape_joined.csv')
# calculate change by extracting 2006 values from 2018 values
land_change <- landscape_joined %>% mutate(l_change = value_2018 - value_2006)
write_csv(land_change, "land_change.csv")

# Preprocess biodiversity data. First read it in
biodiv <- read_csv("ATLAS_DATA_LOB_2020.csv")
# split it into two periods ( names containing "_l" are from  survey 2000-2004
# names containing "_e" are from 2013-2017 survey)
e_2013_2017 <- biodiv %>% select(`5x5 SQUARE`, contains("_e"))
l_2000_2004 <- biodiv %>% select(`5x5 SQUARE`, contains("_l"))

write_csv(e_2013_2017, 'e_2013_2017.csv')
write_csv(l_2000_2004, "l_2000_2004.csv")

# count sums of species by quadrat
sum_2000_2004 <- l_2000_2004 %>% mutate(total = rowSums(pick(where(is.numeric))))
sum_2013_2017 <- e_2013_2017 %>% mutate(total = rowSums(pick(where(is.numeric))))

write_csv(sum_2000_2004, 'sum_2000_2004.csv')
write_csv(sum_2013_2017, 'sum_2013_2017.csv')

# to join the tables we need to rename columns called total
sum_2000_2004_r <- sum_2000_2004 %>% rename(total_2000_2004 = total)
sum_2013_2017_r <- sum_2013_2017 %>% rename(total_2013_2017 = total)

# join the tables on `5x5 SQUARE` as a key
joined_bio_sums <- full_join(sum_2000_2004_r, sum_2013_2017_r)

# now we count species richness change by extracting total_2000_2004 from
# total_2013_2017
change <- joined_bio_sums %>% mutate(change = total_2013_2017 - total_2000_2004)
write_csv(change, 'change.csv')

# drop unnecessary fields
change_short <- change %>% select(`5x5 SQUARE`, total_2000_2004, total_2013_2017, change)
write_csv(change_short, 'change_short.csv')

# read species richness change by quadrant in
bio_change <- read_csv("change_short.csv")
# join tables on  landscape metrics change with biodiversity change
l_and_bio_ch <- full_join(land_change, bio_change, by = c("Quadrat" = "5x5 SQUARE"))
write_csv(l_and_bio_ch, "l_and_bio_ch.csv")
# drop unnecessary fields
l_and_bio_ch_short <- l_and_bio_ch %>% select(Quadrat, metric, l_change, change)
# calculate correlation between landscape change and species richness change
# grouped by metric. I think this way we can identify
# changes in which metrics have the strongest effects ob biodiversity
cor_l_b <-  l_and_bio_ch_short %>% group_by(metric) %>% summarise(cor=cor(l_change, change))
write_csv(cor_l_b, 'cor_l_b.csv')
# now we can arrange by absolute value in a desc order to see
# which metrics have the strongest impact on biodiv
arr_cor_l_b <- arrange(cor_l_b, desc(abs(cor)))
write_csv(arr_cor_l_b, "arr_cor_l_b.csv")
