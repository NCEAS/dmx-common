# ADF&G Large Mesh Trawl Processing Script
# Colette Ward, Nov 2015
#########################################


# load packages
library(httr)
library(dplyr)

# run cleaning script
View(adfgLMT)

# exclude trawls with poor performance
unique(sort(adfgLMT$performance)) # [1] 0 1 2 4    ?1 = satisfactory, 2 = unsatisfactory, ...?
sum((adfgLMT$performance == 0)) # 182627
sum((adfgLMT$performance == 1)) # 604
sum((adfgLMT$performance == 2)) # 1456
sum((adfgLMT$performance == 4)) # 1419

# looks like performance == 0 means satisfactory

adfgLMT1 <- adfgLMT %>%
  filter(performance < 1)
unique(sort(adfgLMT1$performance)) # [1] 0


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 