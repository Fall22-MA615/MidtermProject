
## The purpose of this R script is to get you started on the
## midterm project. 

library(tidyverse)
library(magrittr)
library(readxl)

## Start by reading the data
strawb <- read_xlsx("strawberries-2022oct30-a.xlsx",col_names = TRUE)

## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

## Explore data by viewing it in R.  
## Double click the strawb data frame to lauch the view() function.
## The data frame has 1008 rows, so you can't get very far by
## simply scrolling around.  But, you can generate some initial
## questions to help you explore using R functions from the
## tidyverse.  
##
## It looks like some of the columns may be blank or may contain 
## a single unique value.  These columns can be eliminated without 
## losing any information.

## Start by examining the content of the columns

## Column 1 contains two unique values.  
## Retain column 1 -- those values might be needed.
unique(strawb[1])

## Column 2 -- contains the years included in this dataset.
## Keep column 2, of course.
unique(strawb[2])

## Column 3 -- contains the time periods covered by in the dataset.
## There's only one -- years.  No info here.  Drop it
unique(strawb[3])

## you don't have to do this one column at a time.
## Note that the cells of columns that are empty contain NA, so
## the number of unique values in these columns is 1, just 
## like column_3.

## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)


## Look at the strawb data frame again. You can see that the 
## columns need work. The State ANSI column contains a unique
## code for each state. If you need to access US Census data for
## the states, this code will come in handy.

colnames(strawb)

## now look at the `Data Item` column

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()

## Look at temp1!  There's a lot going on there.
## In fact, it's at least three columns packed into one.
## Use separate() to split it up

## When you run this code you can see that there are 
## some rows where `Data Item` has 4 comma-separated 
## data items.  Look at the warning on the Console 
## after 

strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")

## try 4 columns

strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

## That worked. Clean up the dat.

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")

## now explore the new columns

## we know that "THIRAM" is a chemical in the data, so
## test for it to check out the way code
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", 
                   strawb$`Domain Category`, 
                   ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                       strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                      strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                          strawb$`Domain Category`, 
                          ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                        strawb$`Domain Category`, 
                        ignore.case = T)

## We'll come back to chemicals after they 
## have their own column

## Let's continue cleaning up the columns

temp1 <- strawb %>% select(Strawberries) %>% 
  distinct()

pr_rec <- grep("STRAWBERRIES - PRICE RECEIVED", 
                  strawb$Strawberries, 
                  ignore.case = T)

## looks like it this might be a good time to split this analysis 
## into oranic and non organic -- and commercial vs chemicals in each.

## some data details
## NOP is certified organic
## see https://bityl.co/FOWS

## Look at the HELP for the NASS selector interface
## https://quickstats.nass.usda.gov/

## let's track down the organic entries

type_organic <- grep("organic", 
               strawb$type, 
               ignore.case = T)

items_organic <- grep("organic", 
                      strawb$items, 
                      ignore.case = T)  ## nothing here

Domain_organic <- grep("organic", 
                      strawb$Domain, 
                      ignore.case = T)


Domain_Category_organic <- grep("organic", 
                       strawb$`Domain Category`, 
                       ignore.case = T)

## OK.  So let's create a strawb_organic tibble
## All three are the same

same <- (intersect(type_organic, Domain_organic)==
         intersect(type_organic, Domain_organic))
length(same)==length(type_organic)


org_rows <- intersect(type_organic, Domain_organic)

strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)

## ok strawb has been split
## Doesn't look like we are going to get chemical data
## split by organic v non_organic
## so We will need to count on 
## https://bityl.co/FOcF
## https://bityl.co/FOcW

## now separate the chemical data

temp1 <- strawb_non_organic %>% select(type) %>% 
  distinct()

## the chemical data in the column "type" 
## is in the rows where type = BEARING - Applications

chem_rows <- grep("BEARING - APPLICATIONS", 
                       strawb_non_organic$type, 
                       ignore.case = T)
## of the 2172 rows in strawb_non_organic 2112 contain
## chemical data

## the column "Domain" also contains indications of rows 
## 

chem_rows_1 <- grep("chemical", 
                  strawb_non_organic$Domain, 
                  ignore.case = T)

## there are less chem rows in the Domain column
## than in the type column

ins <- intersect(chem_rows, chem_rows_1)

## the cardinality of the intersection equals the cardinality
## of chem_rows_1.  So, chem_rows calls all of the chem rows 
## we have found so far.

## Finally, we examine the `Domain Category`column.

chem_rows_2 <- grep("chemical", 
                    strawb_non_organic$`Domain Category`, 
                    ignore.case = T)

ins_2 <- intersect(chem_rows, chem_rows_2)

## again, all of the row numbers in chem_rows_2 are in 
## chem_rows


## now sort out these tibbles just like we did strawb
## Now, create a chem tibble


strawb_chem <- strawb_non_organic %>% slice(chem_rows, preserve = FALSE)

## now clean up the workspace before tackling the three tibbles just created.

rm(x, T, drop_cols, temp1, r_thiram, r_thiram_1,
   df_carbendazim, df_Bifenthrin, df_methyl_bromide, 
   df_1_3_dichloropropene, df_chloropicrin, df_Telone,
   pr_rec, type_organic, items_organic, Domain_organic,
   Domain_Category_organic, same, org_rows, chem_rows,
   chem_rows_1, chem_rows_2, ins, ins_2, cnames, i)

## now the environment lists the tibbles strawb, strawb_chem,
## strawb_non_organic, and strawb_organic

## first drop the "no-info" columns  Since we will need to do this 
## for all three tibbles, write a functin


  before_cols = colnames(strawb_chem)
  T = NULL
  x = length(before_cols)

   for(i in 1:x){
    b <- length(unlist(strawb_chem[,i] %>% unique()) )
    T <- c(T,b)
   }
    
  drop_cols <- before_cols[which(T == 1)]
  strawb_chem %<>% select(!all_of(drop_cols))
  after_cols = colnames(strawb_chem)

## drop_cols is labeling information
  
  temp1 <- strawb_chem %>% select(units) %>% distinct()
  ## in units rows are either NA or AVG
  
## separate Domain Category at :
  
  strawb_chem %<>% separate(col=`Domain Category`, 
                                    into = c("dc1", "chem_name"),
                                    sep = ":", 
                                    fill = "right")
  
temp1 <- strawb_chem %>% select(chem_name) %>% unique()
length(unlist(temp1))

## first clean it up

aa  <- grep("measured in", 
                    strawb_chem$items, 
                    ignore.case = T)
length(aa)

## so "item" is really units

## are Domain and dc1 the same thing?

sum(strawb_chem$Domain == strawb_chem$dc1) == dim(strawb_chem)[1]

## yes, they are. let's work with dc1. Also, drop State ANSI

strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)

## now you can see that this tibble needs to be wider.  
## clean up the entries before using pivot_wider

## items should only contain unit names
## rename units to category

strawb_chem %<>% rename(category = units)

## remove "MEASURED IN " 

strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")

## rename items to units
strawb_chem %<>% rename(units = items)

## Do all the dc1 entries begen with "Chemical"?

bb  <- grep("CHEMICAL, ", 
            strawb_chem$dc1, 
            ignore.case = T)
length(bb)
chem <- 1:2112

non_chem_rows <- setdiff(chem, bb)
length(non_chem_rows)

## on let's look at these rows in a tibble

temp1 <- strawb_chem %>% slice(non_chem_rows)

### !! fertilizers  

## keep them -- probably won't use them as a lone tibble

fertilizers <- temp1

## cleanup
rm(temp1, temps, temp3, aa, bb)

## now remove "CHEMICAL, " from the entries in the dc1
## and rename the column chem_types


strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")

strawb_chem$dc1 %>% unique()

strawb_chem %<>% rename(chem_types = dc1)


## Now let's get the units and categories sorted out
## we can see that the chemicals appear many times each
## to investigate, pick one

bb  <- grep("BIFENTHRIN", 
            strawb_chem$chem_name, 
            ignore.case = T)

bifen <- strawb_chem %>% slice(bb)

## now look at the befen tibble you just made

## now fix the chem_name column

## remove the parens

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")

## separate chem_name and chem_code

strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
                            ) 


## now fill in a label fot NA in the category column

## first check that "lb" in the units column corresponds 
## to NA in the category column


aa <- which(strawb_chem$units == " LB")

bb <- which(is.na(strawb_chem$category))

sum(aa==bb)==length(aa)

## yes, they correspond

## So, 

