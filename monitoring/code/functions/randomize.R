# randomize SAM child and rectal IDs
set.seed(2)
library(tidyverse)

# function that requires a vector of IDs and the treatment assignments. 
# produces a data frame with treatment assignment for each ID
randomize <- function(ID_vec, treat_vec){
  
  # total IDs to assign
  n <- length(ID_vec)
  
  # randomize assignments
  rand <- runif(n)
  assignments <- ifelse(rand>.5, treat_vec[1], treat_vec[2])
  
  # bind together and return data frame
  t <- bind_cols(childID=ID_vec, assign=assignments)
  return(t)
}

# pull in IDs
df <- read.csv("monitoring/data/SAM_child_id.csv")
IDs <- as.character(df$id)

# treatment assignments 
treat <- c("Azithromycin", "Amoxicillin")

# randomize assignments
letters <- randomize(ID_vec = IDs, treat_vec = treat)

# save
write_csv(letters, "monitoring/tables/tr_letters_sam.csv")

# sample without replacement
t <- sample(450, 150)

# rectal ones
r <- IDs[t]

# non rectal ones
nr <- setdiff(IDs, IDs[t])

# make data
d <- data.frame(id=c(r, nr), rectal=c(rep(1,100), rep(0,350)))

# save
write_csv(d, "monitoring/tables/rectal_letter_sam.csv")


# fix the unbalanced treatment groups for rectal swabbing
# read in randomized ids
tr <- read_csv("monitoring/tables/tr_letters_sam.csv")
re <- read_csv("monitoring/tables/rectal_letter_sam.csv")

# extract already used IDs
ids <- pluck(df_list, "treatment") %>% distinct(childID) %>% pull

# subset to only ids that have not been used
tr <- tr %>% filter(childID %in% setdiff(childID, ids))
table(tr$assign)
re <- re %>% filter(id %in% setdiff(id, ids))

# join rectal and treatment allocation
b <- re %>%
  select(childID=id, rectal) %>%
  left_join(tr, by="childID")

b %>% group_by(rectal) %>% count(assign)