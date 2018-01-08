#Installation of necessary packages
install.packages("stringdist")
install.packages("dplyr")
install.packages("tidyr")

library(stringdist)
library(dplyr)
library("tidyr")

#Loading file
refinedata <- read.csv("refine_original.csv")
companynames <- c("philips","akzo","vanhouten","unilever")

i <- amatch(refinedata$company,companynames,maxDist=4)
refinedata$company <- companynames[i]

# Creating separate product category column for each product
refinedata <- refinedata %>%
  separate(Product.code...number,c("Product","number")) %>%
  mutate(ProductCategory= ifelse(Product=="p","Smartphone",if_else(Product=="v","TV",if_else(Product=="x","Laptop","Tablet")))) %>%
  mutate(present = 1) %>%
  spread(company,present,fill = 0) %>%
  mutate(yesno=1) %>%
  spread(ProductCategory,yesno,fill = 0)

#Concatenating Address 
refineclean <- refinedata %>%
  unite(full_address,c("address","city","country"),sep = ",")