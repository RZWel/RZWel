# Quick recap of what I did previously.
library(tidyverse)

url <- paste0(
  "https://archive.ics.uci.edu/",
  "ml/machine-learning-databases/abalone/abalone.data"
)

abalone_raw <- read_csv(url, col_names = FALSE)
abalone <-
  abalone_raw %>%
  rename("sex"=X1,"Length"=X2,"Diameter"=X3,"Height"=X4,"Whole weight"=X5,"Shucked weight"=X6,"Viscera weight"=X7,"Shell weight"=X8,"Number of Rings"=X9) 

abalone <-
  mutate(abalone$age<-abalone$`Number of Rings`+1.5) # mutated the column by forming a numerical value.

  

Name<-c("sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Number of Rings","Age")
dat.ty=c('nominal','continuous','continuous','continuous','continuous','continuous','continuous','continuous','Integer', 'continous')
Meas=c('-','mm','mm','mm','grams','grams','grams','grams','-','year')
data=data.frame(col1=Name,col2=dat.ty,col3=Meas)
view(data)
  select(abalone,Diameter)
  #my_df <- as.data.frame(abalone)
  #str(my_df)
  #typeof(abalone)
  
  
  # Task 2. plotting 
  
  p1 <-
  abalone %>%
    ggplot(aes(age)) + 
    geom_histogram(binwidth = 1.0)   #With a binwidth of 1, you can clearly see the different 
  
              
  p2 <- 
    abalone %>%
    ggplot(aes(age)) +
    geom_density(bw=1)
    geom_rug()
  p3 <- 
    abalone %>%
    ggplot(aes(age)) +
    geom_boxplot()
  
  
  abalone %>%
    ggplot(aes(age, Diameter,color=sex)) +
      geom_jitter(alpha = 0.5, height = 0, width = 0.5) +
      scale_color_brewer(palette = "Set2") + 
      labs(
      y = "Diameter (mm)",
      x = "Age (years)",
      color="sex"
    )
  
  
  p <- ggplot(mpg, aes(cyl, hwy))
  p+ geom_jitter(aes(colour = class))
  
    