library(ggpubr)
library(scatterplot3d)

head <- c("animal name","hair",
          "feathers","eggs","milk","airborne","aquatic","predator",
          "toothed","backbone","breathes","venomous","fins","legs",
          "tail","domestic","catsize","type")

data <- read.table("zoo.data",header = FALSE, sep = ",")
colnames(data) <- head
