library(ggpubr)
library(scatterplot3d)
library(ggplot2)

head <- c("animal name","hair",
          "feathers","eggs","milk","airborne","aquatic","predator",
          "toothed","backbone","breathes","venomous","fins","legs",
          "tail","domestic","catsize","type")

data <- read.table("zoo.data",header = FALSE, sep = ",")
colnames(data) <- head

ph <- gghistogram(
  data,
  x = "type",
  add = "mean",
  color = "#1631D8",
  bins = 7
)

p <- ggboxplot(
  data,
  x = "type"
  , y = "legs",add = "mean"
  , add.params = list(color = "#FC4E07"),
  color = "legs",title = "Cantidad de patas por tipo",ylab = "Cantidad de piernas"
)
