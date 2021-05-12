library(ggpubr)
library(scatterplot3d)
library(ggplot2)
library(corrplot)
library(caret)

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

num.wide.data <- data[,-1]
data.wide <- data[,-1]
data.wide[["hair"]] <- as.logical(data.wide[["hair"]])
data.wide[["feathers"]] <- as.logical(data.wide[["feathers"]])
data.wide[["eggs"]] <- as.logical(data.wide[["eggs"]])
data.wide[["milk"]] <- as.logical(data.wide[["milk"]])
data.wide[["airborne"]] <- as.logical(data.wide[["airborne"]])
data.wide[["aquatic"]] <- as.logical(data.wide[["aquatic"]])
data.wide[["predator"]] <- as.logical(data.wide[["predator"]])
data.wide[["toothed"]] <- as.logical(data.wide[["toothed"]])
data.wide[["backbone"]] <- as.logical(data.wide[["backbone"]])
data.wide[["breathes"]] <- as.logical(data.wide[["breathes"]])
data.wide[["venomous"]] <- as.logical(data.wide[["venomous"]])
data.wide[["fins"]] <- as.logical(data.wide[["fins"]])
data.wide[["tail"]] <- as.logical(data.wide[["tail"]])
data.wide[["domestic"]] <- as.logical(data.wide[["domestic"]])
data.wide[["catsize"]] <- as.logical(data.wide[["catsize"]])



corr <- round(cor(data.wide), 1)

cp <- corrplot(corr,method = "number",type = "upper")

summary(data.wide)


