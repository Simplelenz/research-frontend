library(readxl)
library(FactoMineR)
caData <- read_excel("Desktop/caData.xlsx")
data <- subset(caData, select = c("seat", "meeting room", "cafeteria", "home", "lobby"))
rownames(data) <- caData$X__1
res.ca <- CA(data)
