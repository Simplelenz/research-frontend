library(readxl)
positionData <- read_excel("Desktop/positionData.xlsx")
pos <- subset(positionData, select = c("latitude", "longitude"))
rownames(pos) <- positionData$empName
View(pos)

distMat <- dist(pos, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

library(graphics)

loc <- cmdscale(distMat)
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly
# X11()
plot(x, y, type = "p", xlab = "", ylab = "", asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), pos = 4)

library(gatepoints)
selectedPoints <- fhs(loc, mark = TRUE)
