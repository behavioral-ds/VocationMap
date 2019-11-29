## Script to build the Vocation Map from occupation profiles The idea is to
## first compute the mean value per profession over the big10, cluster them into
## 20 clusters, apply tSNE to visualize them on a 2D plot.
##
## author: Marian-Andrei Rizoiu (Marian-Andrei@rizoiu.eu)

require(readr)
require(cluster)
require(Rtsne)
require(fpc)
require(plotly)
require(stylo)
require(RColorBrewer)
require(dplyr)
library(ggplot2)
require(ggrepel)
source("scripts/utils.R")

# load dataset of occupation profiles. 12 Fields 
# `Profession nams`, 
# the Big-5, 
# the personal values (5 fields)
# the number of users with that profession in the initial dataset.
df <- read.csv("data/profession-profiles.csv")
nclus <- 20

################# Start buiding the Vocation Map ############ 
## first, cluster professions into 20 clusters
clust <- pam(x = df[, ! names(df) %in% c("Profession", "n")], k = nclus)
myLabs <- as.character(df$Profession[clust$id.med])

## set colors
colors <- c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12), brewer.pal(name="Accent", n = nclus - 19))
colors <- colors[colors != "#FFFF99"]
colors <- colors[1:nclus]
names(colors) <- myLabs

########### Starting plotting ###############
## First, reduce dimensionality to 2 dimensions using t-SNE
tsne <- Rtsne(df[, ! names(df) %in% c("Profession", "n")], dims = 2, perplexity=30, verbose=TRUE, max_iter = 1000)

## Plotting
myNames <- c("Profession", "Openness", "Conscientousness", "Extraversion", "Agreeableness", "Emotional_Range", 
             "Conversation", "Openness to Change", "Hedonism", "Self-enhancement", "Self-transcendence", "n", "X", "Y", "Cluster")
toPlot <- cbind(df, tsne$Y, Cluster = as.factor(as.character(myLabs[clust$clustering])) )
names(toPlot) <- myNames
# toPlot$Cluster <- toPlot$lbl
# toPlot$lbl <- NULL

squarePlot <- as.character(toPlot$Cluster) == as.character(toPlot$Profession)
toPlot$ProfessionPlot <- gsub(pattern = " ", replacement = "\n", x = toPlot$Profession)
toPlot$ProfessionPlot <- gsub(pattern = "\n[(]CFP[)]", replacement = "", x = toPlot$ProfessionPlot)
toPlot$ProfessionPlot <- gsub(pattern = "Director\nof", replacement = "Director of", x = toPlot$ProfessionPlot)
toPlot$ProfessionPlot <- gsub(pattern = "Health\nCare", replacement = "Health Care", x = toPlot$ProfessionPlot)
toPlot$HooverTip <- paste(toPlot$Profession, " (", toPlot$n, " users)", sep = "")

t <- list(
  family = "sans serif", size = 18) # size = 14 ) # color = toRGB("grey50") )
p <- plot_ly(data = toPlot, x = ~X, y = ~Y, text = ~HooverTip) %>%
  add_markers(alpha = 0.7, type = 'scatter', 
              color = ~Cluster, size = 25,
              colors = colors,
              hoverinfo = "text", textposition = "middle right") %>%
  # add_markers(data = toPlot[squarePlot,], x = ~X, y = ~Y, size = 10, alpha = 1,
  #             color = ~Cluster,
  #             colors = "Paired", showlegend = FALSE) %>%
  # add_text(textfont = t, textposition = "top right") #%>%
  add_text(data = toPlot[squarePlot,], x = ~X, y = ~Y, text = ~ProfessionPlot, showlegend = FALSE, ## before: text = ~Profession
           textposition = "center", textfont = t) %>% 
  layout(title = "The Vocation Map (interactive)", xaxis = list(title = ""), yaxis = list(title = ""))
# color = ~Cluster, colors = "Paired", ) 
# add_trace(data = toPlot[squarePlot,], x = ~X, y = ~Y,
# symbol = 'x',
# showlegend = FALSE)
show(p)

###################### Generate the static VocationMap %%%%%%%%%%%%%%%%%%%%%%
toPlot$ProfessionPlot <- gsub(pattern = " ", replacement = "\n", x = toPlot$Profession)

### now need to create a static version of the map
plot(x = toPlot$X, y = toPlot$Y, pch = 19, col = add.alpha(colors, alpha=0.5)[clust$clustering],
     main="", xlab = "", ylab = "")
text(x = toPlot[squarePlot, "X"], y = toPlot[squarePlot, "Y"], labels = toPlot[squarePlot, "ProfessionPlot"], cex=0.75) # , pos=3
# legend("topright", legend = names(colors), pch = 1, col = colors, bty = "n")

png(file = "plots/professions-explore-20-cluster.png", res = 200, width = 8, height = 6, units = "in")
ggplot(data = toPlot, aes(x = X , y = Y, color = Cluster)) + 
  geom_point(size = 3) +
  scale_color_manual(values=add.alpha(colors, alpha=0.6), guide=FALSE) +
  geom_label(data = toPlot[squarePlot,], aes(x = X, y = Y, label = ProfessionPlot), 
             fill = add.alpha(colors[order(myLabs)], alpha=0.4), label.size = NA, size = 3, color = "black", fontface = "bold") + #family = "sans serif",
  # theme(panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
  theme_bw() +
  theme( axis.title.x=element_blank(), axis.title.y=element_blank())
dev.off()

