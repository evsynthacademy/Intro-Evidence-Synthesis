library(rentrez)

search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

year <- 1940:2017
papers <- sapply(year, search_year, term="((((pain management[MeSH Terms]) OR analgesia[MeSH Terms]) AND neonate[MeSH Terms]) NOT labor) NOT cesarean ", USE.NAMES=FALSE)
library(ggplot2)
df <- data.frame(cbind(year,papers))
ggplot(df) + ggtitle("Research Studies of Newborn Babies and Pain") + geom_bar(aes(year,papers), stat = "identity", 
                                                                               position="dodge", color="dodgerblue", fill="lightblue") + theme_minimal() +  
  scale_x_discrete(breaks = seq(1939, 2017,5), 
                   limits=seq(1935,2021), name="Publication Year") + scale_y_discrete(breaks = seq(0, 100,10), 
                                                                                      limits=seq(0, 80), name="Count") +  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  annotate("text", x = 1965, y = 20, label = "Anand & Hickey (1987)", size=4) + annotate("segment", 
                                                                                         x = 1975, xend = 1987, y = 20, yend = 4, colour = "dodgerblue3") + 
  theme(plot.title = element_text(hjust = 0.5, margin=margin(b=20)),  panel.grid.major = element_line(color = "gray90",
                                                                                                      size = 0.5), panel.grid.major.x = element_blank())

