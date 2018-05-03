# Packages
library(data.table)            # Data manipulation
library(DT)                    # Data manipulation
library(knitr)                 # Dynamic Report Generation in R
library (ggplot2)              # Data visualization
library(caret)                 # Classification and Regression Training -  for any machine learning algorithms
library(jsonlite)              # Json manipulation
library(DMwR)                  # Statistics & Outliers manipulation
library(scales)                # Data visualization
library(chron)                 # Create chronological objects which represent dates and times of day

#' Read the files in the folder
sales <- fread(input = paste0(path_to_data, "/datav0.5p-final.txt"))

#' Introduce transaction ID to sales data.table (Adding ID column)
sales[, transID := .I]

#' A basic idea of what it looks like
dim(sales)
names(sales)
str(sales)
sum(is.na(sales))

#' Re-order data.table column 
setcolorder(x = sales, neworder = c("transID", "Date", "Model", "Price", "Sex", "Age", "Incomes", "Color"))
kable(head(x = sales, 10))

#' Save the tables as RData object
saveRDS(object = sales, file = paste0(path_to_clean_data, "/sales.rds"))

#' Numeric variables 
numVar <- sales[, c(1, 4, 6, 7)]
summary(numVar[, c(2:4)])

#' Create separate boxplots for each attribute
numVarMelt <- melt(numVar, id.vars = 'transID')
ggplot(data = numVarMelt, mapping = aes(x = variable, y = value)) + 
  geom_boxplot(fill = '#56B4E9', outlier.colour = "black", outlier.shape = 16,
               outlier.size=2) +
  facet_wrap( ~ variable, scales = "free") +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 7)

kable(summary(numVar[, -1]))

# Replace outliers by NA 
numVar$Age[numVar$Age < 18 | numVar$Age > 83] <- NA

#' Now, I perform knn imputation 
knnOutput <- knnImputation(numVar)
anyNA(knnOutput)

#' Finaly, I replace old column Age in sales data.table by new column Age from knnOutput data.table :
sales$Age <- knnOutput$Age
summary(sales$Age)

#' Visualizing Age boxplot again after Age imputation with Prediction method 
ggplot(data = sales, mapping = aes(x = '', y = Age)) + 
  geom_boxplot(fill = '#56B4E9', outlier.colour = "black", outlier.shape = 16,
               outlier.size=2) +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 7)

#' Date, Model, Sex & Color
sales$Date <- ifelse(test = sales$Date %in% c("week", "weekend"), 
                     yes = sales$Date,
                     no = ifelse(test = is.weekend(seq(Sys.Date()-10, Sys.Date(), by = 1)), 
                                 yes = "weekend", 
                                 no = "week"))

sales$Color <-ifelse(test = sales$Color == "noir", 
                    yes = "black", 
                    no = ifelse(test = sales$Color == "bleu", 
                                yes = "blue", 
                                no = ifelse(test = sales$Color == "vert", 
                                            yes = "green", 
                                            no = ifelse(test = sales$Color == "jaune", 
                                                        yes = "yellow", 
                                                        no = ifelse(test = sales$Color == "rouge", 
                                                                    yes = "red", no = sales$Color)))))
cols <- c("Date", "Model", "Sex", "Color")
setDT(sales)[, (cols):= lapply(.SD, as.factor), .SDcols=cols]
unlist(lapply(sales, class))

catVar <- sales[, c(1:3, 5, 8)]
lapply(catVar[, c(2:5)], pct)


#' Create separate barplots for each attribute

#' Meltin data
catVarMelt <- melt(catVar, id.vars = 'transID')
freq <- catVarMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]

#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 2, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582", "#FF9999")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "consumers that obtain the best price offer")

# Sales, Sales Revenue, Mean Incomes, Mean Price, Mean Age
cols <- c("Price", "Age", "Incomes")
sales[, lapply(.SD, mean), .SDcols = cols]
sum(sales$Price/1)


#''Gender Analysis
#' In this step, I want to explore sales by gender
table(sales$Sex, sales$Date)
genderVarCat <- sales[, c(2, 3, 5, 8)]

#' Meltin data
genderMelt <- melt(genderVarCat, id.vars = 'Sex')
freq <- genderMelt[, .N, by = c("Sex", "variable", "value")]
freq[, percent := (N/sum(N))*100, by = c("value")]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
p1 <- ggplot(freq, aes(x = value, y = percent, fill = Sex)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("#F4A582","#D1E5F0", "#56B4E9")) +
  theme(legend.position="bottom") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "", fill = "Gender") +
  # set transparency
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave("gender.png", p1, bg = "transparent")
  
#' What is the profile of buyers by age and gender ?*

#' Categorize Age in group
agebreaks <- c(18, 26, 36, 46, 56, 66, Inf)
agelabels <- c("18-25","26-35","36-45","46-55","56-65","66+")

setDT(sales)[ , agegroups := as.factor(cut(Age, 
                                           breaks = agebreaks, 
                                           right = FALSE, 
                                           labels = agelabels))]

#' Meltin data
ageVarMelt <- melt(sales[, c(5, 9)], id.vars = 'Sex')
freq <- ageVarMelt[, .N, by = c("Sex", "value")]
freq[, percent := (N/sum(N))*100, by = c("value")]

#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
p2 <- ggplot(freq, aes(x = value, y = percent, fill = Sex)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  scale_fill_manual(values = c("#F4A582","#D1E5F0", "#56B4E9")) +
  theme(legend.position="bottom") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "", fill = "Gender") +
  # set transparency
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave("age.png", p2, bg = "transparent")

#' what model for what age groups ?

#' Meltin data
ageVarMelt <- melt(sales[Model != "Talisman", c(3, 9)], id.vars = 'Model')
freq <- ageVarMelt[, .N, by = c("Model", "value")]
freq[, percent := (N/sum(N))*100, by = c("value")]

#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
p3 <- ggplot(freq, aes(x = value, y = percent, fill = Model)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582")) +
  theme(legend.position = "bottom") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "", fill = "Model") +
  # set transparency
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave("model.png", p3, bg = "transparent")

#' Which model generate the largest revenue ?
statModel <- sales[, list(total = .N, mean = round(mean(Price)), min = min(Price),
                          max = max(Price), revenue = sum(Price)), by = Model]
kable(statModel[order(-statModel$revenue),])

#' And, in color ?
statModelbyColor <- sales[, list(total = .N, revenue = sum(Price)), by = c("Model", "Color")]
kable(head(statModelbyColor[order(-statModelbyColor$revenue),],5))

#' Who are the major purchasers of yellow Clio ? 
yellowClio <- sales[Model == "Clio" & Color =="yellow", ]

#' Meltin data
yellowClioMelt <- melt(yellowClio[, c(1, 2, 5, 9)], id.vars = 'transID')
freq <- yellowClioMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "Buyers of the yellow Clio")

kable(summary(yellowClio[, c(4, 7)]))

#' What if, we compare them with the purchasers of black Kadjar ? do we obtain the same profile ?
blackKadjar <- sales[Model == "Kadjar" & Color =="black", ]

#' Meltin data
blackKadjarMelt <- melt(blackKadjar[, c(1, 2, 5, 9)], id.vars = 'transID')
freq <- blackKadjarMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "Buyers of the black Kadjar")

kable(summary(blackKadjar[, c(4, 7)]))

#' What if, we compare them with the purchasers of red Clio ? do we obtain the same profile ?
redClio <- sales[Model == "Clio" & Color =="red", ]

#' Meltin data
redClioMelt <- melt(redClio[, c(1, 2, 5, 9)], id.vars = 'transID')
freq <- redClioMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "Buyers of the red Clio")

kable(summary(redClio[, c(4, 7)]))

#' What if, we compare them with the purchasers of red Megane ? do we obtain the same profile ?
redMegane <- sales[Model == "Megane" & Color =="red", ]

#' Meltin data
redMeganeMelt <- melt(redMegane[, c(1, 2, 5, 9)], id.vars = 'transID')
freq <- redMeganeMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "Buyers of the red Megane")

kable(summary(redMegane[, c(4, 7)]))

#' Who are the consumers that obtain the best price offer ? 
bestPrice <- setorder(sales, Model, Price)
bestPrice[, mean := mean(Price), by = Model]
bestPrice <- bestPrice[Price < mean, ]

#' Meltin data
bestPriceMelt <- melt(bestPrice[, c(1:3, 5, 8, 9)], id.vars = c('transID', 'Model'))
freq <- bestPriceMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
#' Barplot
freq$label = paste0(sprintf("%.0f", freq$percent), "%")
ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 2, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582", "#FF9999")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "consumers that obtain the best price offer")

statbestPrice <- bestPrice[, list(total = .N, revenue = sum(Price), age = mean(Age), 
                                  mean = mean(Price), incomes = median(Incomes))]

kable(statbestPrice)

#' What if we look by model 

bestPriceMelt <- melt(bestPrice[, c(1:3, 5, 8, 9)], id.vars = c('transID', 'Model'))
freq <- bestPriceMelt[, .N, by = c("Model", "variable", "value")]
freq[, percent := (N/sum(N))*100, by = c("Model", "variable")]
freq$label = paste0(sprintf("%.0f", freq$percent), "%")

allScales <- c("18-25" = "#56B4E9", "26-35" = "#56B4E9", "36-45" = "#56B4E9", 
               "46-55" = "#56B4E9", "56-65" = "#56B4E9", "66+"= "#56B4E9",
               "black" = "#FF0033", "blue" = "#FF0033", "green" = "#FF0033", "red" = "#FF0033", "yellow" = "#FF0033",
               "Couple" = "#FFCC00", "Men" = "#FFCC00", "Women" = "#FFCC00",
               "week" = "#CCCCCC", "weekend" = "#CCCCCC")

allNames <- c("Age groups", "Color", "Gender", "Date") 

ggplot(data = freq, aes(x = value, y = percent))+
  geom_bar(aes(fill = factor(value)), stat = "identity", position = position_dodge(0.9))+
  facet_grid(Model~variable, scales = "free_x") +
  scale_fill_manual(values = allScales, name = allNames,
                    guide = guide_legend(override.aes = list(alpha = 0.4))) +
  theme(legend.position="none") 

statbestPriceModel <- bestPrice[, list(total = .N, revenue = sum(Price), age = round(mean(Age)), 
                                  mean = mean(Price), incomes = median(Incomes)), by = "Model"]
kable(statbestPriceModel)

#' What is the profile of the women who bought a car in 2016 ?
womenProfile <- sales[Sex == "Women", ]
womenProfileMelt <- melt(womenProfile[, c(1:3, 8, 9)], id.vars = 'transID')
freq <- womenProfileMelt[, .N, by = c("variable", "value")]
freq[, percent := (N/sum(N))*100, by = "variable"]
freq$label = paste0(sprintf("%.0f", freq$percent), "%")

ggplot(data = freq, mapping = aes(x = factor(value), y = percent, fill = variable)) +
  geom_bar(stat = "identity", fill = '#56B4E9') +
  facet_wrap(~variable, nrow = 2, scales = "free_x") +
  scale_fill_manual(values = c("#56B4E9","#D1E5F0", "#F4A582", "#FF9999")) +
  geom_text(aes(label = label), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(x = "", fill = "consumers that obtain the best price offer")

statwomenProfile <- womenProfile[, list(total = .N, revenue = sum(Price), mean = mean(Price), age = mean(Age),
                                        incomes = median(Incomes))]

kable(statwomenProfile)