# Cereal project
# 1.  Show do different manufacturers (mfr) compare in terms of average calories, protein, fat,
# and fiber in their cereals?
# 2.  Are there a correlation between the nutritional content (calories, protein, fat, fiber)
# and the rating of cereals?
# 3.  What is the distribution of cereal types (hot vs cold) in the dataset.
# 4.  What are the top 5 cereals based on their rating?
# 5.  Which cereals have the highest and lowest sodium content?
# 6.  Do cereals placed on different shelves (shelf 1, 2, 3) have different ratings?
# 7.  Is there a relationship between sugars and carbohydrates in cereals?
# 8.  How does the nutrient composition (calories, protein, fat, etc.) vary by serving size (weight)?
# 9.  Create histograms or box plots to visualize the distribution of nutrients (calories, protein, fat, etc.) 
# across all cereals.
# 10. Visualize the top-rated cereals based on user ratings.

# Solution

# Activate packages for data analysis, manipulation and visualization
library("dplyr")
library("ggplot2")
library("readxl")
library("forcats")
library("gridExtra")

# Import the 80 cereals data sets using the readxlsx function from readxl package

Cereals_80 <- readxl::read_xlsx("cereal.xlsx")
View(Cereals_80)

# Question 1
Average_Nutrients <- Cereals_80 %>% group_by(Manufacturer = Cereals_80$MFR) %>% summarise(Calory = round(mean(Calories),1),
                                                                           Proteins = round(mean(Protein),1),
                                                                           Fats = round(mean(Fat),1),
                                                                           Fibers = round(mean(Fiber),1))
View(Average_Nutrients)

# Visualization
Average_Calory <- ggplot(data = Average_Calory, mapping = aes(x = Calory,
                                                                 y = Manufacturer)) +
  geom_bar(stat = "identity",
           fill = "red",
           color = "navy", width = 0.8) + labs(title = "Average calory", x = "Manufacturer", y = "Calory") + theme_minimal()
# Since the plot is arranged in terms of manufacturer instead of bars, we say activate the forcat package and say:
Average_Calory <- Average_Nutrients %>% mutate(Manufacturer = fct_reorder(Manufacturer,Calory))
Average_calory <- ggplot(data = Average_Calory, mapping = aes(x = Calory,
                                                              y = Manufacturer)) +geom_bar(stat = "identity",
                                                                                           fill = "red",
                                                                                           color = "navy", 
                                                                                           width = 0.8) + 
  labs(title = "Average calory", x = "Calory", y = "Manufacturer") + theme_minimal()

# Protein
Average_Protein <- Average_Nutrients %>% mutate(Manufacturer = fct_reorder(Manufacturer,Proteins))
Average_protein <- ggplot(data = Average_Protein, mapping = aes(x = Proteins, y = Manufacturer)) + geom_bar(stat = "identity",
                                                                                         fill = "navy",
                                                                                         colour = "black") +
  labs(title = "Average protein", x = "Protein", y = "Manufacturer") + theme_minimal()

# Fats
Average_Fats <- Average_Nutrients %>%  mutate(Manufacturer = fct_reorder(Manufacturer, Fats))
Average_fat <- ggplot(data = Average_Fats, mapping = aes(x = Fats, y = Manufacturer)) + geom_bar(stat = "identity",
                                                                                                 fill = "black",
                                                                                                 color = "black",
                                                                                                 width = 0.5) +
  labs(title = "Average Fat", x = "Fat Distribution", y = "Manufacturer") + theme_minimal()

# fiber
Average_Fiber <- Average_Nutrients %>% mutate(Manufacturer = fct_reorder(Manufacturer,Fibers))
Average_fiber <- ggplot(data = Average_Fiber, mapping = aes(x = Fibers, y = Manufacturer)) + geom_bar(stat = "identity",
                                                                                                      fill = "green",
                                                                                                      color = "green",
                                                                                                      width = 0.5) +
  labs(title = "Average fiber", x = "Fibers", y = "Manufacturer") + theme_minimal()
library(gridExtra)
grid.arrange(Average_calory,Average_fat,Average_protein,Average_fiber, nrow = 2, ncol = 2)
install.packages("gridExtra")
# Question 2
# For calories
`Calories Correlations` <- cor(Cereals_80$Calories, Cereals_80$Ratings)
print( `Calories Correlations`)

# Hence the correlation between the ratings and calories is negative

# For protein
Protein_Correlation <- cor(Cereals_80$Protein, Cereals_80$Ratings)
print(Protein_Correlation)
# Hence there is a zero correlation between the proteins and the ratings of the cereals

# For fat
Fat_Correlation <- cor(Cereals_80$Fat, Cereals_80$Ratings)
print(Fat_Correlation)

# Hence there is a negative correlation between the fat and the ratings of the cereals

# For fiber
Fiber_Correlation <- cor(Cereals_80$Fiber, Cereals_80$Ratings)
print(Fiber_Correlation)

# Therefore, there is a positive correlation between fiber and ratings

# Question 3 
table(Cereals_80$Type)

# Hence, there are 77 cereal types, 74 are cold and 3 are hot

# Question 4
Top5_Cereal <- select(Cereals_80, Name, Ratings)
Top5_Cereal <- Top5_Cereal %>% group_by(Name) %>% arrange(desc(Ratings)) %>% head(5)
# Recall the fct_reorder function
Top5_Cereal <- Top5_Cereal %>% mutate(Name = fct_reorder(Name, Ratings))
View(Top5_Cereal)

# Visualization
ggplot(data = Top5_Cereal, mapping = aes(x = Name, y = Ratings)) + geom_bar(stat = "identity",
                                                                            fill = "darkgreen",
                                                                            color = "darkgreen",
                                                                            width = 0.8)
# Question 5
Lowest_Sodium <- select(Cereals_80, Name, Sodium)
View(Lowest_Sodium)
Lowest_Sodium <- Lowest_Sodium %>% group_by(Name) %>% arrange(Sodium) %>% head(1)

Highest_Sodium <- select(Cereals_80, Name, Sodium)
Highest_Sodium <- Highest_Sodium %>% group_by(Name) %>% arrange(desc(Sodium)) %>% head(1)
View(Highest_Sodium)

# Question 6
Ratings <- Cereals_80 %>% group_by(Shelf) %>% summarise(Total = mean(Ratings))
print(Ratings)

# Question 7
Sugar_vs_Carbohydrates <- cor(Cereals_80$Carbo, Cereals_80$Sugars)
print(Sugar_vs_Carbohydrates)

# Hence there is a negative correlation between Carbohydrate and sugar

# Question 8
cereals_long <- Cereals_80 %>% 
  pivot_longer(cols = c(Calories, Protein, Fat, Sodium, Fiber, Carbo, Sugars, Potass), 
               names_to = "nutrient", 
               values_to = "value")

# Visualizatio using scatterplot
ggplot(cereals_long, aes(x = Weight, y = value, color = nutrient)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ nutrient, scales = "free_y") +
  labs(title = "Variation of Nutrient Composition by Serving Size (Weight)",
       x = "Serving Size (Weight in ounces)",
       y = "Nutrient Value")

# Question 9
cereals_Distribution_viz <- Cereals_80 %>% 
  pivot_longer(cols = c(Calories, Protein, Fat, Sodium, Fiber, Carbo, Sugars, Potass), 
               names_to = "nutrient", 
               values_to = "value")

# Visuals using histogram
ggplot(data = cereals_Distribution_viz, aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  facet_wrap(~ nutrient, scales = "free_x") +
  labs(title = "Distribution of Nutrients Across All Cereals",
       x = "Nutrient Value",
       y = "Frequency")

# Question 10
Cereals_Visuals <- Cereals_80 %>% group_by(Name) %>% arrange(desc(Ratings))
View(Cereals_Visuals)
Cereals_Visuals <- select(Cereals_Visuals, Name, Ratings)
barplot(Cereals_Visuals$Ratings,
        names.arg = Cereals_Visuals$Name,
        main = "Top rated cereal",
        xlab = "Cereal name",
        ylab = "User ratings",
        col = "red",
        las = 2)
