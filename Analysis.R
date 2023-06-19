

# Importing Packages ------------------------------------------------------

library(pacman)
p_load(ggplot2, dplyr, lubridate, plotly, stringi, stringr, readxl, caret, recipes)


# Importing data ----------------------------------------------------------

counties <- read_excel("data/POPULATIONCOUNTIES.xlsx")
head(counties)

imgs <- imgs <- list(
  "www/F.jpg",
  "www/kenya2.png",
  "www/kenya6.png",
  "www/kenya.png",
  "www/wild.png",
  "www/g.jpg",
  "www/hipo.jpg",
  "www/ele.jpg"
)


# Plotting ----------------------------------------------------------------

# 1. Pop Per County
p1 <- counties |>
  ggplot(aes(x = County)) + 
  geom_col(aes(y = Total) ,fill = "royalblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .6), axis.ticks = element_line()) +
  labs(title = "Kenya Population by Counties") +
  xlab("County") +
  ylab("Head Count")
p1 <- ggplotly(p1)

# 2.  Kenya Population since 1959
population <- read_excel("data/PopGrowthRate.xlsx")

p2 <- population |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = Population), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Kenya Population Since 1950") +
  xlab("Year") +
  ylab("Head Count")
p2 <- ggplotly(p2)

# 3. Kenya Population Growth
population <- read_excel("data/PopGrowthRate.xlsx")

p3 <- population |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Growth Rate`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Kenya Population Since 1950") +
  xlab("Year") +
  ylab("Growth Rate in %")
p3 <- ggplotly(p3)


# 4. Kenya fertility rate
Fertility <- read_excel("data/Fertility Rate.xlsx")

p4 <- Fertility |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Fertility Rate`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Kenya Ferility Rate Since 1950") +
  xlab("Year") +
  ylab("Ferility Rate in %")
p4 <- ggplotly(p4)


# 5. Kenya Mortality Ratep5 
Mortality <- read_excel("data/Mortality Rate.xlsx")
p5<- Mortality |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Death Rate`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Kenya Mortality Rate Since 1950") +
  xlab("Year") +
  ylab("Mortality Rate in %")
p5 <- ggplotly(p5)
Mortality <- read_excel("data/Mortality Rate.xlsx")

# 6. Kenya Life expectancy
Expectancy <- read_excel("data/Life Expectancy.xlsx")

p6 <- Expectancy |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Life Expectancy`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Life Expectnacy Since 1950") +
  xlab("Year") +
  ylab("Life Expectancy in %")
p6 <- ggplotly(p6)

# 7. Kenya Gdp
Gdp <- read_excel("data/Gross Domestic Product.xlsx")

p7 <- Gdp |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `GDP(Billions)`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "GDP(Billions) Since 1960") +
  xlab("Year") +
  ylab("GDP(Billions)")
p7 <- ggplotly(p7)

# 8. Kenya Per Capita Income
Gdp <- read_excel("data/Gross Domestic Product.xlsx")

p8 <- Gdp |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Per Capita`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Per Capita income Since 1960") +
  xlab("Year") +
  ylab("Per Capita")
p8 <- ggplotly(p8)

# 9. Kenya Infant Moratility Rate
infantM <- read_excel("data/Infant Mortality Rate.xlsx")

p9 <- infantM |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Infant Mortality Rate`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Infant Mortality Since 1950") +
  xlab("Year") +
  ylab("Infant Mortality")
p9 <- ggplotly(p9)

# 10. Kenya Unemployement
Unemployment <- read_excel("data/Unemployment Rate.xlsx")

p10 <- Unemployment |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Unemployment Rate (%)`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Unemployment Rate Since 1991") +
  xlab("Year") +
  ylab("Unemployment Rate in %")
p10 <- ggplotly(p10)

# 11. Kenya Eductaion Spending
Education <- read_excel("data/Education Spending.xlsx")

p11 <- Education |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Education Spending (% of GDP)`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "Education Spending (% of GDP) Since 1982") +
  xlab("Year") +
  ylab("Spending")
p11 <- ggplotly(p11)

# 12. Kenya c02 Emmision
Co2 <- read_excel("data/Co2.xlsx")

p12 <- Co2 |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `Kilotons of Co2`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "CO2 Emmision(in Kilotons) In Kenya Since 1960") +
  xlab("Year") +
  ylab("CO2")
p12 <- ggplotly(p12)

# 13. Kenya Electricity Access
Electricity <- read_excel("data/Electricity Access.xlsx")

p13 <- Electricity |>
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = `% of Population`), col = "royalblue", size = .7) +
  theme_minimal() +
  labs(title = "% of Population with Electricity Access In Kenya Since 1993") +
  xlab("Year") +
  ylab("CO2")
p13 <- ggplotly(p13)


# Relationship Between Various Population Factors -------------------------

# 1. Population Growth and Fertility Rate

  
  
  
  
  


