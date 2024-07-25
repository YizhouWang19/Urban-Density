
# CORRELATION BETWEEN URBAN AND GDP

# Install necessary packages
install.packages("summarySE")

# Package Library
library(ggplot2)
library(dplyr)
library(agricolae)
library(cowplot)
library(Rmisc)
library(tidyr)
library(cowplot)
library(ggpubr)
library(readxl)
library(summarySE)
# ==============================================================================
# prep the data
# 获取当前工作目录
getwd()

# 设置工作目录为指定路径
setwd("你的路径/文件夹")



# isolate by region
# different organizaiton have different region defination, ask that 
CompiledData <- read_excel("D:/WYZ/Documents/04实习/Mui Ho slides/density data/Density_Rplots/CompiledData.xlsx")

EastAsiaData <- dplyr::filter(CompiledData, Region == "East Asia")
EuropeData <- dplyr::filter(CompiledData, Region == "Europe")
SouthandCentralAsiaData <- dplyr::filter(CompiledData, Region == "South and Central Asia")
SouthEastAsiaData <- dplyr::filter(CompiledData, Region == "Southest Asia")
SubSaharanAfricaData <- dplyr::filter(CompiledData, Region == "Sub-Saharan Africa")
NorthAmericaData <- dplyr::filter(CompiledData, Region == "North America")
WesternAsiaandNorthAfricaData<- dplyr::filter(CompiledData, Region == "Western Asia and North Africa")
LatinAmericaandtheCaribbeanData<- dplyr::filter(CompiledData, Region == "Latin America and the Caribbean")
PacificData<- dplyr::filter(CompiledData, Region == "The Pacific")

# summarize the standard deviation, standard error of the mean, and a (default 95%) confidence interval of regions
EastAsiaData1.0 <- summarySE(EastAsiaData, measurevar = "Year2015", groupvars = c("Country"))
EuropeData1.0 <- summarySE(EuropeData, measurevar = "Year2015", groupvars = c("Country"))


EastAsiaData1.0
EuropeData1.0



# summarize the standard deviation, standard error of the mean, and a (default 95%) confidence interval of the full and cleaned data set
CompiledData2.0 <- summarySE(CompiledData, measurevar = "Year1990", groupvars = c("Region"))
CompiledData2.0


# clean the modified data (remove missing data)



# Construct boxplots of the data



# construct boxplots of the data, add a unit in the y axis 
Density1 <- ggplot(CompiledData, aes(x = Region, y = Year1990, color = Region)) + geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange" , "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue","North America" = "brown","The Pacific" = "purple" )) +
  theme_bw() +
  ggtitle("1990") +
  labs(x = "", y = "Urban Density, (persons / ha)") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density1


# construct boxplots of the data
Density2 <- ggplot(CompiledData, aes(x = Region, y = Year2000, color = Region)) + geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange" , "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue","North America" = "brown","The Pacific" = "purple" )) +
  theme_bw() +
  ggtitle("2000") +
  labs(x = "", y = "Urban Density") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density2



# construct boxplots of the data
Density3 <- ggplot(CompiledData, aes(x = Region, y = Year2015, color = Region)) + geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange" , "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue","North America" = "brown","The Pacific" = "purple" )) +
  theme_bw() +
  ggtitle("2015") +
  labs(x = "", y = "Urban Density") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))
Density3

Density3 <- ggplot(CompiledData, aes(x = Region, y = Year2015, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  geom_text(aes(label = City), 
            position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
            vjust = -0.5,  # Adjust vertical position of labels
            size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("2015") +
  labs(x = "", y = "Urban Density") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density3

Density3 <- ggplot(CompiledData, aes(x = Region, y = Year2015, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  geom_text(aes(label = City), 
            position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
            hjust = 0.5,  # Adjust horizontal justification of labels
            vjust = -0.5,  # Adjust vertical justification of labels
            size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("2015") +
  labs(x = "", y = "Urban Density") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density3


Density4 <- ggplot(CompiledData, aes(x = Region, y = ChangeRate2, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  geom_text(aes(label = City), 
            position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
            hjust = 0.5,  # Adjust horizontal justification of labels
            vjust = -0.5,  # Adjust vertical justification of labels
            size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Urban Density Change Rate: 1990-2000") +
  labs(x = "", y = "Urban Density Change Rate") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density4

Density5 <- ggplot(CompiledData, aes(x = Region, y = ChangeRate3, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  geom_text(aes(label = City), 
            position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
            hjust = 0.5,  # Adjust horizontal justification of labels
            vjust = -0.5,  # Adjust vertical justification of labels
            size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Urban Density Change Rate: 2000-2015") +
  labs(x = "", y = "Urban Density Change Rate") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density5

Density6 <- ggplot(CompiledData, aes(x = Region, y = Changerate4, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  #geom_text(aes(label = City), 
          #  position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
           # hjust = 0.5,  # Adjust horizontal justification of labels
            #vjust = -0.5,  # Adjust vertical justification of labels
            # size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Urban Density Change Rate: 1990-2015") +
  labs(x = "", y = "Urban Density Change Rate") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Density6


plot_grid(Density1, Density2, Density3, align = "hv", ncol = 3, axis = 'lr')

ggarrange(Density1, Density2, Density3, ncol = 3, nrow = 3, common.legend = TRUE, legend = "bottom")




# construct bargraph of the EastAsia Data
EastAsiaData_bar <- ggplot(EastAsiaData1.0, aes(x = Country, y = Year2015, fill = Country)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(Japan = "skyblue4")) + 
  coord_flip() +
  theme_bw() +
  ggtitle("2015") +
  theme(legend.position = "none") +
  ylab("Density") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))


EastAsiaData_bar


# construct bargraph of the EastAsia Data
EuropeData_bar <- ggplot(EuropeData1.0, aes(x = Country, y = Year2015, fill = Country)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(Ukraine = "skyblue4", Poland = "skyblue4", Lithuania = "skyblue4", Latvia = "skyblue4", Hungary = "skyblue4", Estonia = "skyblue4", Bulgaria = "skyblue4", Belarus = "skyblue4")) + 
  coord_flip() +
  theme_bw() +
  ggtitle("2015") +
  theme(legend.position = "none") +
  ylab("% Urban Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))


EuropeData_bar


plot_grid(EastAsiaData_bar, EuropeData_bar, align = "hv", ncol = 2, axis = "lr")



# construct bargraph of the EastAsia Data
EastAsiaData_bar <- ggplot(EastAsiaData1.0, aes(x = Country, y = Year2015)) + 
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  theme_bw()
  

EastAsiaData_bar

# construct density growth rate data
EastAsiaData3.0 <- summarySE(EastAsiaData, measurevar = "ChangeRate", groupvars = c("Country"))
EuropeData3.0 <- summarySE(EuropeData, measurevar = "ChangeRate", groupvars = c("Country"))
SouthAsiaData3.0 <- summarySE(SouthandCentralAsiaData, measurevar = "ChangeRate", groupvars = c("Country"))
SouthEastAsiaData3.0 <- summarySE(SouthEastAsiaData, measurevar = "ChangeRate", groupvars = c("Country"))
SubsaharanAfricaData3.0 <- summarySE(SubsaharanAfricaData, measurevar = "ChangeRate", groupvars = c("Country"))
NorthAmericaData3.0 <- summarySE(NorthAmericaData, measurevar = "ChangeRate", groupvars = c("Country"))
WesternAsiaandNorthAfricaData3.0 <- summarySE(WesternAsiaandNorthAfricaData, measurevar = "ChangeRate", groupvars = c("Country"))
LatinAmericaanatheCaribbeanData3.0 <- summarySE(LatinAmericaanatheCaribbeanData, measurevar = "ChangeRate", groupvars = c("Country"))
PacificData3.0 <- summarySE(PacificData, measurevar = "ChangeRate", groupvars = c("Country"))

# construct bargraph of the EastAsia Data
EastAsiaData_bar2 <- ggplot(EastAsiaData3.0, aes(x = Country, y = ChangeRate, fill = Country)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(Japan = "skyblue4")) + 
  coord_flip() +
  theme_bw() +
  ggtitle("2010-2015") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))


EastAsiaData_bar2


# construct bargraph of the EastAsia Data
EuropeData_bar2 <- ggplot(EuropeData3.0, aes(x = Country, y = ChangeRate, fill = Country)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(Ukraine = "skyblue4", Poland = "skyblue4", Lithuania = "skyblue4", Latvia = "skyblue4", Hungary = "skyblue4", Estonia = "skyblue4", Bulgaria = "skyblue4", Belarus = "skyblue4")) + 
  coord_flip() +
  theme_bw() +
  ggtitle("2010-2015") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))


EuropeData_bar2

# construct bargraph of the EastAsia Data
SubsaharandataData_bar2 <- ggplot(SubSaharanAfricaData, aes(x = City Name, y = ChangeRate, fill = City Name)) + 
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  theme_bw() +
  ggtitle("2010-2015") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))

SubsaharandataData_bar2 
avgdata <- read_excel("D:/WYZ/Documents/04实习/Mui Ho slides/density data/Density_Rplots/continentavgdata.xlsx")

FullData_bar2 <- ggplot(avgdata, aes(x = Region, y = ChangeRate1, fill = Region)) + 
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  theme_bw() +
  ggtitle("1990-2015") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))

FullData_bar2

FullData_bar3 <- ggplot(CompiledData, aes(x = Region, y = ChangeRate2, fill = Region)) + 
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  theme_bw() +
  ggtitle("1990-2010") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))

FullData_bar3


FullData_bar4 <- ggplot(CompiledData, aes(x = Region, y = ChangeRate3, fill = Region)) + 
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  theme_bw() +
  ggtitle("2010-2015") +
  theme(legend.position = "none") +
  ylab("Density Growth") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))

FullData_bar4



df<- CompiledData %>%
  select(Region, Year1990, Year2000, Year2015)

avg1 = aggregate(df$Year1990, by=list(type=df$Region),mean)
avg_data <- aggregate(cbind(Year1990, Year2000, Year2015) ~ Region, data = df, FUN = mean, na.rm = TRUE)

avg_data_long <- tidyr::gather(avg_data, Year, Value, -Region)

# Plot the line graph
Linegraph <- ggplot(avg_data_long, aes(x = Year, y = Value, color = Region, group = Region)) +
  geom_line() +
  labs(title = "Line Graph of Regions Over Years",
       x = "Year",
       y = "Average Density Value") +
  geom_line(size = 1.5)+
  scale_color_manual(values = c(
    "East Asia" = "skyblue4",
    "Europe" = "coral3",
    "Latin America and the Caribbean" = "orange",
    "South and Central Asia" = "chartreuse4",
    "Southeast Asia" = "grey",
    "Sub-Saharan Africa" = "violet",
    "Western Asia and North Africa" = "skyblue",
    "North America" = "brown",
    "The Pacific" = "purple"
  ))

Linegraph


# Reshape CompiledData to long format
long_data <- CompiledData %>%
  pivot_longer(cols = starts_with("Year"), names_to = "Year", values_to = "UrbanDensity")

# Merge reshaped CompiledData with avg_long
combined_data <- merge(long_data, avg_data_long, by = c("Region", "Year"))

# Calculate SE for each continent
se_data <- combined_data %>%
  group_by(Year, Region) %>%
  dplyr::summarise(
    MeanDensity = mean(UrbanDensity),
    SE = sd(UrbanDensity) / sqrt(n())
  )

# Plot the line graph with standard error ribbons
# Calculate SE for each continent
se_data <- combined_data %>%
  group_by(Year, Region) %>%
  summarise(
    MeanDensity = mean(UrbanDensity),
    SE = sd(UrbanDensity) / sqrt(n())
  )

# Plot the line graph with standard error ribbons
Linegraph <- ggplot(se_data, aes(x = Year, y = MeanDensity, color = Region, group = Region)) +
  geom_line() +
  geom_ribbon(aes(ymin = MeanDensity - SE, ymax = MeanDensity + SE, fill = Region), alpha = 0.3) +
  labs(title = "Line Graph of Regions Over Years with Standard Error",
       x = "Year",
       y = "Average Density Value") +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(
    "East Asia" = "skyblue4",
    "Europe" = "coral3",
    "Latin America and the Caribbean" = "orange",
    "South and Central Asia" = "chartreuse4",
    "Southeast Asia" = "grey",
    "Sub-Saharan Africa" = "violet",
    "Western Asia and North Africa" = "skyblue",
    "North America" = "brown",
    "The Pacific" = "purple"
  )) +
  scale_fill_manual(values = c(
    "East Asia" = "skyblue4",
    "Europe" = "coral3",
    "Latin America and the Caribbean" = "orange",
    "South and Central Asia" = "chartreuse4",
    "Southeast Asia" = "grey",
    "Sub-Saharan Africa" = "violet",
    "Western Asia and North Africa" = "skyblue",
    "North America" = "brown",
    "The Pacific" = "purple"
  ))

Linegraph



#population
pData <- read_excel("D:/WYZ/Documents/04实习/Mui Ho slides/density data/Density_Rplots/populationData .xlsx")

pdata2 <- join(pdata, select(CompiledData, City, Region), by = "City")
pdata3 <- pdata3 %>%
  select(-8)

Population1 <- ggplot(pdata3, aes(x = Region, y = change9010, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  geom_text(aes(label = City), 
    position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
   hjust = 0.5,  # Adjust horizontal justification of labels
  vjust = -0.5,  # Adjust vertical justification of labels
   size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Population Change Rate: 1990-2015") +
  labs(x = "", y = "Population Change Rate") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

Population1


#urban extent
UEdata <- read_excel("D:/WYZ/Documents/04实习/Mui Ho slides/density data/Density_Rplots/urbanextent.xlsx")

uedata2 <- join(UEdata, select(CompiledData, City, Region), by = "City")


UE1 <- ggplot(uedata2, aes(x = Region, y = UEChange9015, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
  #  geom_text(aes(label = City), 
           # position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
            #hjust = 0.5,  # Adjust horizontal justification of labels
            #vjust = -0.5,  # Adjust vertical justification of labels
            #size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Urban Extent Change Rate: 1990-2015") +
  labs(x = "", y = "Urban Extent Change Rate: 1990-2015 %") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

UE1

UE2 <- ggplot(uedata2, aes(x = Region, y = UE15, color = Region)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25)) +
 # geom_text(aes(label = City), 
 # position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.25),
  #hjust = 0.5,  # Adjust horizontal justification of labels
  #vjust = -0.5,  # Adjust vertical justification of labels
  #size = 4) +   # Adjust the size of labels
  scale_color_manual(values = c("East Asia" = "skyblue4", "Europe" = "coral3", "Latin America and the Caribbean" = "orange", "South and Central Asia" = "chartreuse4", "Southeast Asia" = "grey", "Sub-Saharan Africa" = "violet", "Western Asia and North Africa" = "skyblue", "North America" = "brown", "The Pacific" = "purple")) +
  theme_bw() +
  ggtitle("Urban Extent 2015 (ha)") +
  labs(x = "", y = "Urban Extent 2015 (ha)") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18))

UE2


install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)

# Plotting the correlation between urban extent change rate and density change rate
ggplot(UEdata, aes(x = UEChange9015, y = DensityChangeRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation Between Urban Extent Change Rate and Density Change Rate",
       x = "Urban Extent Change Rate",
       y = "Density Change Rate")


ggpairs(your_data, cardinality_threshold = 201)  # Adjust the threshold as needed
# Create scatterplot matrix using ggpairs
pair=ggpairs (UEdata[,  c ( "UEChange9015" ,  "DensityChangeRate","PopChangeRate" )],cardinality_threshold = 201)
pair
