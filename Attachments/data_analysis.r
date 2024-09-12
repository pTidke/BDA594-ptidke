library(ggplot2)
setwd("./Documents/coursework/BDA594/exercise2/assignment2/")

#Load Data
medical_cases = read.csv("medical_examiner_cases.csv")

#Filter Data
medical_data_2023 = medical_cases[medical_cases$Year == 2023,]

# Add helper column for aggregation count
medical_data_2023["Count"] = 1

# Add values fo blank cells
medical_data_2023$Manner.of.Death[medical_data_2023$Manner.of.Death == ""] = "Undetermined"

# Aggregate data based on death types and update columns names
type_count = aggregate(medical_data_2023$Count, list(medical_data_2023$Manner.of.Death), FUN=sum)
colnames(type_count) <- c("manner_of_death", "total_deaths")

# Plotting a Bar chart
ggplot(type_count,
    aes(manner_of_death,total_deaths)) +
    geom_bar(stat="identity",aes(fill= factor(manner_of_death)), width=0.5) +
    geom_text(aes(label = total_deaths))+
    ggtitle("Manner of Deaths in San Diego for FY2023")+
    ylab("Total number of Deaths") +
    xlab("Manner of Death") +
    theme(legend.position="right") +
    guides(fill = guide_legend(title = "Manner of Death"))+
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))

# Plotting a Pie Chart
ggplot(type_count, aes(x = "", y = total_deaths, fill = reorder(manner_of_death, total_deaths))) +
    geom_col(color = "black") +
    geom_text(aes(x = 1.6, label = total_deaths),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y")+
    guides(fill = guide_legend(title = "Manner of Death"))+
    ggtitle("Manner of Deaths in San Diego, CA") +
    ylab("Total Number of Deaths")

# Filter records whihc have cause of death due to Drug
drug_cases = medical_cases[grepl('Drug',medical_cases$Manner.Type),]

# Add helper columns for aggregation count
drug_cases["Count"] = 1
drug_cases$male = with(drug_cases, ifelse(drug_cases$Gender == "Male", 1, 0))
drug_cases$female = with(drug_cases, ifelse(drug_cases$Gender == "Female", 1, 0))

# Aggregate data based on year, total deaths, female deaths, male deaths and update columns names
drug_cases_across_years = aggregate(drug_cases[, c("Count", "male", "female")], list(drug_cases$Year), FUN=sum)
colnames(drug_cases_across_years) = c("year", "total_cases", "male_count", "female_count")

# Plotting a combined Bar + Line chart
pltx = ggplot(drug_cases_across_years) + # This is the dataframe which needs to be plotted
    geom_col(   # This is the bar plot
        aes(
            x = reorder(year, year),
            y = total_cases, 
            fill = total_cases 
        ),
        position = "dodge2",
        show.legend = TRUE,
        alpha = .9
    )+
    geom_point(     # This are the data points for female deaths
        aes(
            x = reorder(year, year), 
            y = female_count, 
        ),
        size = 2, 
        color = "white"
    )+
    geom_line(      # This is the lineplot for female deaths
        aes(
            x = reorder(year, year), 
            y = female_count, 
            group=1
        ),
        linewidth = 1, 
        color = "#0099f9",
        show.legend = TRUE
    )+
    geom_text(      # This are the labels for female death counts
        aes(x = reorder(year, year), 
            y = female_count, 
            label = female_count),
        nudge_x = 0,
        nudge_y = 30,
        size = 3,
        color = "white"
    )+
    geom_text(      # This are the labels for total death counts
        aes(x = reorder(year, year),
            y = total_cases,
            label = total_cases),
        nudge_x = 0,
        nudge_y = 30,
        size = 3,
    )+
    ggtitle("Trends in Drug Related Deaths (Total vs. Female) in San Diego, CA") +
    xlab("Total Number of Cases") + ylab("Year") +
    labs(subtitle = "[Line plot represents female deaths]")

pltx

# Aggregate data based on age groups and update columns names
age_group_count = aggregate(medical_data_2023$Count, list(medical_data_2023$Age.Group.Option.1), FUN=sum)
colnames(age_group_count) <- c("age_group", "total_deaths")

# Add values fo blank cells
age_group_count$age_group[age_group_count$age_group == ""] = "Unknown"

# Plotting a pie chart for the age group wise death distribution
ggplot(age_group_count, aes(x = age_group, y = total_deaths)) +
    geom_col(aes(fill = age_group), color = NA) +
    coord_polar()+
    guides(fill = guide_legend(title = "Number of Deaths"))+
    ggtitle("Total number of Deaths across age groups in San Diego, CA") +
    ylab("Total Number of Deaths") + xlab("Age Groups")


# Aggregate data based on age and update columns names
age_count = aggregate(medical_data_2023$Count, list(medical_data_2023$Age.in.Years), FUN=sum)
colnames(age_count) <- c("age", "total_deaths")

# Plotting density chart for deaths across various ages
ggplot(age_count, aes(age, total_deaths)) +
  geom_density_2d_filled(show.legend = TRUE) +
  coord_cartesian(expand = FALSE) +
  labs(x = "Age", y = "Total Deaths")