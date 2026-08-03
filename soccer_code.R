# Load required packages
library(readxl)

# Read in the data
Soccer_Shot_Stats <- read_excel("C:/Users/ASUS/OneDrive - onu.edu/Desktop/Soccer Statistics/Soccer Shot Stats.xlsx")

# Makes all variables upper case to avoid multiple variables stating the same thing
Soccer_Shot_Stats[] <- lapply(Soccer_Shot_Stats, function(x) if(is.character(x)) toupper(x) else x)

# Remove duplicated columns
Soccer_Shot_Stats <- Soccer_Shot_Stats[, !duplicated(names(Soccer_Shot_Stats))]

# Convert typeOfAssist to "other" if it contains certain values
Soccer_Shot_Stats$typeOfAssist <- ifelse(Soccer_Shot_Stats$typeOfAssist %in% c("CHEST", "FLICK UP", "HEADER"), "Other", Soccer_Shot_Stats$typeOfAssist)

# Makes goals variable to show actual goals scored
Soccer_Shot_Stats$goals <- ifelse(Soccer_Shot_Stats$Result == "GOAL", 1, 0)
# Groups shot types into on ground shots or shots in the air
Soccer_Shot_Stats$shotTypeGrouped <- ifelse(Soccer_Shot_Stats$shotType %in% c("HEADER", "LEFTVOLLEY", "RIGHTVOLLEY"), "InAir",
                                            ifelse(Soccer_Shot_Stats$shotType %in% c("LEFTFOOT", "RIGHTFOOT"), "OnGround", "Other"))
# Creates the expected goals category
Xg <- aggregate(goals ~ zone + shotTypeGrouped, data = Soccer_Shot_Stats,
                FUN = function(x) c(mean = mean(x)))
# Merges Xg data with original data
Soccer_Shot_Stats <- merge(Soccer_Shot_Stats, Xg, by = c("zone", "shotTypeGrouped"), all.x = TRUE)

# Replace xG column with goals.y.mean
Soccer_Shot_Stats$xG <- Soccer_Shot_Stats$goals.y.mean

# Replace xG for penalties with average conversion rate
Soccer_Shot_Stats$goals.y <- ifelse(Soccer_Shot_Stats$typeOfAttack == "PENALTY", 0.75, Soccer_Shot_Stats$goals.y)

# Remove unnecessary columns
Soccer_Shot_Stats <- Soccer_Shot_Stats[, !(names(Soccer_Shot_Stats) %in% c("shotTypeGrouped", "goals.y.mean"))]

# Aggregate goals.x by player
player_stats_goals <- aggregate(goals.x ~ player, data = Soccer_Shot_Stats, FUN = sum)
# Aggregate goals.y by player
player_stats_xG <- aggregate(goals.y ~ player, data = Soccer_Shot_Stats, FUN = sum)
# Merge the two aggregated data frames by player
player_stats_merged <- merge(player_stats_goals, player_stats_xG, by = "player", all = TRUE)
# Calculate the difference between actual goals and xG
player_stats_merged$Difference <- player_stats_merged$goals.x - player_stats_merged$goals.y
# Rename columns for clarity
colnames(player_stats_merged) <- c("PLAYER", "Actual_Goals", "xG", "Difference")
# Display the table
print(player_stats_merged)

# Copy the data to avoid modifying the original data frame
Soccer_Shot_Stats_modified <- Soccer_Shot_Stats
# Replace specific values in the typeOfAttack column with "other"
Soccer_Shot_Stats_modified$typeOfAttack[Soccer_Shot_Stats_modified$typeOfAttack %in% c("DEFLECTED PASS", "POOR CLEARANCE", "DEFLECTED SHOT")] <- "OTHER"
# Remove penalty from the typeOfAttack column
Soccer_Shot_Stats_modified <- Soccer_Shot_Stats_modified[Soccer_Shot_Stats_modified$typeOfAttack != "PENALTY", ]

#Seeing if the type of assist influences actual goals

# Convert "N/A" values to actual NA (missing values)
Soccer_Shot_Stats$typeOfAssist[Soccer_Shot_Stats$typeOfAssist == "N/A"] <- NA
model <- aov(Soccer_Shot_Stats$goals.x ~ Soccer_Shot_Stats$typeOfAssist, data = Soccer_Shot_Stats)
summary(model)
tukey.model <- TukeyHSD(model)
print(tukey.model)


#Seeing if side of attack influences actual goals

Soccer_Shot_Stats$sideOfAttackGrouped <- ifelse(Soccer_Shot_Stats$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", Soccer_Shot_Stats$sideOfAttack)

model_2 <- aov(Soccer_Shot_Stats$goals.x ~ Soccer_Shot_Stats$sideOfAttackGrouped, data = Soccer_Shot_Stats)
summary(model_2)
tukey.model_2 <- TukeyHSD(model_2)
print(tukey.model_2)

#Seeing if where possession was won influences actual goals

# Subset the data to exclude observations with typeOfAttack equal to "restart", "throw-in", or "penalty"
filtered_data <- subset(Soccer_Shot_Stats, !(typeOfAttack %in% c("RESTART", "THROW-IN", "PENALTY")))

model_3 <- aov(filtered_data$goals.x ~ filtered_data$PossesionWon, data = filtered_data)
summary(model_3)
tukey.model_3 <- TukeyHSD(model_3)
print(tukey.model_3)


#Seeing if the type of attack influences actual goals

model_4 <- aov(Soccer_Shot_Stats_modified$goals.x ~ Soccer_Shot_Stats_modified$typeOfAttack, data = Soccer_Shot_Stats_modified)
summary(model_4)
tukey.model_4 <- TukeyHSD(model_4)
print(tukey.model_4)

#Seeing if the type of assist influences expected goals

model_5 <- aov(Soccer_Shot_Stats$goals.y ~ Soccer_Shot_Stats$typeOfAssist, data = Soccer_Shot_Stats)
summary(model_5)
tukey.model_5 <- TukeyHSD(model_5)
print(tukey.model_5)


#Seeing if side of attack influences expected goals

Soccer_Shot_Stats$sideOfAttackGrouped <- ifelse(Soccer_Shot_Stats$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", Soccer_Shot_Stats$sideOfAttack)

model_6 <- aov(Soccer_Shot_Stats$goals.y ~ Soccer_Shot_Stats$sideOfAttackGrouped, data = Soccer_Shot_Stats)
summary(model_6)
tukey.model_6 <- TukeyHSD(model_6)
print(tukey.model_6)

#Seeing if where possession was won influences expected goals

model_7 <- aov(filtered_data$goals.y ~ filtered_data$PossesionWon, data = filtered_data)
summary(model_7)
tukey.model_7 <- TukeyHSD(model_7)
print(tukey.model_7)

#Seeing if the type of attack influences expected goals

model_8 <- aov(Soccer_Shot_Stats_modified$goals.y ~ Soccer_Shot_Stats_modified$typeOfAttack, data = Soccer_Shot_Stats_modified)
summary(model_8)
tukey.model_8 <- TukeyHSD(model_8)
print(tukey.model_8)

#frequency tables
table(Soccer_Shot_Stats$sideOfAttack)
table(Soccer_Shot_Stats$typeOfAttack)
table(Soccer_Shot_Stats$shotType)

## Creating a Pie Chart for the Type Of Attack ##

# Calculate frequencies of each category
attack_freq <- table(Soccer_Shot_Stats_modified$typeOfAttack)
# Calculate percentages
attack_percent <- round(prop.table(attack_freq) * 100, 1)
# Order categories by percentages
sorted_index <- order(attack_percent, decreasing = TRUE)
attack_freq <- attack_freq[sorted_index]
attack_percent <- attack_percent[sorted_index]
# Create pie chart with percentages
pie(attack_freq, main = "Type of Attack",
    labels = paste(names(attack_freq), "\n", attack_percent, "%"),
    col = rainbow(length(attack_freq)),
    cex = 0.7, # Adjust label size
    xpd = TRUE) # Allow labels to be drawn outside the plot area

## T-test to determine if dom vs non-dom foot changes goals scored ##

# Subset the data to include only non-null values in domVSnondom
subset_data <- Soccer_Shot_Stats[!is.na(Soccer_Shot_Stats$domVSnondom) & Soccer_Shot_Stats$domVSnondom != "*"
                                 & Soccer_Shot_Stats$domVSnondom != "?", ]
result <- t.test(subset_data$goals.x ~ subset_data$domVSnondom)
print(result)
table(Soccer_Shot_Stats$domVSnondom)

#creates the csv file that can be used in the Tableau
write.csv(Soccer_Shot_Stats, "C:/Users/ASUS/OneDrive - onu.edu/Desktop/Soccer Statistics/XGStats.csv")

