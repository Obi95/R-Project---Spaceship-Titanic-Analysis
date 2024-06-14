library(dplyr)
library(ggplot2)

# Specify the path to your CSV file
our_path <- "c:/Users/Sushi/Downloads/Capstone II/train.csv"

# Read the CSV file into a dataframe
df <- read.csv(our_path)

# Clean the data
df_cleaned <- df %>%
  mutate(
    CryoSleep = trimws(tolower(CryoSleep)),
    VIP = tolower(as.character(VIP)),
    Age = as.numeric(Age)
  )

# Total passengers
total_passengers <- nrow(df)
cat("\nTotal passengers:", total_passengers, "\n")

# Count the number of VIP and non-VIP passengers
vip_counts <- df_cleaned %>%
  group_by(VIP) %>%
  summarise(count = n())
cat("\nVIP Counts:\n")
print(vip_counts)

# Find the number of unique planets in the HomePlanet column
unique_planets <- df_cleaned %>%
  summarise(unique_planets = n_distinct(HomePlanet))
cat("\nNumber of unique planets:", unique_planets$unique_planets, "\n")
unique_home_planets <- df_cleaned %>%
  distinct(HomePlanet) %>%
  arrange(HomePlanet)
cat("\nUnique HomePlanets:\n")
print(unique_home_planets)

# Filter for VIP passengers who are in cryosleep
vip_cryosleep <- df_cleaned %>%
  filter(VIP == "true" & CryoSleep == "true")
total_vip_cryosleep <- nrow(vip_cryosleep)
cat("\nTotal VIP passengers in cryosleep:", total_vip_cryosleep, "\n")

# Total passengers per destination
destination_counts <- df_cleaned %>% 
  count(Destination) %>%
  arrange(desc(n))
cat("\n Total passengers per Destination:\n")
print(destination_counts)

# Average age of the passengers
average_age <- df_cleaned %>%
  filter(!is.na(Age)) %>%
  summarise(avg_age = mean(Age)) %>%
  pull(avg_age)
cat("\nThe average age of passengers is:", average_age, "\n")

# Filter passengers who are over the age of 65
passengers_over_65 <- df_cleaned %>%
  filter(Age > 65)
total_passengers_over_65 <- nrow(passengers_over_65)
cat("\nTotal passengers over the age of 65:", total_passengers_over_65, "\n")

# I wonder if most of our passengers are below their 30s because years might pass
# on cryosleep we do not know how long it last, maybe it interstellar space travel
# of this kind who needs crypsleep advices the passenger to travel as young as they can
# perphaps faster lightspeed travel isnt invented yet. 


# How many passengers over the age of 65 are in CryoSleep?
passengers_over_65_cryosleep <- df_cleaned %>%
  filter(Age > 65 & CryoSleep == "true")
total_passengers_over_65_cryosleep <- nrow(passengers_over_65_cryosleep)
cat("\nTotal passengers over the age of 65 in cryosleep:", total_passengers_over_65_cryosleep, "\n")


# as expected it is curious to see there are only 84 people over the age of 65
# out of 8.693 passengers. Maybe for this type of travel, youth/time/years are
# consumed. It could be that senior people are afraid of spacetravel too.
# Althought we can see there are 28 senior people over the age of 65 that are in CryoSleep.
# we Cannot erase the posibility of prices having an influence in the passenger's decision
# To choose CryoSleep for their trip. 


vip_passengers <- df_cleaned %>%
  filter(VIP == "true" & !is.na(HomePlanet))

# Count VIP passengers per HomePlanet
vip_homeplanet_counts <- vip_passengers %>%
  group_by(HomePlanet) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

most_common_homeplanet <- vip_homeplanet_counts$HomePlanet[1]
most_common_count <- vip_homeplanet_counts$count[1]

cat("HomePlanet with the most VIP passengers:", most_common_homeplanet, "\n")
cat("Number of VIP passengers from this HomePlanet:", most_common_count, "\n")

# Visualization: Bar Chart of VIP passengers per HomePlanet
ggplot(vip_homeplanet_counts, aes(x = HomePlanet, y = count, fill = HomePlanet)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of VIP Passengers by HomePlanet",
       x = "HomePlanet",
       y = "Count")

# Bar Chart for VIP and Non-VIP Counts
ggplot(vip_counts, aes(x = VIP, y = count, fill = VIP)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of VIP and Non-VIP Passengers",
       x = "VIP Status",
       y = "Count")
# In the Bar Chart we see a significant difference for VIP and Non VIP members in our
# Tripulation. We can see above 8000 passengers are NonVIP and less than 500 are VIP
# We know the exact number of VIP Holders are 199 passengers. 


# Bar Chart for Total Passengers per Destination
ggplot(destination_counts, aes(x = Destination, y = n, fill = Destination)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Passengers per Destination",
       x = "Destination",
       y = "Count")
# As shown we can see Most of our passengers come from TRAPPIST-1e



# In this Box Plot we can see the average age being around the 20's close to the 30's
# Which of course we know thru our findings that this average is 28old. 
# Ww have a few outliers which indicate a few individuals are only aboves the age of 65-70
# out of all our passengers. 


# Bar Chart for Passengers in CryoSleep (VIP vs Non-VIP)
cryosleep_counts <- df_cleaned %>%
  mutate(CryoSleep = ifelse(CryoSleep == "true", "In CryoSleep", "Not in CryoSleep")) %>%
  group_by(VIP, CryoSleep) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(cryosleep_counts, aes(x = CryoSleep, y = count, fill = VIP)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Passengers in CryoSleep: VIP vs Non-VIP",
       x = "CryoSleep Status",
       y = "Count")

# Display the plots
print(ggplot2::last_plot())

# Adding to the Observations through this code. There are multiple indications that show
# People who are able to afford VIP Tickets in this trip are very wealthy individuals.
# Only a handful individuals were part of this VIP group out of +8000 passengers. We
# can see most of these individuals are from "Europa" and very few from "Mars"

