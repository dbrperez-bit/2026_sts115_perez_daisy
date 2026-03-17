##Final Exam 
#Daisy Perez 

#Loading the main data
# Data biography note: travel_df is the main respondent-level survey dataset.
# Data biography note: dict_df is the data dictionary used to interpret variables.
travel_df <- read.csv("~/Desktop/2026_sts115_perez_daisy/finalexam/cts_share_data_student.csv")
dict_df <- read.csv("~/Desktop/2026_sts115_perez_daisy/finalexam/cts_data_dictionary_student.csv")

#Forensics 
#Look at the structure
str(travel_df)
head(travel_df)
names(travel_df)
ncol(travel_df)

# Statistic for critique: respondent count
nrow(travel_df)


# Missingness
colSums(is.na(travel_df))
sort(colSums(is.na(travel_df)), decreasing = TRUE)[1:20]

# Findings Travel Related Variables  
grep("role|mode|travel|commute|bike|walk|bus|transit|parking|distance|time", 
     names(travel_df), value = TRUE, ignore.case = TRUE)


# Most Common Variables 
grep("role_primary|usual_mode|mode_all|attitudes_travel|housing_problems|attitudes_bike",
     names(travel_df), value = TRUE)

# Statistic for critique: distribution of respondent role
table(travel_df[["role_primary"]], useNA = "ifany")

table(travel_df[["usual_mode...Selected.Choice"]], useNA = "ifany")

# Making Figures that would make logical sense to representfinidngs 
#load packages 
library(dplyr)
library(ggplot2)

# clean data for graph, becuase there was random variables like -99 and NA. 
role_plot_df <- travel_df |>
  mutate(role_primary = as.character(role_primary)) |>
  mutate(role_primary = trimws(role_primary)) |>
  filter(!is.na(role_primary)) |>
  filter(role_primary != "-99") |>
  filter(role_primary != "") |>
  count(role_primary, sort = TRUE)

role_plot_df
# Figure 1 used in critique: distribution of respondent role
fig_role <- ggplot(role_plot_df, aes(x = reorder(role_primary, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 1: Survey Respondents by Primary Role",
    x = "Primary role",
    y = "Count"
  ) +
  theme_minimal()

fig_role

# Figure 2: Usual commute mode, this filter to rows with actual counts and shorten the mode names for readibility in figure. 
mode_plot_df <- travel_df |>
  mutate(`usual_mode...Selected.Choice` = as.character(`usual_mode...Selected.Choice`)) |>
  mutate(`usual_mode...Selected.Choice` = trimws(`usual_mode...Selected.Choice`)) |>
  filter(!is.na(`usual_mode...Selected.Choice`)) |>
  filter(`usual_mode...Selected.Choice` != "-99") |>
  filter(`usual_mode...Selected.Choice` != "") |>
  filter(`usual_mode...Selected.Choice` != "NA") |>
  count(`usual_mode...Selected.Choice`, sort = TRUE) |>
  filter(n > 0) |>
  mutate(mode_short = case_when(
    `usual_mode...Selected.Choice` == "Drive alone in a car (or other vehicle)" ~ "Drive alone",
    `usual_mode...Selected.Choice` == "Bus and/or shuttle" ~ "Bus/shuttle",
    `usual_mode...Selected.Choice` == "Walk (or wheelchair)" ~ "Walk/wheelchair",
    `usual_mode...Selected.Choice` == "Stand up electric scooter (e-scooter)" ~ "E-scooter",
    `usual_mode...Selected.Choice` == "Carpool and/or vanpool with others going to campus" ~ "Carpool/vanpool",
    `usual_mode...Selected.Choice` == "Get dropped off by a friend or family (the driver continues on elsewhere)" ~ "Dropped off",
    `usual_mode...Selected.Choice` == "Train (Amtrak)" ~ "Train",
    `usual_mode...Selected.Choice` == "Electric-assist bike (e-bike)" ~ "E-bike",
    `usual_mode...Selected.Choice` == "Motorcycle or Vespa-like scooter" ~ "Motorcycle/Vespa",
    `usual_mode...Selected.Choice` == "Lyft, Uber, or other ride-hailing service" ~ "Ride-hailing",
    `usual_mode...Selected.Choice` == "Electric skateboard (e-skateboard)" ~ "E-skateboard",
    `usual_mode...Selected.Choice` == "Skates, conventional skateboard, or  kick scooter" ~ "Skates/skateboard",
    `usual_mode...Selected.Choice` == "Other:" ~ "Other",
    TRUE ~ `usual_mode...Selected.Choice`
  ))

mode_plot_df

fig_mode <- ggplot(mode_plot_df, aes(x = reorder(mode_short, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure:2 Usual Commute Mode",
    x = "Usual mode",
    y = "Count"
  ) +
  theme_minimal()

fig_mode

# Figure 3:  usual commute time 
time_plot_df <- travel_df |>
mutate(usual_time = as.character(usual_time)) |>
  mutate(usual_time = trimws(usual_time)) |>
  filter(!is.na(usual_time)) |>
  filter(usual_time != "-99") |>
  filter(usual_time != "") |>
  filter(usual_time != "NA") |>
  count(usual_time, sort = TRUE)

fig_time <- ggplot(time_plot_df, aes(x = reorder(usual_time, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure:3 Usual Commute Time",
    x = "Usual commute time",
    y = "Count"
  ) +
  theme_minimal()

fig_time

#Figure 4 Commute is Expensive 
cost_plot_df <- travel_df |>
  mutate(`attitudes_travel...My.commute.is.expensive.` =
           as.character(`attitudes_travel...My.commute.is.expensive.`)) |>
  mutate(`attitudes_travel...My.commute.is.expensive.` =
           trimws(`attitudes_travel...My.commute.is.expensive.`)) |>
  filter(!is.na(`attitudes_travel...My.commute.is.expensive.`)) |>
  filter(`attitudes_travel...My.commute.is.expensive.` != "-99") |>
  filter(`attitudes_travel...My.commute.is.expensive.` != "") |>
  filter(`attitudes_travel...My.commute.is.expensive.` != "NA") |>
  count(`attitudes_travel...My.commute.is.expensive.`, sort = TRUE)

fig_cost <- ggplot(cost_plot_df,
                   aes(x = reorder(`attitudes_travel...My.commute.is.expensive.`, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 4: Agreement That Commute Is Expensive",
    x = "Response",
    y = "Count"
  ) +
  theme_minimal()

fig_cost

savehistory(file = "~/Desktop/2026_sts115_perez_daisy/finalexam/docs/examhistory.Rhistory")




