library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)


# Import Trackman Data Excel File
TrackmanData <- read_excel("TrackmanData.xlsx")

# Calculate wOBA for Each Batter
weights <- c(Walk = 0.72, HBP = 0.72, Single = 0.89, Double = 1.27, Triple = 1.62, HomeRun = 2.10)

wOBA <- TrackmanData %>%
  group_by(Batter) %>%
  summarise(
    AB = sum(PlayResult %in% c("Out", "Single", "Double", "Triple", "HomeRun", "FieldersChoice", "Strikeout")),
    BB = sum(PlayResult == "Walk"),
    IBB = sum(PlayResult == "IntentionalWalk"),
    SF = sum(PlayResult == "SacFly"),
    HBP = sum(PlayResult == "HBP"),
    `1B` = sum(PlayResult == "Single"),
    `2B` = sum(PlayResult == "Double"),
    `3B` = sum(PlayResult == "Triple"),
    HR = sum(PlayResult == "HomeRun")
  ) %>%
  mutate(
    wOBA = (weights["Walk"] * BB + weights["HBP"] * HBP + weights["Single"] * `1B` + 
              weights["Double"] * `2B` + weights["Triple"] * `3B` + weights["HomeRun"] * HR) / 
      (AB + BB - IBB + SF + HBP)
  )

# Calculate Average EV for Each Batter
AvgEV_by_Batter <- TrackmanData %>%
  filter(PitchCall == "InPlay") %>%       
  group_by(Batter) %>%       
  summarize(AvgEV = mean(ExitSpeed, na.rm = TRUE))

# Calculate Barrel % for Each Batter (95 MPH EV, b/w 10 and 35 LA)
BBE <- TrackmanData %>%
  filter(PitchCall == "InPlay")

BarrelPct_by_Batter <- BBE %>%
  group_by(Batter) %>%  
  summarize(
    BarrelPct = (sum(ExitSpeed > 95 & Angle >= 10 & Angle <= 35, na.rm = TRUE) / n()) * 100  # Barrel percentage
  )

# Calculate 95+ EV % for Each Batter
EV95 <- TrackmanData %>%
  filter(PitchCall == "InPlay")

EV95_by_Batter <- EV95 %>%
  group_by(Batter) %>%
  summarize(
    EV95Pct = (sum(ExitSpeed > 95, na.rm = TRUE) / n()) * 100
  )

# Calculate LA b/w 10 and 30 Degrees %
LA10_30 <- TrackmanData %>%
  filter(PitchCall == "InPlay")

LA10_30_by_Batter <- LA10_30 %>%
  group_by(Batter) %>%
  summarize(
    LA10_30Pct = (sum(Angle >= 10 & Angle <= 30, na.rm = TRUE) / n()) * 100
  )

# Combine all metrics into a single data frame
performance_data <- wOBA %>%
  inner_join(AvgEV_by_Batter, by = "Batter") %>%
  inner_join(BarrelPct_by_Batter, by = "Batter") %>%
  inner_join(EV95_by_Batter, by = "Batter") %>%
  inner_join(LA10_30_by_Batter, by = "Batter")

# Rank the batters for each metric within the same game
performance_data <- performance_data %>%
  mutate(
    wOBA_Rank = rank(-wOBA, ties.method = "min"),
    AvgEV_Rank = rank(-AvgEV, ties.method = "min"),
    BarrelPct_Rank = rank(-BarrelPct, ties.method = "min"),
    EV95Pct_Rank = rank(-EV95Pct, ties.method = "min"),
    LA10_30Pct_Rank = rank(-LA10_30Pct, ties.method = "min")
  )

# Normalize ranks to a 0-50 scale
performance_data <- performance_data %>%
  mutate(
    wOBA_Score = (max(wOBA_Rank) - wOBA_Rank + 1) / max(wOBA_Rank) * 50,
    AvgEV_Score = (max(AvgEV_Rank) - AvgEV_Rank + 1) / max(AvgEV_Rank) * 50,
    BarrelPct_Score = (max(BarrelPct_Rank) - BarrelPct_Rank + 1) / max(BarrelPct_Rank) * 50,
    EV95Pct_Score = (max(EV95Pct_Rank) - EV95Pct_Rank + 1) / max(EV95Pct_Rank) * 50,
    LA10_30Pct_Score = (max(LA10_30Pct_Rank) - LA10_30Pct_Rank + 1) / max(LA10_30Pct_Rank) * 50
  )

# Define weights for each metric
weights <- c(
  wOBA = 0.5,
  AvgEV = 0.25,
  BarrelPct = 0.10,
  EV95Pct = 0.10,
  LA10_30Pct = 0.05
)

# Calculate performance score for each batter
performance_data <- performance_data %>%
  mutate(
    Performance_Score = (
      wOBA_Score * weights["wOBA"] +
        AvgEV_Score * weights["AvgEV"] +
        BarrelPct_Score * weights["BarrelPct"] +
        EV95Pct_Score * weights["EV95Pct"] +
        LA10_30Pct_Score * weights["LA10_30Pct"]
    )
  )

# View the final performance score for each batter
performance_data



# Import Trackman Data Excel File
TrackmanData <- read_excel("TrackmanData.xlsx")

# Define the strike zone boundaries (based on MLB standards)
strike_zone <- list(
  top = 3.5,     # Top of the zone
  bottom = 1.5,  # Bottom of the zone
  left = -0.8,   # Left side of the zone
  right = 0.8    # Right side of the zone
)

# Add a new column to classify whether each pitch is in the strike zone
TrackmanData <- TrackmanData %>%
  mutate(
    InZone = PlateLocSide >= strike_zone$left & PlateLocSide <= strike_zone$right &
      PlateLocHeight >= strike_zone$bottom & PlateLocHeight <= strike_zone$top
  )

# Calculate Chase % for each batter (swings at pitches outside the strike zone)
ChasePct_by_Batter <- TrackmanData %>%
  group_by(Batter) %>%
  summarize(
    total_pitches_outside_zone = sum(!InZone, na.rm = TRUE),
    chase_swings = sum(!InZone & PitchCall %in% c("StrikeSwinging", "FoulBall"), na.rm = TRUE),
    ChasePct = ifelse(total_pitches_outside_zone > 0, chase_swings / total_pitches_outside_zone * 100, 0)
  )

# Calculate Whiff % (swings and misses at all pitches)
WhiffPct_by_Batter <- TrackmanData %>%
  group_by(Batter) %>%
  summarize(
    total_swings = sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), na.rm = TRUE),
    whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
    WhiffPct = ifelse(total_swings > 0, whiffs / total_swings * 100, 0)
  )

# Calculate In-Zone Whiff % (swings and misses at pitches in the strike zone)
InZoneWhiffPct_by_Batter <- TrackmanData %>%
  group_by(Batter) %>%
  summarize(
    total_in_zone_swings = sum(InZone & PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), na.rm = TRUE),
    in_zone_whiffs = sum(InZone & PitchCall == "StrikeSwinging", na.rm = TRUE),
    InZoneWhiffPct = ifelse(total_in_zone_swings > 0, in_zone_whiffs / total_in_zone_swings * 100, 0)
  )

# Calculate FB Whiff % (whiffs on fastballs)
FBWhiffPct_by_Batter <- TrackmanData %>%
  group_by(Batter) %>%
  summarize(
    total_fastball_swings = sum(TaggedPitchType == "Fastball" & PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), na.rm = TRUE),
    fastball_whiffs = sum(TaggedPitchType == "Fastball" & PitchCall == "StrikeSwinging", na.rm = TRUE),
    FBWhiffPct = ifelse(total_fastball_swings > 0, fastball_whiffs / total_fastball_swings * 100, 0)
  )

# Calculate Offspeed Chase % (chasing offspeed pitches outside the zone)
OffspeedChasePct_by_Batter <- TrackmanData %>%
  group_by(Batter) %>%
  summarize(
    total_offspeed_pitches_outside_zone = sum(TaggedPitchType %in% c("Slider", "Curveball", "ChangeUp", "Splitter", "Forkball") & !InZone, na.rm = TRUE),
    offspeed_chase_swings = sum(TaggedPitchType %in% c("Slider", "Curveball", "ChangeUp", "Splitter", "Forkball") & !InZone & PitchCall == "StrikeSwinging", na.rm = TRUE),
    OffspeedChasePct = ifelse(total_offspeed_pitches_outside_zone > 0, offspeed_chase_swings / total_offspeed_pitches_outside_zone * 100, 0)
  )

# Combine all metrics into one data frame
plate_discipline_data <- ChasePct_by_Batter %>%
  full_join(WhiffPct_by_Batter, by = "Batter") %>%
  full_join(InZoneWhiffPct_by_Batter, by = "Batter") %>%
  full_join(FBWhiffPct_by_Batter, by = "Batter") %>%
  full_join(OffspeedChasePct_by_Batter, by = "Batter") %>%
  replace_na(list(ChasePct = 0, WhiffPct = 0, InZoneWhiffPct = 0, FBWhiffPct = 0, OffspeedChasePct = 0))

# Scale each plate discipline metric to a maximum of 50
plate_discipline_data <- plate_discipline_data %>%
  mutate(
    ChasePct_Score = ifelse(!is.na(ChasePct) & !is.infinite(ChasePct),
                            pmin((max(ChasePct, na.rm = TRUE) - ChasePct) / max(ChasePct, na.rm = TRUE) * 50, 50),
                            0),
    WhiffPct_Score = ifelse(!is.na(WhiffPct) & !is.infinite(WhiffPct),
                            pmin((max(WhiffPct, na.rm = TRUE) - WhiffPct) / max(WhiffPct, na.rm = TRUE) * 50, 50),
                            0),
    InZoneWhiffPct_Score = ifelse(!is.na(InZoneWhiffPct) & !is.infinite(InZoneWhiffPct),
                                  pmin((max(InZoneWhiffPct, na.rm = TRUE) - InZoneWhiffPct) / max(InZoneWhiffPct, na.rm = TRUE) * 50, 50),
                                  0),
    FBWhiffPct_Score = ifelse(!is.na(FBWhiffPct) & !is.infinite(FBWhiffPct),
                              pmin((max(FBWhiffPct, na.rm = TRUE) - FBWhiffPct) / max(FBWhiffPct, na.rm = TRUE) * 50, 50),
                              0),
    OffspeedChasePct_Score = ifelse(!is.na(OffspeedChasePct) & !is.infinite(OffspeedChasePct),
                                    pmin((max(OffspeedChasePct, na.rm = TRUE) - OffspeedChasePct) / max(OffspeedChasePct, na.rm = TRUE) * 50, 50),
                                    0)
  )

# Define weights for each plate discipline metric (sum of weights = 1)
plate_discipline_weights <- c(
  ChasePct = 0.25,
  WhiffPct = 0.25,
  InZoneWhiffPct = 0.20,
  FBWhiffPct = 0.15,
  OffspeedChasePct = 0.15
)

# Calculate the final plate discipline score for each batter
plate_discipline_data <- plate_discipline_data %>%
  mutate(
    PlateDiscipline_Score = (
      ChasePct_Score * plate_discipline_weights["ChasePct"] +
        WhiffPct_Score * plate_discipline_weights["WhiffPct"] +
        InZoneWhiffPct_Score * plate_discipline_weights["InZoneWhiffPct"] +
        FBWhiffPct_Score * plate_discipline_weights["FBWhiffPct"] +
        OffspeedChasePct_Score * plate_discipline_weights["OffspeedChasePct"]
    )
  )


# View the final plate discipline score for each batter
plate_discipline_data



# Merge performance data and plate discipline data
final_data <- performance_data %>%
  full_join(plate_discipline_data, by = "Batter") %>%
  
  # Calculate the total score by summing performance and plate discipline scores
  mutate(Total_Score = Performance_Score + PlateDiscipline_Score)

# View the final data with total scores
final_data






