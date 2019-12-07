library(censusapi)
library(dplyr)
library(data.table)
options(scipen=999)


# Import and process 2017 homicide rate data from CDC WONDER dataset
homicides <- read.delim2(
  file = "input/2017 deaths by assault CDC WONDER.txt",
  sep = "\t",
  header = T,
  colClasses = c("County.Code"="character")
) %>%
  filter(Crude.Rate != "Unreliable")

homicides$Crude.Rate <- as.numeric(as.character(homicides$Crude.Rate))
homicides <- separate(
  data = homicides,
  col = County.Code,
  into = c("StateFIPS","CountyFIPS"),
  sep = 2,
  remove = F
)


# Import and process Census Bureau data
censusAPI <- Sys.getenv("CENSUS_API")

Func.ProcessCensusData <- function(api,dataNm,vint,vr,eng){
  output <- getCensus(
    key = api,
    name = dadtaNm,
    vintage = vint,
    vars = vrs,
    region = "county:*"
  )
  names(output) <- c(eng,vr)
}


dummy <- getCensus(
  key = censusAPI,
  name = "acs/acs1/cprofile",
  vintage = 2017,
  vars = c("CP04_2017_046E"),
  region = "county:*"
)

censusVars <- c(
  "CP03_2017_062E",
  "CP03_2017_128E",
  "CP05_2017_038E",
  "CP05_2017_077E",
  "CP02_2017_008E",
  "CP02_2017_009E",
  "CP05_2017_018E",
  "CP02_2017_067E",
  "CP02_2017_001E",
  "CP03_2017_002E",
  "CP04_2017_046E"
)
english <- c(
  "Median annual income", 
  "Percent in poverty", 
  "Percent black",
  "Percent non-Hispanic white",
  "Percent of households headed by husbandless women",
  "Percent of households headed by husbandless mothers",
  "Median age",
  "Percent of 25-and-older with college degree",
  "Households",
  "Percent in labor force",
  "Percent of homes owner-occupied"
)
demographics <- getCensus(
  key = censusAPI,
  name = "acs/acs1/cprofile",
  vintage = 2017,
  vars = censusVars,
  region = "county:*"
) %>%
  setnames(
    old = censusVars,
    new = english
  ) %>%
  filter(
    `Percent non-Hispanic white` > 0
  )


# Merge homicides and demographics
merged <- merge(
  x = homicides,
  y = demographics,
  by.x = c("StateFIPS","CountyFIPS"),
  by.y = c("state","county")
)


# Run correlations
models <- list()

models[["White"]] <- lm(
  formula = Crude.Rate ~ `Percent non-Hispanic white`,
  data = merged
)

models[["Black"]] <- lm(
  formula = Crude.Rate ~ `Percent black`,
  data = merged
)

models[["Blk2"]] <- lm(
  formula = Crude.Rate ~ `Percent black`+ `Percent in poverty` + `Percent of households headed by husbandless women` + `Percent of 25-and-older with college degree` + Households + `Percent in labor force` + `Percent of homes owner-occupied`,
  data = merged
)

for(model in models){
  print(summary(model))
  print("==========")
}





