library(censusapi)
library(dplyr)
library(data.table)
library(readxl)
options(scipen=999)

# Import and process 2013-17 homicide data from FDLE stats
FLCrime <- read_excel(
  path = "input/fdle/2013-17 FL counties homicide and index crime rates.xlsx"
)

fips <- read.csv(
  file = "input/all-geocodes-v2017.csv",
  stringsAsFactors = F,
  colClasses = c(
    "State.Code..FIPS."="character",
    "County.Code..FIPS."="character",
    "County.Subdivision.Code..FIPS."="character",
    "Place.Code..FIPS."="character",
    "Consolidtated.City.Code..FIPS."="character"
  )
) %>% 
  filter(State.Code..FIPS.=="12")

hom_fips <- merge(
  x = FLCrime,
  y = fips,
  by.x = "County",
  by.y = "Area.Name..including.legal.statistical.area.description."
)


# Import and process Census Bureau data
censusAPI <- Sys.getenv("CENSUS_API")

censusVars <- c(
  "CP03_2013_2017_062E",
  "CP03_2013_2017_128E",
  "CP05_2013_2017_038E",
  "CP05_2013_2017_077E",
  "CP02_2013_2017_008E",
  "CP02_2013_2017_009E",
  "CP05_2013_2017_018E",
  "CP02_2013_2017_067E",
  "CP02_2013_2017_001E",
  "CP03_2013_2017_002E",
  "CP04_2013_2017_046E",
  "CP04_2013_2017_003E",
  "CP02_2008_2012_007E",
  "CP02_2013_2017_093E",
  "CP03_2013_2017_009E",
  "CP04_2013_2017_004E",
  "CP04_2013_2017_005E",
  "CP02_2013_2017_152E",
  "CP02_2013_2017_040E",
  "CP04_2013_2017_047E"
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
  "Percent of homes owner-occupied",
  "Percent of homes vacant",
  "Percent of households headed by wifeless fathers",
  "Percent born outside U.S.",
  "Unemployment rate",
  "Homeowner vacancy rate",
  "Rental vacancy rate",
  "Percent with broadband internet",
  "Births per 1,000 women aged 15-19",
  "Percent of homes rented"
)
demographics <- getCensus(
  key = censusAPI,
  name = "acs/acs5/cprofile",
  vintage = 2017,
  vars = censusVars,
  region = "county:*"
) %>%
  setnames(
    old = censusVars,
    new = english
  ) %>%
  filter(
    `Percent non-Hispanic white` > 0,
    state == "12"
  )


# Merge crime rates and demographics
allData <- merge(
  x = hom_fips,
  y = demographics,
  by.x = c("State.Code..FIPS.","County.Code..FIPS."),
  by.y = c("state","county")
)


# Get all R-squared correlations
allDataNum <- allData[sapply(allData,is.numeric)]
corrs <- round(
  x = cor(
    x = allDataNum,
    use = 'p',
    method = "pearson"
  ),
  digits = 2
)

scatter.smooth(
  x = allData$`Percent of 25-and-older with college degree`,
  y = allData$`Average index crime rate, 2013-17`
)


# Run multiple linear regressions
models <- list()
models[["white"]] <- lm(
  formula = `2013-17 average murders per 100K` ~ `Percent non-Hispanic white`,
  data = allData
)
models[["black"]] <- lm(
  formula = `2013-17 average murders per 100K` ~ `Percent black`,
  data = allData
)
models[["factors"]] <- lm(
  formula = `2013-17 average murders per 100K` ~ `2013-17 average population` + `Median annual income` + `Percent in poverty` + `Percent black` + `Percent of households headed by husbandless mothers` + `Median age` + `Percent of 25-and-older with college degree`,
  data = allData
)
models[["factors2"]] <- lm(
  formula = `2013-17 average murders per 100K` ~ `Percent of households headed by husbandless mothers` + `Percent of homes rented`,
  data = allData
)
models[["factors3"]] <- lm(
  formula = `2013-17 average murders per 100K` ~ `Percent of households headed by husbandless mothers` + `Percent of homes owner-occupied`,
  data = allData
)
models[["index"]] <- lm(
  formula = `Average index crime rate, 2013-17` ~ `Percent of homes rented`,
  data = allData
)
for(model in models){
  print(summary(model))
  print("========================================================================================")
}


scatter.smooth(
  x = allData$`Percent of households headed by husbandless mothers`,
  y = allData$`2013-17 average murders per 100K`
)

scatter.smooth(
  x = allData$`Percent of homes owner-occupied`,
  y = allData$`2013-17 average murders per 100K`
)

scatter.smooth(
  x = allData$`Percent of homes owner-occupied`,
  y = allData$`Average index crime rate, 2013-17`
)


allData$BlackCategory <- ifelse(
  test = allData$`Percent black` < 10,
  yes = "Less than 10% black",
  no = ifelse(
    test = allData$`Percent black` < 25,
    yes = "10-24",
    no = ifelse(
      test = allData$`Percent black` < 50,
      yes = "25-49",
      no = "50+"
    )
  )
)

allData$WhiteCategory <- ifelse(
  test = allData$`Percent non-Hispanic white` < 10,
  yes = "Less than 10% white",
  no = ifelse(
    test = allData$`Percent non-Hispanic white` < 25,
    yes = "10-24",
    no = ifelse(
      test = allData$`Percent non-Hispanic white` < 50,
      yes = "25-49",
      no = "50+"
    )
  )
)

write.csv(
  x = allData,
  file = "output/2013-17 FL crime data and demographics by county.csv",
  na = '',
  row.names = F
)

write.csv(
  x = corrs,
  file = "output/2013-17 FL crime data and demographics R2.csv",
  na = ''
)
