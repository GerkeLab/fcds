# The fields below were copy-pasted from the table in
# `20 Appendix B FIPS County Codes for Florida.pdf`
library(stringr)
library(dplyr)

# FIPS County Codes -------------------------------------------------------

county_name <- "
ALACHUA

BAKER

BAY

BRADFORD

BREVARD

BROWARD

CALHOUN

CHARLOTTE

CITRUS

CLAY

COLLIER

COLUMBIA

DESOTO

DIXIE

DUVAL

ESCAMBIA

FLAGLER

FRANKLIN

GADSDEN

GILCHRIST

GLADES

GULF

HAMILTON

HARDEE

HENDRY

HERNANDO

HIGHLANDS

HILLSBOROUGH

HOLMES

INDIAN RIVER

JACKSON

JEFFERSON

LAFAYETTE

LAKE

LEE

LEON

LEVY

LIBERTY

MADISON

MANATEE

MARION

MARTIN

MIAMI-DADE

MONROE

NASSAU

OKALOOSA

OKEECHOBEE

ORANGE

OSCEOLA

PALM BEACH

PASCO

PINELLAS

POLK

PUTNAM

SANTA ROSA

SARASOTA

SEMINOLE

ST. JOHNS

ST. LUCIE

SUMTER

SUWANNEE

TAYLOR

UNION

VOLUSIA

WAKULLA

WALTON

WASHINGTON

UNKNOWN"

fips_code <- "
001

003

005

007

009

011

013

015

017

019

021

023

027

029

031

033

035

037

039

041

043

045

047

049

051

053

055

057

059

061

063

065

067

069

071

073

075

077

079

081

083

085

086

087

089

091

093

095

097

099

101

103

105

107

113

115

117

109

111

119

121

123

125

127

129

131

133

999"


# FIPS Data Frame --------------------------------------------------------------
split_and_clean <- function(x, by = "\n") {
  x <- str_split(x, by)[[1]]
  x[x != ""]
}

county_fips_code <- tibble(
  county_name = tools::toTitleCase(tolower(split_and_clean(county_name))),
  fips_code = str_remove(split_and_clean(fips_code), "^0+")
) %>%
  mutate(county_name = recode(county_name, "Desoto" = "DeSoto"))

use_data(county_fips_code)
