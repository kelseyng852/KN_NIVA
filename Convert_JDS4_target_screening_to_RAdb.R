Sys.setenv(lang = "en_US")

library(readxl) # reads Excel file
library(dplyr) # Data manipulation
library(reshape2) # data manipulation (melt function) 
library(openxlsx) # creates Excel file
library(tidyr)

#### Call file without header ####

my_data <- read_excel("Source_file_JDS4_target_screening.xlsx",
                      col_names = FALSE)

#### Remove last column (Comments) ####

my_data <- my_data[,-length(my_data)]

# Select all columns with sites and transpose them

row_data <- my_data %>% 
  select(!1:18) #Ignores columns 1-18, takes only the 
#columns containing metadata to be transposed
row_data <- t(row_data) # transpose data 

#### Keep only columns containing metadata, remove results ####


row_data <- as.data.frame(row_data) %>% 
  
  select(1:10) # selects only what it has to be transposed 



#### Create column names ####

colnames(row_data) <- c("SITE_NAME","SITE_CODE","ExtMet", "EnvCom",
                        "COUNTRY","LATITUDE","LONGITUDE", "ENTERED_DATE", "SUBSAMPLE", "SAMPLE_SPECIES")

#### Convert SAMPLE_DATE back to date format ####

row_data$ENTERED_DATE <- as.Date(as.numeric(row_data$ENTERED_DATE),origin = "1899-12-30")

#### Manipulate transposed metadata, add some missing database columns ####
row_data <- row_data %>% 
  mutate(LOD_VALUE = EnvCom,
         LOQ_VALUE = EnvCom,
         SAMPLE_DATE = ENTERED_DATE,
         SAMPLING_METHOD = EnvCom,
         FRACTION = EnvCom,
         SAMPLE_REMARK = EnvCom,
         MEASURED_UNIT = EnvCom,
         SAMPLE_SPECIES_GENDER = "Not relevant",
         SAMPLE_SPECIES_LIFESTAGE = "Not relevant",
         SAMPLE_TISSUE = "Not relevant",
         SAMPLE_MATRIX = replace(EnvCom,
                                 EnvCom == "Groundwater",
                                 "Groundwater"),
         ENVIRON_COMPARTMENT = replace(EnvCom,
                                       EnvCom == "Groundwater",
                                       "Fresh water"),
         ENVIRON_COMPARTMENT = replace(ENVIRON_COMPARTMENT, 
                                       ENVIRON_COMPARTMENT == "River water",
                                       "Fresh water"),
         ENVIRON_COMPARTMENT = replace(ENVIRON_COMPARTMENT, 
                                       ENVIRON_COMPARTMENT != "Fresh water",
                                       "Other (add to SAMPLE_REMARK)"),
         FRACTION = replace(EnvCom, EnvCom == "Groundwater", "Aqueous"),
         FRACTION = replace(FRACTION, 
                            FRACTION == "River water", 
                            "Aqueous"),
         FRACTION = replace(FRACTION, 
                            FRACTION == "Effluent wastewater", 
                            "Aqueous"),
         FRACTION = replace(FRACTION, 
                            FRACTION == "Influent wastewater", 
                            "Aqueous"),
         FRACTION = replace(FRACTION, 
                            FRACTION == "Biota", 
                            "Total"),
         FRACTION = replace(FRACTION, 
                            FRACTION == "Sediment", 
                            "Total"),
         MEASURED_UNIT = replace(EnvCom, EnvCom == "Groundwater", "ng/L"),
         MEASURED_UNIT = replace(MEASURED_UNIT, MEASURED_UNIT == "River water", 
                                 "ng/L"),
         MEASURED_UNIT = replace(MEASURED_UNIT, MEASURED_UNIT == "Effluent wastewater", 
                                 "ng/L"),
         MEASURED_UNIT = replace(MEASURED_UNIT, MEASURED_UNIT == "Influent wastewater", 
                                 "ng/L"),
         MEASURED_UNIT = replace(MEASURED_UNIT, MEASURED_UNIT == "Biota", 
                                 "ug/kg"),
         MEASURED_UNIT = replace(MEASURED_UNIT, MEASURED_UNIT == "Sediment", 
                                 "ug/kg dw"))

row_data$SAMPLE_REMARK<-gsub("Biota","",row_data$SAMPLE_REMARK)
row_data$SAMPLE_REMARK<-gsub("Sediment","",row_data$SAMPLE_REMARK)
row_data$SAMPLE_REMARK<-gsub("River water","",row_data$SAMPLE_REMARK)
row_data$SAMPLE_REMARK<-gsub("Groundwater","",row_data$SAMPLE_REMARK)
row_data$SAMPLE_MATRIX<-gsub("Influent wastewater","Wastewater",row_data$SAMPLE_MATRIX)
row_data$SAMPLE_MATRIX<-gsub("Effluent wastewater","Wastewater",row_data$SAMPLE_MATRIX)

row_data <- row_data  %>% 
  mutate(LOQ_UNIT = MEASURED_UNIT,
         LOD_UNIT = MEASURED_UNIT)



#### Call file with header ####

my_data_Header <- read_excel("Source_file_JDS4_target_screening.xlsx")

# remove transposed metadata plus last column (comments)

my_data_Header <- my_data_Header[c(-1:-9),-length(my_data_Header)] 

#### Transpose site columns (from column 19 on) ####

melting_data = melt(my_data_Header, id = 1:18) 




#### rename melted columns #### 

melting_data = melting_data %>% 
  rename(SITE_NAME = variable,
         MEASURED_VALUE = value) 

melting_data = melting_data %>% 
  mutate(MEASURED_FLAG = MEASURED_VALUE,
         ANALYTICAL_PROTOCOL_ID = "5",
         DATA_TYPE = "Stressor",
         MEASURED_CATEGORY = "External",
         MEASURED_TYPE = "Concentration",
         ENTERED_BY = "KNG") 


#### Merge data frames ####

final <- merge(melting_data, row_data, by = "SITE_NAME")

#### Adds db columns missing in the imported file ####

final <- final %>% 
  mutate(CAMPAIGN = "JDS4", .before = 1)

final <- final %>% 
  rename(DATA_TYPE_SUB = categs,
         STRESSOR_NAME = Name,
         ORGANISATION = Contributor) 



final <- final %>% 
  mutate(SAMPLING_METHOD = replace(SAMPLING_METHOD,
                                   ORGANISATION == "JRC",
                                   "LVSPE"),
         SAMPLING_METHOD = replace(SAMPLING_METHOD,
                                   ORGANISATION %in% c("LfU", "UoA"),
                                   "LVSPE"),
         SAMPLING_METHOD = replace(SAMPLING_METHOD, 
                                   EnvCom == "Biota", 
                                   "Biopsy"),
         SAMPLING_METHOD = replace(SAMPLING_METHOD, 
                                   EnvCom == "Sediment", 
                                   "No information/not reported"))

final$MEASURED_FLAG = replace(x = final$MEASURED_VALUE, 
                              list =  !final$MEASURED_VALUE %in% c("<LOD", "<LOQ"), 
                              values =  "")


final <- final %>% 
  mutate(EXTRACTION_METHOD = replace(SAMPLING_METHOD,
                                     SAMPLING_METHOD == "LVSPE",
                                     "SPE Oasis HLB"),
         EXTRACTION_METHOD = replace(SAMPLING_METHOD,
                                     SAMPLING_METHOD != "LVSPE",
                                     "Methanol extraction"),
         
         EXTRACTION_PROTOCOL_ID = replace(EnvCom,
                                          ORGANISATION == "UoA",
                                          "2"),
         EXTRACTION_PROTOCOL_ID = replace(EXTRACTION_PROTOCOL_ID,
                                          ORGANISATION %in% c("LfU", "JRC"),
                                          "5"),
         EXTRACTION_PROTOCOL_ID = replace(EXTRACTION_PROTOCOL_ID, 
                                          EnvCom == "Biota", 
                                          "3"),
         EXTRACTION_PROTOCOL_ID = replace(EXTRACTION_PROTOCOL_ID, 
                                          EnvCom == "Sediment", 
                                          "4"),
         MEASURED_REFERENCE_ID = "5",
         DATA_TYPE_SUB = "KET",
         DATA_TYPE = "",
         ENTERED_DATE = "")

final$MEASURED_VALUE<-gsub("<LOD","",final$MEASURED_VALUE)
final$MEASURED_VALUE<-gsub("<LOQ","",final$MEASURED_VALUE)

final <- final %>% 
  mutate(ANALYTICAL_METHOD = "LCMS/MS",
         MEASURED_REFERENCE_ID = "5",
         FRACTION_PROTOCOL_ID = "5")

final$LOD_VALUE[final$LOD_VALUE == 'Biota'] <- final$LOD_Biota
final$LOD_VALUE[final$LOD_VALUE == 'Effluent wastewater'] <- final$LOD_w
final$LOD_VALUE[final$LOD_VALUE == 'Influent wastewater'] <- final$LOD_w
final$LOD_VALUE[final$LOD_VALUE == 'River water'] <- final$LOD_w
final$LOD_VALUE[final$LOD_VALUE == 'Groundwater'] <- final$LOD_w
final$LOD_VALUE[final$LOD_VALUE == 'Sediment'] <- final$LOD_Sediment

final$LOQ_VALUE[final$LOQ_VALUE == 'Biota'] <- final$LOQ_Biota
final$LOQ_VALUE[final$LOQ_VALUE == 'Effluent wastewater'] <- final$LOQ_w
final$LOQ_VALUE[final$LOQ_VALUE == 'Influent wastewater'] <- final$LOQ_w
final$LOQ_VALUE[final$LOQ_VALUE == 'River water'] <- final$LOQ_w
final$LOQ_VALUE[final$LOQ_VALUE == 'Groundwater'] <- final$LOQ_w
final$LOQ_VALUE[final$LOQ_VALUE == 'Sediment'] <- final$LOQ_Sediment

final$ID <- 1:nrow(final) 


#### Create Excel file ####

my_workbook<- createWorkbook()
addWorksheet(
  my_workbook,
  sheetName = "Sheet1"
) 
writeData(
  my_workbook,
  sheet = "Sheet1",
  final,
  startRow = 1,
  startCol = 1
)

### Save Excel file ####

saveWorkbook(my_workbook, "JDS4_source_file_curated.xlsx",
             overwrite = TRUE)

#Read RAdb import template format

RAdb <- read_excel("RAdb import template.xlsx", sheet = "RA_ENVIRONMENTAL_CON_EXT")

#Fill in RAdb import template with JDS4 target screening results
RAdb$ID <- final$ID
RAdb$CAMPAIGN <- final$CAMPAIGN
RAdb$ENVIRON_COMPARTMENT <- final$ENVIRON_COMPARTMENT
RAdb$SITE_CODE <- final$SITE_CODE
RAdb$SAMPLE_DATE <- final$SAMPLE_DATE
RAdb$SAMPLE_MATRIX <- final$SAMPLE_MATRIX
RAdb$SAMPLE_SPECIES <- final$SAMPLE_SPECIES
RAdb$SAMPLE_SPECIES_GENDER <- final$SAMPLE_SPECIES_GENDER
RAdb$SAMPLE_SPECIES_LIFESTAGE <- final$SAMPLE_SPECIES_LIFESTAGE
RAdb$SAMPLE_TISSUE <- final$SAMPLE_TISSUE
RAdb$SAMPLE_REMARK <- final$SAMPLE_REMARK
RAdb$SAMPLING_METHOD <- final$SAMPLING_METHOD
RAdb$SUBSAMPLE <- final$SUBSAMPLE
RAdb$FRACTION <- final$FRACTION
RAdb$EXTRACTION_METHOD <- final$EXTRACTION_METHOD
RAdb$EXTRACTION_PROTOCOL_ID <- final$EXTRACTION_PROTOCOL_ID
RAdb$ANALYTICAL_METHOD <- final$ANALYTICAL_METHOD
RAdb$ANALYTICAL_PROTOCOL_ID <- final$ANALYTICAL_PROTOCOL_ID
RAdb$DATA_TYPE <- final$DATA_TYPE
RAdb$DATA_TYPE_SUB <- final$DATA_TYPE_SUB
RAdb$MEASURED_CATEGORY <- final$MEASURED_CATEGORY
RAdb$MEASURED_TYPE <- final$MEASURED_TYPE
RAdb$STRESSOR_NAME <- final$STRESSOR_NAME
RAdb$INCHIKEY_STANDARD <- final$INCHIKEY_STANDARD
RAdb$MEASURED_VALUE <- final$MEASURED_VALUE
RAdb$MEASURED_FLAG <- final$MEASURED_FLAG
RAdb$MEASURED_UNIT <- final$MEASURED_UNIT
RAdb$LOQ_VALUE <- final$LOQ_VALUE
RAdb$LOQ_UNIT <- final$LOQ_UNIT
RAdb$LOD_VALUE <- final$LOD_VALUE
RAdb$LOD_UNIT <- final$LOD_UNIT
RAdb$MEASURED_REFERENCE_ID <- final$MEASURED_REFERENCE_ID
RAdb$FRACTION_PROTOCOL_ID <- final$FRACTION_PROTOCOL_ID
RAdb$ORGANISATION <- final$ORGANISATION
RAdb$ENTERED_BY <- final$ENTERED_BY
RAdb$ENTERED_DATE <- final$ENTERED_DATE

write.xlsx(RAdb,"RAdb_destination_file.xlsx")
