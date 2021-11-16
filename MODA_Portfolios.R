require(RSocrata)
require(rGBAT16AB)
# Custom Functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# load in the HPD Multiple Dwelling registration file: tesw-yqqr
reg <- read.socrata(url = 'https://data.cityofnewyork.us/resource/tesw-yqqr.csv')
reg0 <- reg

# create BBL10
reg$BBL <- paste(as.numeric(reg$boroid), # boro id field
                 formatC(as.numeric(reg$block), # block field
                         width = 5, 
                         flag = '0',
                         format = 'd'),
                 formatC(as.numeric(reg$lot), # lot field
                         width = 4, 
                         flag = '0',
                         format = 'd'),
                 sep='')

# Assemble addresses for geocoding
reg$prop_address <- trim(paste(trim(reg$housenumber),
                               " ",
                               trim(reg$streetname),
                               sep = ""))
reg <- reg[c("BBL","bin","prop_address","zip","registrationid", "lastregistrationdate")]
names(reg)[1:4] <- c("prop_bbl","prop_bin","prop_address","prop_zip")


# drop records that are key missing info like BIN, BBL or registration ID
reg <- reg[is.na(reg$prop_bin)==FALSE &
             is.na(reg$prop_bbl)==FALSE &
             is.na(reg$registrationid)==FALSE,]

# drop records that reference a temporary or dummy BIN
reg <- reg[reg$prop_bin != 0 &
             reg$prop_bin != 1000000 &
             reg$prop_bin != 2000000 &
             reg$prop_bin != 3000000 &
             reg$prop_bin != 4000000 &
             reg$prop_bin != 5000000,]

# De-duplicate
reg <- unique(reg)

# load in the HPD registration contacts file: feu5-w2e2
con <- read.socrata(url = 'https://data.cityofnewyork.us/resource/feu5-w2e2.csv')
con0 <- con

# drop any contacts records referencing a registration ID not in our reg set
con <- con[con$registrationid %in% reg$registrationid,]

# drop contacts records missing house number, street or zip code
con <- con[is.na(con$businesshousenumber)==FALSE &
             is.na(con$businessstreetname)==FALSE &
             is.na(con$businesszip)==FALSE,]

# Geocode Business addresses using rGBAT16AB package
con$bus_addr <- paste(trim(con$businesshousenumber),
                      trim(con$businessstreetname),
                      sep=' ')
geo_start <- Sys.time()
con00 <- GBAT.zip_code(con, 
                       'registrationcontactid', 
                       'bus_addr', 
                       'businesszip')
geo_end <- Sys.time()
geo_end - geo_start
con$bus_bin <- con00$F1A_BinOfInputAddress

# Create a cleaned 'Business Address' Field
con$bus_addr <- paste(toupper(trim(con$businesshousenumber)),
                      toupper(trim(con$businessstreetname)),
                      toupper(trim(con$businesszip)),
                      toupper(trim(con$businessapartment)),
                      sep='  ')
con$bus_addr <- paste(con$bus_addr, ' ', sep='')
con$bus_addr <- gsub(" NA ", "", con$bus_addr)
con$bus_addr[trim(con$bus_addr) == ''] <- NA
con$bus_addr[trim(con$bus_addr) == 'NA'] <- NA
con$bus_addr <- trim(con$bus_addr)

# Create a 'Full Name' field for all contacts
con$FullName <- paste(con$firstname,
                      con$lastname,
                      sep = " ")

# Filter contacts to only 'Head Officer' and 'Agent' type records
#con <- con[con$type %in% c("HeadOfficer","Agent"),]

# Ensure each contact has a field for the registration date it was created on
recents <- unique(reg[c("registrationid",
                        "lastregistrationdate")])
con <- merge(x = con,
             y = recents,
             by = 'registrationid',
             all.x = T)

# Loop through buildings and collect info
z <- data.frame()
for(i in unique(reg$prop_bin)){ # For each BIN in the registration dataset:
  ## Pull all of the relevant registration IDs for this BIN
  regs <- unique(reg[reg$prop_bin==i,]$registrationid)
  prop_bbl <- reg[reg$prop_bin==i,]$prop_bbl
  prop_address <- reg[reg$prop_bin==i,]$prop_address
  prop_zip <- reg[reg$prop_bin==i,]$prop_zip
   
  ## Pull all of the relevant contacts for these registration IDs
  cons <- con[con$registrationid %in% regs,] # needs business address, business BIN, registration date & only agent/head officers
  
  ## Calculate data points of interest
  # Most Common
  mcba <- tail(names(sort(table(cons$bus_addr))), 1) # most common business address
  mcba[is.null(mcba)==TRUE]<-NA
  
  mcbb <- tail(names(sort(table(cons$bus_bin))), 1) # most common business bin
  mcbb[is.null(mcbb)==TRUE]<-NA
  
  mcan <- tail(names(sort(table(cons[cons$type=="Agent",]$FullName))), 1) # most common agent name
  mcan[is.null(mcan)==TRUE]<-NA
  
  mcho <- tail(names(sort(table(cons[cons$type=="HeadOfficer",]$FullName))), 1) # most common headofficer name
  mcho[is.null(mcho)==TRUE]<-NA
  
  # Most Recent
  mrba <- cons[cons$lastregistrationdate == sort(cons$lastregistrationdate,
                                                 decreasing = T)[1],]$bus_addr
  mrba <- tail(names(sort(table(mrba))), 1) # most recent business address
  mrba[is.null(mrba)==TRUE]<-NA
  
  mrbb <- cons[cons$lastregistrationdate == sort(cons$lastregistrationdate,
                                                 decreasing = T)[1],]$bus_bin
  mrbb <- tail(names(sort(table(mrbb))), 1) # most recent business bin
  mrbb[is.null(mrbb)==TRUE]<-NA
  
  mran <- cons[cons$lastregistrationdate == sort(cons$lastregistrationdate,
                                                 decreasing = T)[1] &
                 cons$type=="Agent",]$FullName 
  mran <- tail(names(sort(table(mran))), 1) # most recent agent name
  mran[is.null(mran)==TRUE]<-NA
  
  mrho <- cons[cons$lastregistrationdate == sort(cons$lastregistrationdate,
                                                 decreasing = T)[1] &
                 cons$type=="HeadOfficer",]$FullName 
  mrho <- tail(names(sort(table(mrho))), 1) # most recent head officer name
  mrho[is.null(mrho)==TRUE]<-NA
  
  # Build all the data points into one longitudinal record
  b <- data.frame(i,
                  as.character(prop_bbl),
                  as.character(prop_address),
                  as.character(prop_zip),
                  as.character(mcba),
                  as.character(mrba),
                  as.character(mcbb),
                  as.character(mrbb),
                  as.character(mcan),
                  as.character(mran),
                  as.character(mcho),
                  as.character(mrho))
  
  # Ensure all fields of the record are character type for handling later
  b[,1] <- as.character(b[,1])
  b[,2] <- as.character(b[,2])
  b[,3] <- as.character(b[,3])
  b[,4] <- as.character(b[,4])
  b[,5] <- as.character(b[,5])
  b[,6] <- as.character(b[,6])
  b[,7] <- as.character(b[,7])
  b[,8] <- as.character(b[,8])
  b[,9] <- as.character(b[,9])
  b[,10] <- as.character(b[,10])
  b[,11] <- as.character(b[,11])
  b[,12] <- as.character(b[,12])
  
  # Commit the record back to a list of all buildings
  z <- rbind(z, b)
}

# Update columns names to reflect correct fields
names(z) <- c('Property_BIN',
              'Property_BBL',
              'Property_Address',
              'Property_ZIP',
              'MCBA',
              'MRBA',
              'MCBB',
              'MRBB',
              'MC_AgentName',
              'MR_AgentName',
              'MC_HeadOfficerName',
              'MR_HeadOfficerName')

# Save file as reg2 and remove placeholder files
reg2 <- z
reg2 <- unique(reg2)
reg2$uni <- row.names(reg2)

# Geocode the final output 
reg2.geo <- GBAT.zip_code(reg2,
                          "uni",
                          "Property_Address",
                          "Property_ZIP")
reg2$Property_Lat <- reg2.geo$F1A_Latitude
reg2$Property_Lon <- reg2.geo$F1A_Longitude
reg2$Property_XCoord <- reg2.geo$F1A_Xcoordinate
reg2$Property_YCoord <- reg2.geo$F1A_Ycoordinate

# Using 'Most Common'
reg3 <- reg2[c("Property_Address",
               "Property_BIN",
               "Property_BBL",
               "Property_XCoord",
               "Property_YCoord",
               "Property_Lon",
               "Property_Lat",
               "MCBA",
               "MCBB",
               "MC_AgentName",
               "MC_HeadOfficerName")]

write.csv(reg3,
          "C:/Users/RZirngibl/Desktop/LearningSummit/output",
          row.names = F)
