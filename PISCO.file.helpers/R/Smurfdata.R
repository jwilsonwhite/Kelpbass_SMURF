#' Smurfdata
#'
#' Read in PISCO SMURF data and format into dataframe
#'
#' @return Dataframe
#'
#' @export
#
Smurfdata <- function(Species,Names=FALSE){
  D <- read.csv('data/PISCO_UCSB_subtidal_recruitment_fish_data.1.2.csv',header=TRUE)

  # Population with zeros (although it looks like this is not an issue for the SMURF data, at least not for all dates):
  #D$Temp<-0
  #D$Temp[D$Species==Species]<- D$Number.fish[D$Species==Species]
  D$SITE = D$site_code

  D$Island<-NA # Insert Island ID
  D$Island[D$SITE=='HAZ-WEST'|D$SITE=='PEL-WEST'|D$SITE=='TRAIL'|D$SITE=='SMU'|D$SITE=='WIL'
           |D$SITE=='COCHE'|D$SITE=='LITTLE SCORP'|D$SITE=='PAINTED CAVE'|D$SITE=='PRISONERS'
           |D$SITE=='VALLEY'|D$SITE=='GULL'|D$SITE=='SCORP'|D$SITE=='ARCH-ROCK'|D$SITE=='MORSE'
           |D$SITE=='YEL'] = 'SCI'

  D$Island[D$SITE=='ANA-LAND'|D$SITE=='ANA-WIN'|D$SITE=='ANA-SOUTH']='ANA'
  D$Island[D$SITE=='SMI-NORTH'|D$SITE=='SMI-SOUTH'|D$SITE=='SMI-BAY']='SMI'
  D$Island[D$SITE=='SRI-CARR'|D$SITE=='SRI-SOUTH']='SRI'
  D$Island[D$SITE=='COJO'|D$SITE=='PUR'|D$SITE=='JAL'|D$SITE=='CARP'|D$SITE=='NAPLES'
           |D$SITE=='PLATTS'|D$SITE=='AQM'|D$SITE=='ELLWOOD']='MAIN'

  # Obtain Lat & Lon
  Coord = read.csv('data/PISCO_UCSB_subtidal_recruitment_site_list.1.1.csv',header=TRUE)
  D$Lat <- NA
  D$Long <- NA
  for (i in 1:length(Coord$site_code)){
    D$Lat[D$SITE==Coord$site_code[i]]=Coord$LAT_WGS84[i]
    D$Long[D$SITE==Coord$site_code[i]]=Coord$LONG_WGS84[i]
  }

  # Filter to species of interest
  D2 <- D[D$classcode==Species,]

  # Rename columns to match names used in legacy code:
  D2$Month = D2$month
  D2$Day = D2$day
  D2$Year = D2$year
  D2$fsd = D2$recruitment_rate
  D2$fish = D2$total_fish_collected

  # Summarize just for species of interest:
 # D2<-aggregate(D$Temp,by=list(Island=D$Island,Site=D$SITE,Month=D$Month,Day=D$Day,Year=D$Year,NumDays=D$NumDays,N.Smurfs=D$N.Smurfs,
#                               Lat=D$Lat,Lon=D$Long),FUN=sum)
#  D2$fsd = D2$x / D2$NumDays / D2$N.Smurfs

  # Now sum up by year:
  D3.fish<-aggregate(D2$fish,by=list(Island=D2$Island,Site=D2$SITE,Year=D2$Year,
                               Lat=D2$Lat,Lon=D2$Long),FUN=sum)
  D3.fsd<-aggregate(D2$fsd,by=list(Island=D2$Island,Site=D2$SITE,Year=D2$Year,
                                   Lat=D2$Lat,Lon=D2$Lon),FUN=sum)

  # Fix the names & combine frames
  D3<-D3.fish
  D3$fsd <- D3.fsd$x

  # Make column names match species name:
  if (Names){
    colnames(D3)[colnames(D3)=='x']<-Species
    colnames(D3)[colnames(D3)=='fsd']<-paste(Species,'.fsd',sep="")
  } else {
    colnames(D3)[colnames(D3)=='x']<-'fish'
    colnames(D3)[colnames(D3)=='fsd']<-'fish.fsd'}

  #Combine some sites that were renamed and slightly relocated
  D3$Site[D3$Site=="HAZ-WEST"]<-"HAZ"
  D3$Site[D3$Site=='PEL-WEST']<-'PEL'
  D3$Site[D3$Site=='WIL']<-'VALLEY'
  D3$Site[D3$Site=='MORSE']<-'GULL'

  return(D3)
}
