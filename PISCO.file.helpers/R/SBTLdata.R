#' SBTLdata
#'
#' Read in PISCO subtidal visual transect data and format into dataframe
#'
#' @return Dataframe
#'
#' @export
#
# Read in SBTL data
SBTLdata <- function(Species,MinLen,pool.sides=TRUE,Names=FALSE){

  # Species = 4-digit name
  # MinLen = minimum length to be counted (e.g., to exclude PLCA YOY, choose 9)
  # Names = if TRUE, the resulting data frame will have data columns with the species name. Otherwise, 'fish'

  D <- read.csv('data/PISCO_kelpforest_fish.1.3.csv',header=TRUE)

  # Only take SBTL_FISH from UCSB
  D <- D[D$method=='SBTL_FISH',]

  # Population with zeros:
  D$Temp<-0
  OKrows = D$classcode==Species & D$fish_tl >= MinLen & !is.na(D$fish_tl)
  OKrows[is.na(OKrows)] = FALSE # there was one stray NA at SBI_SOUTHEAST_SEA_LION
  D$Temp[OKrows]<- D$count[OKrows]



  # Combine data across sides in a site
  # This procedure used to be different because the data were coded with separate columns for site and side.
  # With the new data format this is a bit tedious. Only do it for the sites we actually care about for this analysis.
  D$site2 <-'NA'
  D$site2[D$site == 'ANACAPA_EAST_ISLE_E'|D$site == 'ANACAPA_EAST_ISLE_W'|D$site == 'ANACAPA_EAST_ISLE_CEN'] = 'ANACAPA_EAST_ISLE'
  D$site2[D$site == 'ANACAPA_MIDDLE_ISLE_E'|D$site == 'ANACAPA_MIDDLE_ISLE_W'|D$site == 'ANACAPA_MIDDLE_ISLE_CEN'] = 'ANACAPA_MIDDLE_ISLE'
  D$site2[D$site == 'ANACAPA_WEST_ISLE_E'|D$site == 'ANACAPA_WEST_ISLE_W'|D$site == 'ANACAPA_WEST_ISLE_CEN'] = 'ANACAPA_WEST_ISLE'
  D$site2[D$site == 'ANACAPA_EAST_FISH_CAMP_E'|D$site == 'ANACAPA_EAST_FISH_CAMP_W'|D$site == 'ANACAPA_EAST_FISH_CAMP_CEN'] = 'ANACAPA_EAST_FISH_CAMP'
  D$site2[D$site == 'ANACAPA_LIGHTHOUSE_REEF_E'|D$site == 'ANACAPA_LIGHTHOUSE_REEF_W'|D$site == 'ANACAPA_LIGHTHOUSE_REEF_CEN'] = 'ANACAPA_LIGHTHOUSE_REEF'
  D$site2[D$site == 'ANACAPA_ADMIRALS_E'|D$site == 'ANACAPA_ADMIRALS_W'|D$site == 'ANACAPA_ADMIRALS_CEN'] = 'ANACAPA_ADMIRALS'
  D$site2[D$site == 'SCI_SCORPION_E'|D$site == 'SCI_SCORPION_W'] = 'SCI_SCORPION'
  D$site2[D$site == 'SCI_SCORPION_ANCHORAGE'] = 'SCI_SCORPION_ANCHORAGE'
  D$site2[D$site == 'SCI_POTATO_PASTURE_E'|D$site == 'SCI_POTATO_PASTURE_W'] = 'SCI_POTATO_PASTURE'
  D$site2[D$site == 'SCI_CAVERN_POINT_E'|D$site == 'SCI_CAVERN_POINT_W'] = 'SCI_CAVERN_POINT'
  D$site2[D$site == 'SCI_PAINTED_CAVE_E'|D$site == 'SCI_PAINTED_CAVE_W'|D$site == 'SCI_PAINTED_CAVE_CEN'] = 'SCI_PAINTED_CAVE'
  D$site2[D$site == 'SCI_HAZARDS_E'|D$site == 'SCI_HAZARDS_W'|D$site == 'SCI_HAZARDS_CEN'] = 'SCI_HAZARDS'
  D$site2[D$site == 'SCI_PELICAN_E'|D$site == 'SCI_PELICAN_W'|D$site == 'SCI_PELICAN_CEN'] = 'SCI_PELICAN'
  D$site2[D$site == 'SCI_GULL_ISLE_E'|D$site == 'SCI_GULL_ISLE_W'] = 'SCI_GULL_ISLE'
  D$site2[D$site == 'SCI_VALLEY_E'|D$site == 'SCI_VALLEY_W'|D$site == 'SCI_VALLEY_CEN'] = 'SCI_VALLEY'
  D$site2[D$site == 'SRI_SOUTH_POINT_E'|D$site == 'SRI_SOUTH_POINT_W'] = 'SRI_SOUTH_POINT'
  D$site2[D$site == 'SRI_JOHNSONS_LEE_SOUTH_E'|D$site == 'SRI_JOHNSONS_LEE_SOUTH_W'] = 'SRI_JOHNSONS_LEE_SOUTH'
  D$site2[D$site == 'SRI_JOHNSONS_LEE_NORTH_E'|D$site == 'SRI_JOHNSONS_LEE_NORTH_W'] = 'SRI_JOHNSONS_LEE_NORTH'

  # Get total count, plus sample size.
#  D2total <- aggregate(D$Temp,by=list(Site=D$site,Side=D$side,Month=D$month,Day=D$day,Year=D$year),FUN=sum)
#  D2ntrans <- aggregate(D$Temp,by=list(Site=D$site,Side=D$side,Month=D$month,Day=D$day,Year=D$year),FUN=length)

  # Tally up number of fish per transect. Pool across sides in a given site.
 D2<-aggregate(D$Temp,by=list(Site=D$site2,Side=D$site,Year=D$year,Month=D$month,Day=D$day,
                                Zone=D$zone,Level=D$level,Transect=D$transect),FUN=sum)

 # debugging:
 #Dsub = D[D$site2=='ANACAPA_EAST_ISLE' & D$year==2002,]
 #write_csv(Dsub,'Dsub.csv')

 # Eliminate the rows corresponding to the sites we aren't using (which got pooled into 'NA' when assigning names)
 D2 <- D2[D2$Site!='NA',]

 # Get total count, plus sample size.
  D2total <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=sum)
  D2ntrans <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=length)
  D2sd <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=sd)

  # Count per transect
  D2total$fpt <- D2total$x/D2ntrans$x
  D2total$fpt.sd <- D2sd$x

  # Now sum up by year & side: [deprecated]
# if (!pool.sides){
#    D3.x<-aggregate(D2total$x,by=list(Site=D2total$Site,Side=D2total$Side,Year=D2total$Year),FUN=sum)
#    D3.fpt<-aggregate(D2total$fpt,by=list(Site=D2total$Site,Side=D2total$Side,Year=D2total$Year),FUN=sum)
#  } else { # pool sides together
#    D3.x<-aggregate(D2total$x,by=list(Site=D2total$Site,Year=D2total$Year),FUN=sum)
#    D3.fpt<-aggregate(D2total$fpt,by=list(Site=D2total$Site,Year=D2total$Year),FUN=sum)
#  }

  # Fix the names & combine frames
 # D3<-D3.x
#  D3$fpt <- D3.fpt$x
  D3 <- D2total


  # Make column names match species name:
  if (Names){
    colnames(D3)[colnames(D3)=='x']<-Species
    colnames(D3)[colnames(D3)=='fpt']<-paste(Species,'.fpt',sep="")
  } else {
    colnames(D3)[colnames(D3)=='x']<-'fish'
    colnames(D3)[colnames(D3)=='fpt']<-'fish.fpt'}

  return(D3)
}
