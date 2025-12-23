#' SWATHdata
#'
#' Function to read in PISCO kelp swath data and format into dataframe
#'
#' @return Dataframe
#'
#' @export
# Read in SWATH data

SWATHdata <- function(){

  # Species = multi-digit name [THIS CODE BASICALLY ASSUMES WE WANT MACROCYSTIS STIPES]
  Species <- 'MACPYRAD'

  D <- read.csv('data/PISCO_kelpforest_swath.1.2.csv',header=TRUE)

  # Only take SBTL_SWATH
  D <- D[D$method=='SBTL_SWATH_PISCO',]

  # Population with zeros:
  D$Temp<-0
  OKrows = D$classcode==Species
  OKrows[is.na(OKrows)] = FALSE
 # D$Temp[OKrows]<- D$macstipes[OKrows] # Would need to change this line if you want something besides macrocystis
 D$Temp[OKrows] <- D$count[OKrows]

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
 D$site2[D$site == 'SCI_SCORPION_ANCHORAGE'] = 'SCI_SCORPION_ANCHORAGE'
 D$site2[D$site == 'SCI_SCORPION_E'|D$site == 'SCI_SCORPION_W'] = 'SCI_SCORPION'
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


  # Tally up number of stipes per transect. Combine across sides in a given site.
  #D2<-aggregate(D$Temp,by=list(Site=D$site,Year=D$year,Side=D$side,
  #                             Zone=D$zone,Transect=D$transect),FUN=sum)
  D2<-aggregate(D$Temp,by=list(Site=D$site2,Side=D$site,Year=D$year,Month=D$month,Day=D$day,
                               Zone=D$zone,Transect=D$transect),FUN=sum)

  # Eliminate the rows corresponding to the sites we aren't using (which got pooled into 'NA' when assigning names)
  D2 <- D2[D2$Site!='NA',]


  # Get total count, plus sample size.
 # D2total <- aggregate(D$Temp,by=list(Site=D$site,Side=D$side,Month=D$month,Day=D$day,Year=D$year),FUN=sum)
#  D2ntrans <- aggregate(D$Temp,by=list(Site=D$site,Side=D$side,Month=D$month,Day=D$day,Year=D$year),FUN=length)
  D2total <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=sum)
  D2ntrans <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=length)
  D2sd <- aggregate(D2$x,by=list(Site=D2$Site,Year=D2$Year),FUN=sd)

  # Count per transect
  D2total$stipespt <- D2total$x/D2ntrans$x
  D2total$stipespt.sd <- D2sd$x

  #debugging:
  Dsub = D[D$site2=='ANACAPA_WEST_ISLE' & D$year==2018,]
  write_csv(Dsub,'Dsubk.csv')

  # Now sum up by year:
 # D3.x<-aggregate(D2total$x,by=list(Site=D2total$Site,Side=D2total$Side,Year=D2total$Year),FUN=sum)
#  D3.stipespt<-aggregate(D2total$stipespt,by=list(Site=D2total$Site,Side=D2total$Side,Year=D2total$Year),FUN=sum)

  # Rename to be consistent with naming conventions downstream
  D3 <- D2total

  # Fix the names & combine frames
 # D3<-D3.x
#  D3$stipespt <- D3.stipespt$x

  # Make column names match species name:
  colnames(D3)[colnames(D3)=='x']<-Species
 # colnames(D3)[colnames(D3)=='fpt']<-paste(Species,'.stipespt',sep="")

  return(D3)
}


