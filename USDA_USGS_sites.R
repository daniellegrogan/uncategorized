

library(dataRetrieval) # functions to access USGS online data

COUNTY = as.data.frame(countyCd)
MN = COUNTY[COUNTY$STUSAB == "MN",]
county.list = MN[MN$COUNTY_NAME %in% c("Anoka County", 
                                       "Hennepin County", 
                                       "Carver County", 
                                       "Scott County", 
                                       "Dakota County", 
                                       "Washington County", 
                                       "Ramsey County"),]
)




Q.sites = whatNWISsites(countyCd=c("27003","27019", "27037", "27053", "27123", "27139", "27163"),
              service="dv", 
              parameterCd="00060")


# USGS N sites
N.sites = whatWQPsites(countycode=c("US:27:003", "US:27:019", "US:27:037", "US:27:053", "US:27:123", "US:27:139", "US:27:163"), 
                       characteristicName="Nitrogen")

# subset to surface water
unique(N.sites$MonitoringLocationTypeName)
N.sites.sw = N.sites[N.sites$MonitoringLocationTypeName %in% 
                       c("Stream", "River/Stream", "River/Stream Intermittent", "River/Stream Perennial"),]
