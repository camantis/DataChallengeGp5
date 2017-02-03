
Sfacts = read.csv("county_facts.csv", sep=",", header=TRUE)

Eresults = read.csv("US_County_Level_Presidential_Results_12-16.csv", sep=",", header=TRUE)

#Subset Key for variables of interest
# Trump voters were economically better off - Income
# Registered voters with a college degree favored Clinton - Education
# Less educated voters favored trump - Education 
# Trump voters were more likely to be white - Race
# Hispanics/African Americans were more likely to vote for Clinton - Race
# Suburban White Women can't stand Clinton - DesperateHouseWives
# Married White Women favored clinton - DesperateHouseWives
# Geographic regions: Northeast - CT, ME, MA, NH, RI, VT, NJ, NY, PA; 
# Midwest - IL, IN, MI, OH, WI, IA, KS, MN, MO, NE, ND, SD
# South - DE, FL, GA, MD, NC, SC, VI, DC, WV, AL, KN, MS, TN, AR, LA, OK, TX; 
# West - AZ, CO, ID, MO, NV, NM, UT, WY, AK, CA, HI, OR, WA

Income = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","INC110213",
"PVY020213")]


Education = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","EDU635213",
                                         "EDU685213")]

Race = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation",
"RHI125214","RHI225214","RHI325214","RHI425214","RHI525214","RHI625214","RHI725214","RHI825214")]

DesperateHouseWives = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","SEX255214")]                                    
#These two questions might be really difficult to answer with the info we have. 
# As far as I can tell, no info on marriage percentages, and surburban could be teased out but it isn't clear...

NE =Sfacts[Sfacts$state_abbreviation %in% c("ME","CT","MA","NH","RI","VT","NJ","NY","PA"),]
droplevels(NE$area_name)
droplevels(NE$state_abbreviation)
                                    
MW = Sfacts[Sfacts$state_abbreviation %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD"),]                              
droplevels(MW$area_name)
droplevels(MW$state_abbreviation)

SO = Sfacts[Sfacts$state_abbreviation %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL",
                                             "KY","MS","TN","AR","LA","OK","TX"),]
droplevels(SO$area_name)
droplevels(SO$state_abbreviation)

WE = Sfacts[Sfacts$state_abbreviation %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA"),]
droplevels(WE$area_name)
droplevels(WE$state_abbreviation)
