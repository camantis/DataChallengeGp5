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


#1. Income (e.g. Trump voters were economically better off)
Income = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","INC110213",
"PVY020213")] #Creating a subset of the data we'll use for 'Income.' Considers  median household income (INC110213), and persons below the poverty level (PVY020213) for all states and all counties. 

#Theory Check: We want to come up with a way to characterize the Economic profile of each area_name/county_name. Why not look at other variables for income? Alternative code:

Income2 = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation", "INC110213","INC910213","HSG445213","HSG096213", "HSG495213","HSD310213","HholdIncomePerPerson","PVY020213")]
#Added these variables:
#INC910213: Per capita money income in the past 12 months
#HSG445213: Homeownership rate
#HSG096213: Housing units in multi-unit structures, percent, 2009-2013
#HSG495213: Median value of owner-occupied housing units
#HSD310213: Persons per household (see http://www.oecd.org/statistics/OECD-ICW-Framework-Chapter2.pdf for theory behind)

#Assumptions: 
#1) We are using housing and income as proxies for "economically better off"

#New variable created:
HholdIncomePerPerson=Sfacts$INC110213/Sfacts$HSD310213

### 2. Education (e.g. "Registered voters with a college degree favored Clinton)
Education = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","EDU635213", "EDU685213")]

#EDU635213: High school graduate or higher
#EDU685213: Bachelor's degree or higher
#Theory check: Looks good.

### 3. Race (e.g. "Trump voters were more likely to be white")
Race = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation",
"RHI125214","RHI225214","RHI325214","RHI425214","RHI525214","RHI625214","RHI725214","RHI825214")]

#Possibly add these variables:
#POP645213: Foreign-born persons
#POP815213: Language other than English spoken at home
#SBO315207: Black-owned firms
#SBO115207: American Indian and Alaska Native-owned firms
#SBO215207: Asian-owned firms
#SBO515207: Native Hawaiian and other Pacific Islander owned firms
#SBO415207: Hispanic-owned firms

##Or, create a variable for each race
#Trump voters were more likely to be white - Race
White = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation", "RHI125214","RHI825214","WhiteFirms")]
#Add to this the number of white firms: 
WhiteFirms=Sfacts$SBO001207-c(Sfacts$SBO315207|Sfacts$SBO115207|Sfacts$SBO215207|Sfacts$SBO515207|Sfacts$SBO415207)

#Hispanics/African Americans were more likely to vote for Clinton:
HisAfrican=Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation", "RHI225214","RHI725214","SBO315207","SBO415207")]

#Suburban White Women can't stand Clinton
DesperateHouseWives = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","SEX255214")]  

#Theory check: How can we include the 'surburban' component? 
DesperateHouseWivesSuburb = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","SEX255214","HSG096213","LND110210","POP060210")] 

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
