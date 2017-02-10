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
Income = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","INC110213","INC910213","PVY020213")] #Creating a subset of the data we'll use for 'Income.' Variables included:median household income (INC110213), and persons below the poverty level (PVY020213) for all states and all counties. #Madeline Added this variable:
#INC910213: Per capita money income in the past 12 months

Income = Income[!(Income$state == ""), ] #Removes the rows for which state is empty. 

EResultsAbridged =Eresults[, c("FIPS","county_name","state_abbr","votes_dem_2016","votes_gop_2016","total_votes_2016","per_dem_2016","per_gop_2016")] #Creates a subset of Election results for 2016  (removes 2012 data)
colnames(EResultsAbridged) = c("fips","county","state","demvote","gopvote","totalvote","%dem2016","%gop2016") #Shorts the column names of the EResultsAbridged dataframe. 

IncomeTotal= merge(Income,EResultsAbridged, by = "fips") #merges Income socio-economic data with election results by fips, eliminates the two extra rows from socioeconomic data by matching "fips" column from both dataframes
str(IncomeTotal)
IncomeTotal=subset(IncomeTotal, select=c("area_name", "state_abbreviation","INC910213","INC110213","PVY020213","demvote","gopvote","totalvote","%dem2016","%gop2016")) #Drop the redundant columns
colnames(IncomeTotal)=c("County","State","IncomePerCapita","MedianHHIncome","BelowPoverty","demvote","gopvote","totalvote","%dem2016","%gop2016")

tapply(IncomeTotal$BelowPoverty,IncomeTotal$State, mean) #avg poverty levels for each state
tapply(IncomeTotal$MedianHHIncome,IncomeTotal$State,mean) #avg median income levels for each state
#Next step: Correlate these two factors to states where Trump won vs. lost. High enough resolution to draw conclusion? Or do we need to study this on a county-scale...
IncomeTotal=cbind(IncomeTotal,trumpvictory=0)  #creates a new column for Trump victory
IncomeTotal$trumpvictory[IncomeTotal$gopvote>=IncomeTotal$demvote]=1 #If GOP vote>DEM vote then this is a Trump Victory
IncomeTotal$trumpvictory[IncomeTotal$trump==1]="Republican" #Labels 1=Republican
IncomeTotal$trumpvictory[IncomeTotal$trump==0]="Democrat"

aggregate(cbind(IncomePerCapita, MedianHHIncome,BelowPoverty)~trumpvictory,IncomeTotal,mean) #mean income for Democrat Counties ($49491.83) is higher than mean income for Republican Counties ($45282.09) & mean poverty rates, which is actually higher for Dem.
aggregate(cbind(IncomePerCapita, MedianHHIncome,BelowPoverty)~trumpvictory,IncomeTotal,median) #median income for Democrat Counties ($48224.5) is higher than mean income for Republican Counties ($43839)
aggregate(cbind(IncomePerCapita, MedianHHIncome,BelowPoverty)~trumpvictory,IncomeTotal,max)
aggregate(cbind(IncomePerCapita, MedianHHIncome,BelowPoverty)~trumpvictory,IncomeTotal,min)

#------------CONTINUATION FROM NICK'S EDITS
#Theory Check: We want to come up with a way to characterize the Economic profile of each area_name/county_name. Why not look at other variables for income? 
#How about adding  doing something separate for home wealth? Then could compare Income vs. Housing as two proxies for economically better off. 

Housing = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","HSG445213","HSG096213", "HSG495213","HSD310213","HholdIncomePerPerson","PVY020213")]
#Includes these variables:
#HSG445213: Homeownership rate
#HSG096213: Housing units in multi-unit structures, percent, 2009-2013
#HSG495213: Median value of owner-occupied housing units
#HSD310213: Persons per household (see http://www.oecd.org/statistics/OECD-ICW-Framework-Chapter2.pdf for theory behind)

###QUESTION for the team: Alternatively, could include these variables directly in the above code for income, rather than comparing housing and income separately. 

#Consider creating a new variable:
HholdIncomePerPerson=Sfacts$INC110213/Sfacts$HSD310213


####------------------ DON'T DO THIS PART YET
### 2. Education (e.g. "Registered voters with a college degree favored Clinton)
Education = Sfacts[,names(Sfacts) %in% c("fips","area_name","state_abbreviation","EDU635213", "EDU685213")]
Education= Education[!(Education$state == ""), ]

EducationTotal= merge(Education,EResultsAbridged, by = "fips")
EducationTotal=cbind(EducationTotal,trumpvictory=0)
EducationTotal$trumpvictory[EducationTotal$gopvote>=EducationTotal$demvote]=1
EducationTotal$trumpvictory[EducationTotal$trump==1]="Republican"
EducationTotal$trumpvictory[EducationTotal$trump==0]="Democrat"

aggregate(cbind(EDU635213,EDU685213)~trumpvictory,EducationTotal,mean)

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

#These two questions might be really difficult to answer with the info we have. 
# As far as I can tell, no info on marriage percentages, and surburban could be teased out but it isn't clear...

NE =Sfacts[Sfacts$state_abbreviation %in% c("ME","CT","MA","NH","RI","VT","NJ","NY","PA"),]
NEResults =Eresults[Eresults$state_abbr %in% c("ME","CT","MA","NH","RI","VT","NJ","NY","PA"), c("FIPS","county_name","state_abbr","votes_dem_2016","votes_gop_2016","total_votes_2016","per_dem_2016","per_gop_2016")]

droplevels(NE$area_name)
droplevels(NE$state_abbreviation)
droplevels(NEResults$state_abbr)

colnames(NEResults) = c("fips","county","state","demvote","gopvote","totalvote","%dem2016","%gop2016")

NETotal = merge(NE,NEResults, by = "fips")


NETotal=cbind(NETotal,trumpvictory=0) #creates a new column to determine whether Trump won or lost each county
NETotal$trumpvictory[NETotal$gopvote>NETotal$demvote]=1 # if column value = 1, trump won the county, gopvotes>demvotes
table(NETotal$trumpvictory)

MW = Sfacts[Sfacts$state_abbreviation %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD"),] 
MWResults = Eresults[Eresults$state_abbr %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD"),c("FIPS","county_name","state_abbr","votes_dem_2016","votes_gop_2016","total_votes_2016","per_dem_2016","per_gop_2016")]    

colnames(MWResults) = c("fips","county","state","demvote","gopvote","totalvote","%dem2016","%gop2016")
MWTotal = merge(MW,MWResults, by = "fips")

MWTotal= cbind(MWTotal,trumpvictory=0) #creates a new column to determine whether Trump won or lost each county
MWTotal$trumpvictory[NETotal$gopvote>NETotal$demvote]=1  # if column value = 1, trump won the county, gopvotes>demvotes
table(MWTotal$trumpvictory)

droplevels(MW$area_name)
droplevels(MW$state_abbreviation)
droplevels(MWResults$state_abbr)

SO = Sfacts[Sfacts$state_abbreviation %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL",
                                             "KY","MS","TN","AR","LA","OK","TX"),]
SOResults = Eresults[Eresults$state_abbr %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX"),
                     c("FIPS","county_name","state_abbr","votes_dem_2016","votes_gop_2016","total_votes_2016","per_dem_2016","per_gop_2016")]
colnames(SOResults) = c("fips","county","state","demvote","gopvote","totalvote","%dem2016","%gop2016")
SOTotal= merge(SO,SOResults, by = "fips")

droplevels(SO$area_name)
droplevels(SO$state_abbreviation)
droplevels(SOResults$state_abbr)

WE = Sfacts[Sfacts$state_abbreviation %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA"),]
WEResults = Eresults[Eresults$state_abbr %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA"),c("FIPS","county_name","state_abbr","votes_dem_2016","votes_gop_2016","total_votes_2016","per_dem_2016","per_gop_2016")]
colnames(WEResults) = c("fips","county","state","demvote","gopvote","totalvote","%dem2016","%gop2016")
WETotal= merge(WE,WEResults, by = "fips")


droplevels(WE$area_name)
droplevels(WE$state_abbreviation)
droplevels(WEResults$state_abbr)
