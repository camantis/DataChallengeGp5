
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
# Areas with low density were more likely to vote for trump-Bubble (population/land area)
# Recent vets were more likely to vote for Clinton-vets

#Questions for Deb
  #Making assumptions of what these things mean. Such as #"Better off" means income or house"
  #How much resolution do we need to answer the question? Can we compare states that trump won or go down to counties where he won?
  #Delete rows in Sfacts where state_abbreviation is null
  #Can bring in new data using FIPS
  #Look at census data



#Median household income, percent below poverty, per capita money income
camhsincome = Sfacts[,names(Sfacts) %in% c("fips","area_name"
                                      ,"state_abbreviation",
                                      "INC110213","INC910213",
"PVY020213")]

camhsincome

#Let's try and change these names
colnames(camhsincome)<-c("fips","county","state","median household income","per capita money income","percent below
                         poverty level")


#Let's remove all the total-state rows (i.e., rows where State is blank)

income <- camhsincome[!(camhsincome$state == ""), ] 




income

#Now we need to merge ze data with election results

electdata = subset(Eresults, 
                    select=c("FIPS","county_name","state_abbr","votes_dem_2016",
                             "votes_gop_2016","per_dem_2016","per_gop_2016"))

colnames(electdata) = c("fips","county","state","demvote","gopvote","percentdem","percgop")

fulldata = merge(electdata,income) #electdata has two more observations than income

fulldata #somehow it merges but now there are only 3110 observations...


#Work on Income




#Merging data sets
#Create new variables
#Presentation



