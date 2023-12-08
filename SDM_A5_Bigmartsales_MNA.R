
#Here the data is multilevel data
#outlet data is upper level
#items lower level
#l1: data - Outlets (Outlet_ID, Outlet_Type, Outlet_age, etc)
#L2: data- Items (Item_ID,Item_Visibility, Item_MRP, Item_Desc)

rm(list=ls())

library("readxl")
bd =read_excel("BigMartSales.xlsx")
str(bd)

colnames(bd)=tolower(make.names(colnames(bd)))
attach(bd)

#converting columns to factors

bd$item_id=factor(bd$item_id)
bd$item_fat_content=factor(bd$item_fat_content)
bd$item_type=factor(bd$item_type)
bd$outlet_id=factor(bd$outlet_id)
bd$outlet_size=factor(bd$outlet_size)
bd$city_type=factor(bd$city_type)
bd$outlet_type=factor(bd$outlet_type)


str(bd)
#let us find if any null values 


colSums(is.na(bd))                                                
# Missing values
# 1463 null values for Item-Weight
# 2410 missing values under Outlet_Size
#outlet_size is null for "out10"
#outlet size is same for same outlet_Id's and hence having one of the columns is better

#bd$outlet_size = ifelse(is.na(bd$outlet_size) & bd$city_type=="Tier 2", "Small", ifelse(is.na(bd$outlet_size) & bd$city_type=="Tier 3","Medium",bd$outlet_size))


hist(bd$item_sales)
hist(log(bd$item_sales))

#log transform of sales is normally distributed
#let us see bivariate analysis on city type and outlet type


library(lattice)

bwplot(item_sales ~ outlet_type, data=bd) #q1
bwplot(item_sales ~ city_type, data=bd) #q2
bwplot(item_sales ~ outlet_id, data=bd) #q3



#autocorrelation test for item variables
cr_item = bd[, c(2, 4, 6, 12)]

str(cr_item)
library(PerformanceAnalytics)
chart.Correlation(cr_item) 
#we have year coulm and let us create new factor called outlet_age
min(bd$outlet_year)
max(bd$outlet_year)
#the minimum year is 1985 and max is 2009
#let us  create age variable staring from 1984 to 2009
bd$outlet_age = 2009 -bd$outlet_year
cr_d = bd[, c(2, 4, 6, 12, 13)]
chart.Correlation(cr_d) 
bd$outlet_type <- relevel(bd$outlet_type, ref="Grocery Store") 
# Models
str(bd)
#fat content will be correlated with item type, let us drop it from the model
#to answer the questions, let us create models with the respective factors in place 
m1 <- lm(log(item_sales) ~item_mrp+item_type+item_visibility+outlet_type+city_type+outlet_age , data=bd)       

summary(m1)
levels(bd$outlet_type)
#add the fixed effect (level 1) column for the model
fe <- lm(log(item_sales) ~ item_mrp+item_type+item_visibility+outlet_type+city_type+ outlet_id +outlet_age , data=bd)  
summary(fe)

library(lme4)             
re <- lmer(log(item_sales) ~item_mrp+item_type+item_visibility+outlet_type+city_type+outlet_age+ (1|outlet_id), data=bd, REML=FALSE)   

library(stargazer)
options(max.print=10000)
stargazer(m1, fe, re, type="text", single.row=TRUE)

ranef(re)
AIC(m1, fe, re)



library(car)
vif(re)

plot(re)
plot(fe)

