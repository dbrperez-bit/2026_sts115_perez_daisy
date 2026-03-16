getwd()
setwd("/Users/")

# z=23
terns = read.csv("~/Desktop/2026-sts115-perez-daisy/data/2000-2023_ca_least_tern.csv")
#  check the class 
class(terns)

# look at the head 
head(terns)
tail(terns)
head(terns, 2)
#get dimensions
dim (terns)

names(terns)

#get structure
str(terns)
#look at years column
terns$year

range (terns$year)

summary(terns)

terns = read.csv("~/2026-sts115-perez-daisy/data/2000-2023_ca_least_tern.csv")
terns = read.csv("~/Desktop/2026-sts115-perez-daisy/data/2000-2023_ca_least_tern.csv")
str (terns)


length(terns$year)

terns$site_name

terns$site_name[1]
terns$site_name[256]
terns$year[15]
#make vectors 
terns$site_name[c(1,2,3)]
1:6
terns$year[200;2005]
terns$year[200:2005]
terns$year[200:205]

y = c (10, 20, 30)
y[1]
y[3]
[1]
terns$year[3]
y[-1]
y[2]

terns$year -1
y-1
y
z= c(3.5, 6.7, 2.1)
z
round(z)

class(terns$year)

mean(z)
sum(z)
unique(c(1,1,3,4))
class(5)

#recycling 
c(cla1,2)+ c(3,4)
c (1, 7, 8)* (4, -1, -2)


class (6i)
class(6.7)
class( 6 == 7)
class(TRUE)

str(terns)

c (TRUE, 10)
c (round, 10, "hi")
