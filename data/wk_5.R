cd 2026_sts115_perez_daisy
install.packages("ggplot2")
library(read.csv)
library(ggplot2)
terns = read.csv("desktop/data/2000-2023_ca_least_tern.csv") 
X2000_2023_ca_least_tern = terns
assign ("terns", X2000_2023_ca_least_tern)

#ggplot layers include data, aesthteics, and geometery, scales themes 

ggplot(data = terns)

#add a geometrey, this gives error 
ggplot(data = terns) + geom_point

#add an aesthetic mapping 
ggplot(data = terns) + aes(x = total_nests, y = bp_min) + 
  geom_point()

#add color for climate period 
ggplot (data=terns)+
  aes(x= total_nests, y = bp_min, color = event) +
  geom_point()

#add shape and color to both indicate event: 
ggplot(data = terns) + 
  aes(x = total_nests, y = bp_min, color = event, shape = event) +
  geom_point()

#add a title and axis label 
ggplot(terns)+
  aes(x= total_nests, y = bp_min, color =
        event, shape = event) +
  geom_point()+
  labs( title ="Breeding pairs compared to total number of nests for California Least Terns",
       subtitle = "Data from California Department of Fish and Wildlife, 2000-2023", 
       x = "total observed nets",
       y = "lower bound estimate of breeding pairs",
       color = "climate period 
       shape = climate period")
#change labels by modyfying the data 
terns$climate_period = terns$event
terns$climate_period[terns$climate_period == "EL_NINO"] = "El Nino"

#mixed up geometrey and aesthetic 
ggplot(terns) + 
  aes(x= total_nests, y = bp_min) + 
  geom_point(color= "pink")

ggplot(terns)+
aes(x= total_nests, y = bp_min, color = "pink") + 
  geom_point()

# show year as a numeric color (my fav)
ggplot(terns) + 
  aes(x= total_nests, y = bp_min, color = year) +
  geom_point()

# show year as a categorical color
ggplot(terns) + 
  aes(x= total_nests, y = bp_min, color = factor(year))+
  geom_point() +
  scale_colour_viridis_d()

# save a plot to disk 
my_plot <- ggplot(terns)+
  aes(x= total_nests, y = bp_min, color =
        event, shape = event) +
  geom_point()+
  labs( title ="Breeding pairs compared to total number of nests for California Least Terns",
        subtitle = "Data from California Department of Fish and Wildlife, 2000-2023", 
        x = "total observed nets",
        y = "lower bound estimate of breeding pairs",
        color = "climate period 
       shape = climate period")
ggsave(my_plot, filename = "my_plot.png")

# bar chart!
ggplot(data = terns) + 
  aes(x = region_3, weight = fl_min) +
  geom_bar()

# take out Arizona, Kings, and sac regions 

t2= terns[terns$region_3 %in% c("CENTRAL","S.F._BAY", "SOUTHERN"),]

#barchart with only three regions 
ggplot(data = t2) + 
  aes(x = region_3, weight = fl_min) + 
  geom_bar()

ggplot(t2) + 
  aes(x=year, weight=fl_min, fill=region_3) +
  geom_bar

