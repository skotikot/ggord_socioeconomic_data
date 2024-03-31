#Load required libraries
require(ggplot2)
require(reshape2)
library(dplyr) # for data manipulation
library(ggplot2) # for pretty plots
library(grid) # for envfit arrows on ordination plot

# # Enable the r-universe repo - then install the ggord package
# options(repos = c(
#     fawda123 = 'https://fawda123.r-universe.dev',
#     CRAN = 'https://cloud.r-project.org'))
# # Install ggord
# install.packages('ggord')
library(ggord) #for plotting ordination plots
library(readr)

#read in data
ordData <- read_csv("OrdinationData.csv")

rainInc1 <- ordData %>% 
  dplyr::select(any_of(c('rainC','rainV','income','yrs_income','moist','Gender','tenure','moist'))) %>% 
  mutate(yrs_income2 = if_else(yrs_income == "< 10", 1, NA),
         yrs_income2 = if_else(yrs_income=="11 - 20",2, yrs_income2),
         yrs_income2 = if_else(yrs_income=="21 - 30",3, yrs_income2),
         yrs_income2 = if_else(yrs_income=="31 - 40",4, yrs_income2),
         yrs_income2 = if_else(yrs_income=="> 40",5, yrs_income2))

rainInc1 <- na.omit(rainInc1)

rainInc <- rainInc1[,c('income', 'rainC','moist','yrs_income2')]
rainInc[] <- lapply(rainInc, function(x) as.numeric(as.factor(x)))

ord <- metaMDS(rainInc, distance = "bray", autotransform = F,na.rm=TRUE)
ordRot = MDSrotate(ord, rainInc1$income)

# add PCA vectors to plot as separate geom
datTest <- rainInc
datTest <- datTest %>% 
  mutate(
    one = ordRot$points[ ,1], 
    two = ordRot$points[ ,2])

# change the vector labels with vec_lab
new_lab <- list(income = 'Income.source', rainC = 'Perceptions', moist = 'Agroclimatic.zone', yrs_income2='Years.in.ocupation')

# make ggord, remove points layer
p=ggord(ordRot, rainInc1$rainC, obslab=F,grp_title='Perceptions on\nRainfall Changes', poly = FALSE,vec_lab=new_lab,
        sizelab=10, txt=4, xlims=c(-0.80,0.60),ylims=c(-0.60,0.60))
p$layers[[1]] <- NULL


# add points layer manually, redefine some aesthetics
rainAmt <- p + geom_point(data = datTest, aes(shape = rainInc1$income, colour = rainInc1$rainC), size= 4, alpha = 1) + 
  scale_shape_manual(values = c(16, 17,20)) +
  geom_text(x=-0.75, y=0.55, label="", fontsize=20, fontface='bold')+
  guides(fill = guide_legend(override.aes= list(shape = NA))) +
  theme(legend.title = element_blank())


rainAmt

ggsave("ordination plot of socio-economic data.tiff", units='px',width=5000, height=5000, dpi=500,compression='lzw')
