#SECTION 1 Label Encoding remaining variables
all$Street <-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
all$PavedDrive <-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
all$LandSlope <-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
all$CentralAir <-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))

#SECTION 2 Changing some numeric variables into factors
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(ys, ms, widths=c(1,2))
#Month Sold and MSSubClass are nominal variables so i'm turning them into factors
all$MoSold <- as.factor(all$MoSold)

all$MSSubClass <- as.factor(all$MSSubClass)

#revalue for better readability
all$MSSubClass<- revalue(all$MSSubClass, 
                        c('20'='1 story 1946+', '30'='1 story 1945-', 
                          '40'='1 story unf attic', '45'='1,5 story unf', 
                          '50'='1,5 story fin', '60'='2 story 1946+', 
                          '70'='2 story 1945-', '75'='2,5 story all ages', 
                          '80'='split/multi level', '85'='split foyer', '
                          90'='duplex all style/age', '120'='1 story PUD 1946+', 
                          '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                          '180'='PUD multilevel', '190'='2 family conversion'))
table(all$MSSubClass)
