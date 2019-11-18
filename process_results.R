#Some simple munging and plotting routines for handling the peer ranking data

require(readr)
require(tidyr)
require(dplyr)
require(ggplot2)
require(lattice)

#Load the data, merge the results with the master csv so we can get clustering/reciprocity values
df <- read_csv('./results/20180303T124221_summary.csv')
df_master <- read_csv('./data/ALL RESULTS TABULATED AND SUMMARIZED_MASTER FILE - DataResults.csv')
df_master <- unique(df_master[, c('GraphName_ISR', 'GraphName_ICIS', 'Recip', 'Clust_C', 'ClustPrime', 'ClustStar')])
df_merge <- merge(df, df_master, by.x='GraphName_', by.y='GraphName_ISR')

#Filter down the dataframe so we are looking at one metric/stat/agg/sigma_2 value . . . can change this however
df_sub <- df_merge %>% filter(Distrib=='tn') %>% filter(a==100) %>% 
          filter(sigma_2==15) %>% filter(CorrStat=='kt') %>% filter(Aggregat=='mean')

#Do some plots looking at the various clustering and reciprocity values
clustc.mean <- ggplot(df_sub, aes(Clust_C, mean)) + geom_point() + stat_smooth(method='lm') + facet_grid(N~K)
clustp.mean <- ggplot(df_sub, aes(ClustPrime, mean)) + geom_point() + stat_smooth(method='lm') + facet_grid(N~K)
clusts.mean <- ggplot(df_sub, aes(ClustStar, mean)) + geom_point() + stat_smooth(method='lm') + facet_grid(N~K)
recip.mean <- ggplot(df_sub, aes(Recip, mean)) + geom_point() + stat_smooth(method='lm') + facet_grid(N~K)

#Let's try a lattice plot with ClustStar and Recip vs KT mean
df_lattice <- df_sub %>% group_by(ClustStar, Recip) %>% summarise(KT = mean(mean), num_examples=n())
df_lattice <- df_lattice[df_lattice$ClustStar<=1, ]
wireframe(KT ~ ClustStar*Recip, data = df_lattice, xlab = "ClustStar", ylab = "Reciprocity",
          main = "KT vs Recip & ClustStar", drape = TRUE, colorkey = TRUE)

#Surface version with interpolation
require(akima)
z <- interp(df_lattice$ClustStar, df_lattice$Recip, df_lattice$KT) 
persp3D(z$x, z$y, z$z, xlab='ClustStar', ylab='Reciprocity', zlab='KT')

#Scatterplot version (easy)
require(plot3D)
scatter3D(df_lattice$ClustStar, df_lattice$Recip, df_lattice$KT, xlab='ClustStar', ylab='Recip', zlab='KT')