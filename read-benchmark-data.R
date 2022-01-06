# read benchmark data

#install.packages("openxlsx")
#library(openxlsx)

setwd("/data/git/DecisonTreeExplorer")

#r <- read.xlsx('./data/Aedes-benchmark-2021-individuele-resultaten-corporaties.xlsx')

library(readxl)

library(janitor)
library(dplyr)

Aedes_benchmark_2021 <- read_excel("data/Aedes-benchmark-2021-individuele-resultaten-corporaties.xlsx", sheet = "Benchmark 2021")
# kolom 5 leeg
Aedes_benchmark_2021 <- rename(Aedes_benchmark_2021, 'Grootte'='...5') 


Aedes_benchmark_2020 <- read_excel("data/Aedes-Benchmark 2020 Individuele resultaten corporaties.xlsx", sheet = "Benchmark 2020")
Aedes_benchmark_2020 <-rename(Aedes_benchmark_2020,corporatie=naam)

Aedes_benchmark_2019 <- read_excel("data/Aedes-benchmark 2019_Individuele resultaten woningcorporaties.xlsx", sheet = "Blad2")
# kolom 1 en 2 leeg
Aedes_benchmark_2019 <- rename(Aedes_benchmark_2019, 'lnummer'='...1') 
Aedes_benchmark_2019 <- rename(Aedes_benchmark_2019, 'corporatie'='...2') 

Aedes_benchmark_2018 <- read_excel("data/Aedes-benchmark 2018_Individuele resultaten woningcorporaties.xlsx", sheet = "Aedes benchmark 2018")

Aedes_benchmark_2018 <- rename(Aedes_benchmark_2018, 'corporatie'='...2') 

Aedes_benchmark_2017 <- read_excel("data/Individuele resultaten corporaties_Aedes-benchmark 2017.xlsx", sheet = "Aedes-Benchmark 2017")

Aedes_Benchmark_2016 <- read_excel("data/Individuele positionering corporaties - Aedes Benchmark 2016.xlsx", sheet = "Aedes Benchmark 2016 Open data")


# clean rownames 
b.2021 <- clean_names(Aedes_benchmark_2021)
b.2020 <- clean_names(Aedes_benchmark_2020)
b.2019 <- clean_names(Aedes_benchmark_2019)
b.2018 <- clean_names(Aedes_benchmark_2018)

# 2016 and 2017 to far off

# letters eruit
n.2021 <- select(b.2021, -contains(c('letter','lettter')))
n.2020 <- select(b.2020, -contains(c('letter','lettter')))
n.2019 <- select(b.2019, -contains(c('letter','lettter')))
n.2018 <- select(b.2018, -contains(c('letter','lettter')))

# jaar erbij
n.2021$jaar <- 2021
n.2020$jaar <- 2020
n.2019$jaar <- 2019
n.2018$jaar <- 2018

# find joined colum names over the years
names(n.2021)
cols_intersection <- intersect(names(n.2021), names(n.2020))
cols_intersection <- intersect(cols_intersection, names(n.2019))
cols_intersection <- intersect(cols_intersection, names(n.2018))

write.csv(n.2021,file='bm2021.csv', row.names = FALSE)

# umap
library(umap)
library(Rtsne)
library(data.table) # set function

library(plotly) 

calcUmap <- function(df) {
  # keep numerics only
  df <- select_if(df,is.numeric)
  
  # impute
  for (k in colnames(df)) {
    i <- median(df[[k]],na.rm = T) # median
    set(x = df, which(is.na(df[[k]])), k, i)
  }
  
  return(umap(df))
}

all.data <- list(n.2018,n.2019,n.2020,n.2021)
years = 2018:2021

umap.all.years <- data.frame()

for (i in 1:4){
  # 
  df <- all.data[[i]]
  #labels <- df$corporatie
  lnummer <- df$lnummer
  
  df.umap <- calcUmap(df)
  layout <- df.umap[["layout"]] 
  layout <- data.frame(layout) 
  final <- cbind(layout, lnummer=lnummer, jaar = years[i])
  umap.all.years <- rbind(umap.all.years,final)
  
}

# tweede optie is om de umap toch over alle jaren te doen (stabieler resultaat)
# row bind all datasets
df.all <- dplyr::bind_rows(n.2018,n.2019,n.2020,n.2021)
summary(df.all)
# short cut remove NA 
# nog veel te winnen hier
## Remove columns with more than 50% NA
df.all <- df.all[, which(colMeans(!is.na(df.all)) > 0.8)]


umap.all <- calcUmap(df.all)
layout.all <- umap.all[["layout"]] 
layout.all <- data.frame(layout.all) 
final.all <- cbind(layout.all, lnummer=df.all$lnummer, jaar=df.all$jaar)


library(tidyverse)

# corporatie namen 2021 mergen met lnummers
df.cn <- n.2021 %>% select(lnummer,corporatie)
umap.all.years <- umap.all.years %>% 
  left_join(df.cn,by='lnummer') %>% 
  mutate_at(vars(corporatie),~replace_na(.,'geen lid in 2021'))

final.all <- final.all %>% 
  left_join(df.cn,by='lnummer') %>% 
  mutate_at(vars(corporatie),~replace_na(.,'geen lid in 2021'))


missingcorporatie2021 <- umap.all.years %>% filter(is.na(corporatie))
df.cn.20 <- n.2020 %>% select(lnummer,corporatie)
missingcorporatie2021 <- missingcorporatie2021 %>% left_join(df.cn.20,by='lnummer')

length(sort(unique(umap.all.years$lnummer)))

# layout <- df.umap[["layout"]] 
# layout <- data.frame(layout) 
# final <- cbind(layout, labels)

# # plotly
# fig <- plot_ly(final, x = ~X1, y = ~X2, color = labels, colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', mode = 'markers')%>%  
#   layout(
#     plot_bgcolor = "#e5ecf6",
#     legend=list(title=list(text='Corporaties')), 
#     xaxis = list( 
#       title = "0"),  
#     yaxis = list( 
#       title = "1")) 
# fig

library(gganimate)
library(ggrepel)

df.t <- umap.all.years %>% filter(jaar==2018)

# trivire, thuisvester, portaal, staedion en de alliantie
#  "Trivire" "Stichting Portaal" "Staedion" "Stichting de Alliantie"
tofollow <- c("Trivire","Stichting Portaal","Staedion","Stichting de Alliantie")

p <- ggplot(final.all, 
            aes(x=X1, y=X2, colour=lnummer)) +
  geom_text(data = final.all,aes(x=0,y=0, label=as.character(jaar)), size=50, colour='white',show.legend = FALSE) +
  geom_point(show.legend = FALSE, alpha= 0.7)  +
  geom_label(data = subset(final.all,corporatie %in% tofollow),
            aes(label=corporatie),
            show.legend = FALSE) + 
  labs(title = "AEDES Benchmark 2018 - 2021",
       subtitle = "UMAP embedding van alle(25+) detail Benchmark informatie in 2D",
                                        caption = "powered by DIKW Academy")
#  annotate(geom="text", x=-0.5, y=0, label=final.all$jaar, colour = 'grey', size=20, alpha = 0.4) 

p          
# p + 
#   transition_time(jaar) +
#   labs(title = "Year: {frame_time}")

a <- p + 
  transition_states(jaar, transition_length = 4, state_length=1, wrap=FALSE) +
  #labs(title = "Year: {closest_state}") +
#  enter_fade() +
  exit_fade()

animate(a, renderer = gifski_renderer("aedes-benchmark-2018-2021.gif"), rewind = F)
   

