#########################################################################################
library(data.table)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(formattable)
library(grid)
#########################################################################################
# read stations
stations <- fread('nomenclatura.csv')


# turns out, a bunch of months have bad dates
#2017 01, 02, 03, 04, 05
#2018 06

# force to date
datess <- c('2017_01', '2017_02', '2017_03', '2017_04', '2017_05', '2018_06')

# re read trips, keep as a list
trips <- lapply(as.list(list.files()[grepl('datos', list.files())]), fread)
names(trips) <- regmatches(list.files()[grepl('datos', list.files())],
                           regexpr('\\d{4}_\\d{2}', list.files()[grepl('datos', list.files())]))



to_date <- function(x) {
  if (is.na(x) | x=='') {print(x)
    return(NA)
  } else if (lubridate::year(x) < 2014) {
    return(as.Date(strptime(x, format = '%d/%m/%Y %H:%M')))
  } else if (lubridate::year(x) >= 2014) {
    return(as.Date(x, format = '%Y-%m-%d %H:%M:%S'))
  } else {
    return(NA)
  }
}


hm <- function(x) {
  if (is.na(x) | x=='') {
    return(NA)
  } else if(year(x) < 2014) {
    return(format(strptime(x, '%d/%m/%Y %H:%M'), '%H:%M'))
  } else if (year(x) >= 2014) {
    return(format(strptime(x, '%Y-%m-%d %H:%M:%S'), '%H:%M:%S'))
  } else {
    return(NA)
  }
}


#remove NA in trip ID
trips <- lapply(trips, function(x) x[!is.na(Viaje_Id)])

datesw <- c('2017_04', '2017_05')
# lapply
lapply(trips, function(x) {
    if (x %in% datesw) {
    x[, ts := as.Date(strptime(Inicio_del_viaje, format = '%d/%m/%Y %H:%M'))]
    x[, te := as.Date(strptime(Fin_del_viaje, format = '%d/%m/%Y %H:%M'))]
    x[, hs := format(strptime(Inicio_del_viaje, format = '%d/%m/%Y %H:%M'))]
    x[, he := format(strptime(Fin_del_viaje, format = '%d/%m/%Y %H:%M'))]
    
    # create variables
    x[, tripstart :=  base::as.POSIXct(paste0(ts, as.ITime(hs)), tz='America/Mexico_City')]
    x[, tripend :=  base::as.POSIXct(paste0(te, as.ITime(he)), tz='America/Mexico_City')]
  } else {
    x[, ts:= tryCatch(to_date(Inicio_del_viaje), error = function(x) return('SR'))]
    x[, te:= tryCatch(to_date(Fin_del_viaje), error = function(x) return('SR'))]
    x[, hs:= tryCatch(hm(Inicio_del_viaje), error = function(x) return('SR'))]
    x[, he:= tryCatch(hm(Fin_del_viaje), error = function(x) return('SR'))]
    
    # create variables
    x[, tripstart :=  base::as.POSIXct(paste0(ts, as.ITime(hs)), tz='America/Mexico_City')]
    x[, tripend :=  base::as.POSIXct(paste0(te, as.ITime(he)), tz='America/Mexico_City')]
  }

})


# bind
trips <- rbindlist(trips)

print(trips)
# day of the week
trips[, duration := tripend - tripstart]
trips[, daystart := strftime(tripstart,'%A')]
trips[, dayend := strftime(tripend,'%A')]
trips[, duration_time := format(.POSIXct(duration,tz="GMT"), "%H:%M:%S")]

# add station info and force lat long twice
trips <- merge(trips, stations[, .(id, latitude, longitude)], by.x = 'Origen_Id', by.y = 'id', all.x = TRUE)
setnames(trips, 'latitude', 'olat')
setnames(trips, 'longitude', 'olon')
trips <- merge(trips, stations[, .(id, latitude, longitude)], by.x = 'Origen_Id', by.y = 'id', all.x = TRUE)
setnames(trips, 'latitude', 'dlat')
setnames(trips, 'longitude', 'dlon')

#
trips[, .N, by = Genero]
# gender has bad encoding M, F H, '' <- potentially F but unsure, NA, 'NA'
trips <-trips[ Genero %in% c(NA, 'M', 'F', 'NA')]
trips[Genero == 'NA', Genero := NA]


# format year month start trip
trips[, yms := format(ts, '%Y-%m')]

# add yms date, always starts with 1
trips[, ymsd := as.Date(paste0(yms, '-01'))]

# plots
################ fix this plot
tripsa <- ggplot(trips, aes(ts)) + geom_bar() + scale_x_date(date_breaks = "1 month", date_labels = "%Y %B", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(#axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()
        ) +
  labs(x= element_blank(), y = element_blank())

## map info
library(ggmap)
api_key = 'api_key_here'

ggmap::register_google(key = api_key)


#oplot
al0 <- get_googlemap(center = c(-103.356, 20.6876), zoom = 13, maptype = 'satellite') # original
al0 <- ggmap(al0) + labs(x = 'Longitude', y = 'Latitude')


# pplot
al1 <- get_googlemap(center = c(-103.356, 20.6876), zoom = 13, maptype = 'roadmap') # plots

al1 <- ggmap(al1) + labs(x = 'Longitude', y = 'Latitude')

#coverage
coverage <- al1 + geom_point(data = stations,
                           aes(x = longitude, y = latitude), alpha = 1/2, stroke = 0, size = 2, colour = 'red') + 
  guides(size = guide_legend(paste0("n = ", dim(stations)[1]))) +
  theme(legend.position="bottom") + labs(x = 'Longitude', y = 'Latitude')



# what's the station usage
tabs <- stations[,.N, by = location][order(-N)]
setnames(tabs, 'N', 'total')

tabss <- kable_styling(kable(tabs, caption= 'Total stations per group'),
                      bootstrap_options = c("striped", "hover", "condensed", "responsive"))



#######
# animation of increased coverage
# unique lat/lon pairs by month, exclude pais that were already there...
library(gganimate)


# new stations per group
gd <- trips[, unique(Origen_Id), by = ymsd]
monthss <- unique(gd$ymsd)

nss <- list()
for (i in 1:length(monthss)) {
  if (i == 1) {
    # initialize list with all of the available stations at the start
    nss[[i]] <- unique(gd[ymsd == monthss[i]])
  } else {
    # previous stations + new stations - los stations
    current <- unique(gd[ymsd == monthss[i]]$V1)
    #print(current)
    old <- unique(gd[ymsd == monthss[i-1]]$V1)
    # lost stations
    lss <- setdiff(old, current)
    # new stations
    nsts <- setdiff(current, old)
    adds <- current[!(current %in% old)]
    if (length(adds) == 0) {
      nss[[i]] <- unique(gd[ymsd == monthss[i] & V1 %in% old])
    } else {
      tots <- setdiff(union(old, adds), lss)
      nss[[i]] <- unique(gd[ymsd == monthss[i] & V1 %in% tots])
    }
  }
}


nss <- rbindlist(nss)

nss <- merge(nss, stations[, .(id, latitude, longitude, location)], by.x = 'V1', by.y = 'id', all.x = TRUE)
setnames(nss, 'V1', 'id')

pl1 <- al1 + geom_point(data = nss,
                        aes(x = longitude, y = latitude, colour = location), 
                        size =2,
                        stroke = 0) +
  guides(size = guide_legend(paste0("n = ", dim(stations)[1]))) +
  theme(legend.position="bottom") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Longitude', y = 'Latitude') +
  transition_time(ymsd) +
  ease_aes('linear')

pl11 <- animate(pl1, nframes = 300, fps=3, end_pause = 10)


# but how is the usage among males and females? where are they leaving from?

pl2 <- al1 + geom_point(data = trips[, .N, by =c('Origen_Id', 'Genero', 'olat', 'olon', 'ts')],
                               aes(x = olon, y = olat, colour = Genero, size = N), 
                               #size =N,
                               stroke = 0) +
 # guides(size = guide_legend(paste0("n = ", dim(stations)[1]))) +
  theme(legend.position="bottom") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Longitude', y = 'Latitude') +
  transition_time(ts) +
  ease_aes('linear')

pl22 <- animate(pl2, nframes = 300, fps=3, end_pause = 10)


# but how is the usage among males and females? where are they going to?

pl3 <-al1 + geom_point(data = trips[, .N, by =c('Origen_Id', 'Genero', 'dlat', 'dlon', 'te')],
                               aes(x = dlon, y = dlat, colour = Genero, size = N), 
                               #size =N,
                               stroke = 0) +
  # guides(size = guide_legend(paste0("n = ", dim(stations)[1]))) +
  theme(legend.position="bottom") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Longitude', y = 'Latitude') +
  transition_time(te) +
  ease_aes('linear')

pl33 <- animate(pl3, nframes = 300, fps=3, end_pause = 10)

#######


# add groups
groups1 <- trips[, .N, by = c('Genero', 'olat', 'olon')]
# groups
totalusage <- al1  + geom_point(data = groups1,
                         aes(x = olon, y = olat, colour = Genero, size = N)) + 
  theme(legend.position="bottom") +
  labs( x = 'Longitude', y = 'Latitude')


#the 'round trip'
trips[Origen_Id == Destino_Id][,.N, by = daystart][order(-N)]


# longest usage
trips[order(-duration)]
trips[, dayss := floor(as.numeric(duration)/86400)]

# top 10 trips by time
tab0 <-  trips[, .(Usuario_Id, tripstart, tripend, duration, dayss)][order(-dayss)][1:10]
setnames(tab0, 'dayss', 'days')
setnames(tab0, 'Usuario_Id', 'user')
setnames(tab0, 'tripstart', 'start of trip')
setnames(tab0, 'tripend', 'end of trip')


tab1 <- kable_styling(kable(tab0, caption= '10 longest trips by duration'),
              bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# why do they use it for? days of the week?



# how many trips per station?
trips[,.N, by = c('ts', 'olat')][order(-N)]
# trips per day
trips[,.N, by = ts][order(-N )]


perg <- trips[,.N, by = Genero]
plotg <- ggplot(perg, aes(Genero, N, fill = Genero)) + geom_bar(stat='identity') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = 'none') +
  geom_text(aes(label=N), position=position_dodge(width=1), vjust = -1) + labs(x=element_blank())





trips[, durationi := as.numeric(duration)]
#boxplots/violinplots
library(gridExtra)
# need to add na.rm = TRUE in the geom, so it doesn'S display in knitr
box <- ggplot(trips, aes(Genero, durationi, color = Genero)) + geom_boxplot(na.rm = TRUE) + scale_y_continuous(limits = quantile(trips$durationi, c(0.1, 0.9))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = 'none') +
  labs(y = 'Trip duration in seconds (outliers removed)', x = element_blank())

violin <- ggplot(trips, aes(Genero, durationi, color = Genero, na.rm = TRUE)) + geom_violin(na.rm = TRUE) + scale_y_continuous(limits = quantile(trips$durationi, c(0.1, 0.9))) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = "none") +
  labs(y = element_blank(), x = element_blank())
bplots <- grid.arrange(box, violin, ncol = 2)




## week trips
dayplot <- trips[, .N, by = daystart][order(daystart)]
dayplot$daystart <- factor(dayplot$daystart, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
dayplots <- ggplot(dayplot[order(daystart)], aes(daystart, y = N, fill = N)) + 
  geom_bar(stat = "identity", fill = c("grey30", "grey30", "grey30", "grey30", "grey30", "red", "red")) +
  labs(x = element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = "none") +
  geom_text(aes(label=N), position=position_dodge(width=1), vjust = 2, colour = 'white') 

# round trips
trips[Origen_Id == Destino_Id & duration != 0]
