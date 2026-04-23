
################################################################################
## LiDAR Datenverarbeitung in R

# Es gibt zwei packages mit denen wir LiDAR Daten in R verarbeiten können: 
# "lidR" und "lasR". lasR basiert auf den Funktionen von lidR und baut 
# diese in gut zu kombinierenden pipelines ein. Für die meisten Anwendungen ist 
# lasR effizienter, manchmal bietet lidR aber zum Beispiel genauere Rechenmethoden
# an. Wir fokussieren uns hier vor allem auf das lasR Paket.

# Zu beiden Paketen gibt es gute Tutorials im Internet:
  
#  -   https://r-lidar.github.io/lidRbook/index.html
  
#  -   https://r-lidar.github.io/lasR/articles/tutorial.html
  
  
# lasR ist nicht auf CRAN verfügbar und muss daher extern installiert werden, 
# lidR können wir allerdings wie gewohnt installieren

#install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
library(lasR)
#install.packages("lidR")
library(lidR)

# für weitere Verarbeitungsschritte und Analysen laden wir auch die Pakete terra und sf ein
library(terra)
library(sf)

### 
data <- read.csv2("data/metadata.csv")

################################################################################

### Wir fangen zur kurzen visualisierung mit Funktionen aus lidR an
### Wir können las Dateien direkt in R einladen mit der Funktion readLAS

las <- readLAS("data/driedorf_1.las")
las
str(las)

### Da unsere Testdatei relativ groß ist reduzieren wir die Auflösung bevor wir es plotten
reduced <- decimate_points(las, random(20))
reduced

### Außerdem schneiden wir die plots auf einen Radius von 60 metern zu
clipped <- clip_circle(reduced, data$Lon[1], data$Lat[1], 60)

### die neue Punktwolke können wir dann einfach darstellen
plot(clipped, color = "RGB")

### um später nicht so lange rechenzeiten zu haben schreiben wir die reduzierte Punktwolke aus
writeLAS(clipped,"data/driedorf_1_reduced.las")


### oder direkt die reduzierte Version einladen:
las_reduced <- readLAS("data/driedorf_1_reduced.las")
plot(las_reduced, color = "RGB")


################################################################################

# Um Daten mit lasR zu verarbeiten müssen diese nicht wie gewohnt in R eingeladen 
# werden.Stattdessen wird auf einen Ordner verwiesen in dem eine oder mehrere 
# Dateien liegen und direkt verarbeitet werden. Zur Verarbeitung werden sogenannte 
# "pipelines" erstellt die, wenn gewünscht, alle Dateien gebündelt verarbeitet 
# werden können. Zum ausprobieren verwenden wir allerdings nur zwei Dateien, 
# da es sonst zu langen Rechenzeiten kommen kann.


### Wir erstellen einen Dateipfad zu allen .las files in unserem Ordner, um
### gebündelt darauf zugreifen zu können.
pfad <- list.files("data/", pattern = ".las", full.names = TRUE, recursive = TRUE)
pfad

f <- pfad[2]


### basic functions in lasR:

### Triangulation
### Normalerweise ist immer der erste Schritt eine Triangulation der Daten,
### dabei können verschiedene Algorithmen angewendet werden um eine Art Netz
### aus der Punktwolke zu erstellen. Das ist nötig um zB ein Geländemodell zu
### berechnen


pipeline = reader() + triangulate(filter = keep_ground(), ofile = tempgpkg())
ans = exec(pipeline, on = f)
ans

### allerdings kommt hier nicht wirklich etwas sinnvolles bei raus mit dem wir 
### weiter arbeiten können.


### Normalisierung der Punktwolke: 

### lasR speichert ja selber keine Daten in R, daher müssen wir bestimmte
### zwischenschritte immer aktiv ausschreiben. das geht mit der write_las
### Funktion. Im nächsten Versuch führen wir wieder eine Triangulation durch,
### schreiben aber vorher einmal alle als Boden klassifizierte Punkte aus sowie
### nachher die normalisierte Punktwolke

write1 = write_las(paste0("data/*_ground.las"), filter = keep_ground())
write2 = write_las(paste0("data/*_normalized.las"), )
del = triangulate(filter = keep_ground())
norm = transform_with(del, "-")
pipeline =  write1 + del + norm + write2
ans = exec(pipeline, on = f)
ans



### hier können wir dann die Dateien wieder händisch einladen um uns sie anzugucken
pfad <- list.files("data/", pattern = ".las", full.names = TRUE, recursive = TRUE)

ground <- readLAS(pfad[3])
plot(ground, color = "RGB")

normalise <- readLAS(pfad[4])
plot(normalise, color = "RGB")


################################################################################


### wir können uns aber auch raster Daten ausgeben lassen. Dafür gibt es die 
### Funktion rasterize() mit der wir uns hier ein Oberflächenmodell erstellen

del = triangulate(filter = keep_ground())
dtm = lasR::rasterize(res = 1)
pipeline = del + dtm
ans = exec(pipeline, on = f)
plot(ans)






################################################################################
################################################################################
#### Was können wir denn dann mit den Daten anstellen?

### Lokale Maxima und Erkennung von Baumkronen
### hier wird es jetzt ein bisschen komplizierter. Zuerst erstellen wir aus 
### unserem normalisierten .las file ein Raster und trennen mithilfe von 
### local_maximum_raster() und region_growing() die Bäume voneinander ab 
### einmal bekommen wir dadurch ein Ergebnis, wie viele Bäume wir in unserem
### Gebiet haben und andererseits können wir zB den Anteil bedeckter Fläche
### berechnen

del = triangulate(filter = keep_first())
chm = lasR::rasterize(0.5, del)
chm2 = pit_fill(chm)
seed = local_maximum_raster(chm2, min_height = 3, ws = 7)
tree = region_growing(chm2, seed)
pipeline = del + chm + chm2 +  seed + tree
ans = exec(pipeline, on = pfad[4])

col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))(25)
col2 = grDevices::colorRampPalette(c("purple", "blue", "cyan2", "yellow", "red", "green"))(50)
terra::plot(ans$rasterize, col = col, mar = c(1, 1, 1, 3))
terra::plot(ans$pit_fill, col = col, mar = c(1, 1, 1, 3))
terra::plot(ans$region_growing, col = col2[sample.int(50, 277, TRUE)], mar = c(1, 1, 1, 3))
plot(ans$local_maximum$geom, add = T, pch = 19, cex = 0.5)

ans$local_maximum



################################################################################
### Parameter zur Höhenstruktur der Punktwolke
### Wir können auch "normale" Berechnungen mit der Punktwolke durchführen
### und zum Beispiel die Maximal oder durchschnittliche Höhe der Punktwolke
### berechnen

### Als erstes müssen wir dazu Funktionen definieren:
meanz = function(data){ return(mean(data$Z)) }
sdz = function(data){ return(sd(data$Z)) }
maxz = function(data){ return(max(data$Z)) }


### dann können wir die Funktionen auf unsere Punktwolke anwenden

### Mittelwert der Höhe
call = callback(meanz, expose = "xyz")
pipeline <- call 
ans <- exec(pipeline, on = pfad[4])    
ans 

### Standardabweichung der Höhe
call = callback(sdz, expose = "xyz")
pipeline <- call 
ans <- exec(pipeline, on = pfad[4])    
ans                     

### Maximale Höhe
call = callback(maxz, expose = "xyz")
pipeline <- call 
ans <- exec(pipeline, on = pfad[4])    
ans                     



