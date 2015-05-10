### SpaRVis: Visualisierung Ihres Sparkassen-Girokontos

Dieses R-Skript fast die monatlichen Kontoauszugs-CSV-Dateien der Sparkasse zusammen und visualiert die Beträge der Buchungen.

##### Anleitung

1. Bei Ihrer Sparkasse einloggen und im `Postfach` unter `Funktionen` des `Giro...`-Kontos `Dokumentenübersicht aufrufen` anklicken.
1. Jeden Kontoauszug via `Funktionen` > `Export` (Diskettensymbol) im `Format: CSV...` in ein und den selben Ordner herunterladen.
1. Dieses Skript herunterladen (Ordner ist egal) und in [RStudio](http://www.rstudio.com/products/rstudio/download/) und [R](http://cran.rstudio.com/) öffnen & ausführen. Diagramme werden als `SpaRVis...png`-Dateien im selben Ordner erstellt. 
1. Optional: Startguthaben im ersten verfügbaren Kontoauszug nachgucken und in Zeile 7 des Skriptes eintragen. Falls nicht alle .csv's bis zurück zur Kontoeröffnung vorliegen, ist ansonsten das Guthaben-Diagramm verschoben.

Getestet mit .csv's der Sparkasse Bodensee in RStudio 0.98.1103, auf Mac OS X 10.9.5.
