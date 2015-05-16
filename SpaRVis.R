rm(list = ls())
library(ggplot2)
library(lubridate) # year, month & day
library(plyr)
library(scales) # date_format

Startguthaben <- 0

print("Bitte eine der 'Konto...CVS'-Dateien auswaehlen. Egal, welche. Allerdings muessen alle diese Dateien im gleichen Ordner liegen, nachdem Sie sie aus dem Onlinepostfachs Ihres Sparkassen-Girokontos heruntergeladen haben.")
setwd(dirname(file.choose()))

SK_CSVs_einlesen <- function(CVS_Namen) {
  SK_Dateien <- list.files(pattern = CVS_Namen)
  SK_Daten <- do.call("rbind",
                      lapply(SK_Dateien,
                             read.csv2,
                             stringsAsFactors = FALSE,
                             encoding = "latin1"
                             )
                      )
  return(SK_Daten)
}

# use summary file if available & create if not, instead of reading files individually
SK_Gesamtdatei <- "Konto_00000000-Auszug_0000_000.csv"
# try(data_SK_known <- read.csv2(SK_Gesamtdatei))
SK_roh_alt <- SK_CSVs_einlesen("Konto_[0-9]{8,9}-Auszug_[0-9]{4}_[0-9]{3}.CSV")
SK_roh_neu <- SK_CSVs_einlesen("Konto_[0-9]{8,9}-Auszug_[0-9]{4}_[0-9]{3}_csv.csv")
names(SK_roh_alt) <- names(SK_roh_neu)
SK_roh <- rbind(SK_roh_alt, SK_roh_neu)
write.csv2(SK_roh, SK_Gesamtdatei)
# ..fs = Gesamtdatei / files summarised

# mache Spalten nützlicher
SK_roh <- SK_roh[order(abs(SK_roh$Betrag), decreasing = TRUE),]
SK_roh$Valutadatum <- as.Date(SK_roh$Valutadatum, format = "%d.%m.%y")
SK_roh$Valutamonat <- months(SK_roh$Valutadatum, abbreviate = TRUE)
SK_roh$Valutajahr <- year(SK_roh$Valutadatum)
SK_roh$Valutatag <- day(SK_roh$Valutadatum)
SK_roh$Ausgabe <- as.numeric("")
SK_roh$Einnahme <- as.numeric("")

# spalte Beträge in Einkommen und Ausgaben
for(i in 1:length(SK_roh$Betrag)) {
  if(SK_roh$Betrag[i] < 0) SK_roh$Ausgabe[i] <- SK_roh$Betrag[i]
  else if(SK_roh$Betrag[i] > 0) SK_roh$Einnahme[i] <- SK_roh$Betrag[i]
}

# male alle Buchungen, evtl. mit Zoom zu kleineren Buchungen
Zoom <- 1 # Faktor, um die größten Einnahmen und Ausgaben von der y-Achse auszublenden
SKpr <- ggplot(SK_roh, aes(Valutadatum))
SKpr +
  geom_point(alpha = 0.5, aes(y = SK_roh$Einnahme, size = abs(SK_roh$Einnahme)), color = "blue") +
  geom_point(alpha = 0.5, aes(y = SK_roh$Ausgabe, size = abs(SK_roh$Ausgabe)), color = "red") +
  scale_x_date(breaks = "1 month", labels = date_format("\'%y %b")) +
  scale_y_continuous(limits = c(min(SK_roh$Betrag)/Zoom, max(SK_roh$Betrag)/Zoom)) +
  labs(x = "Valuta", y = "Betrag (EUR)", size = "Betrag (EUR)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30))
ggsave("SpaRVis-Buchungen.png")


# fasse nach Monaten zusammen & male Monatsbeträge

SK_Monatsdaten <- ddply(.data = SK_roh,
                        .variables = c("Valutajahr", "Valutamonat"),
                        .fun = summarise,
                        Monatsbetrag = sum(Betrag)
)
SK_Monatsdaten$Valutazeitraum <- paste(SK_Monatsdaten$Valutajahr,
                                       SK_Monatsdaten$Valutamonat,
                                       "01",
                                       sep = "-")
SK_Monatsdaten$Valutazeitraum <- as.Date(SK_Monatsdaten$Valutazeitraum, format = "%Y-%b-%d") # benötigt Tag, siehe http://stackoverflow.com/a/16402128
SK_Monatsdaten$Monatminus <- as.numeric("")
SK_Monatsdaten$Monatplus <- as.numeric("")

# Monatsbeträge aufteilen
for(i in 1:length(SK_Monatsdaten$Monatsbetrag)) {
  if(SK_Monatsdaten$Monatsbetrag[i] < 0) SK_Monatsdaten$Monatminus[i] <- SK_Monatsdaten$Monatsbetrag[i]
  else if(SK_Monatsdaten$Monatsbetrag[i] > 0) SK_Monatsdaten$Monatplus[i] <- SK_Monatsdaten$Monatsbetrag[i]
}

SK_Monatsdiagram <- ggplot(SK_Monatsdaten, aes(Valutazeitraum))
SK_Monatsdiagram +
  geom_point(alpha = 0.5, aes(y = SK_Monatsdaten$Monatplus, size = abs(SK_Monatsdaten$Monatplus)), color = "blue") +
  geom_point(alpha = 0.5, aes(y = SK_Monatsdaten$Monatminus, size = abs(SK_Monatsdaten$Monatminus)), color = "red") +
  scale_x_date(breaks = "1 month", labels = date_format("\'%y %b")) +
  labs(x = "Valutamonat", y = "Betrag (EUR)", size = "Betrag (EUR)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30))
ggsave("SpaRVis-Monatsbeträge.png")


# addiere Start- zu Monatsguthaben, berechne -entwicklung & male

SK_Monatsdaten$Monatsguthaben <- as.numeric("")
SK_Monatsdaten$Monatsguthaben[1] <- SK_Monatsdaten$Monatsbetrag[1] + Startguthaben[1]
for(i in 2:length(SK_Monatsdaten$Monatsguthaben)) {
  SK_Monatsdaten$Monatsguthaben[i] <- SK_Monatsdaten$Monatsguthaben [i-1]+ SK_Monatsdaten$Monatsbetrag[i]
}

SK_Guthaben <- ggplot(SK_Monatsdaten, aes(Valutazeitraum, Monatsguthaben, size = Monatsguthaben))
SK_Guthaben +
  geom_point(color = "#ED8C3B") + # Flattr orange
  stat_smooth(color = "#ED8C3B") +
  scale_x_date(breaks = "1 month", labels = date_format("\'%y %b")) +
  labs(x = NULL, y = "Guthaben (EUR)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30),
        legend.position = "none")
ggsave("SpaRVis-Guthaben.png")
