rm(list=ls())
setwd("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/sharp")


output <- "Crosstabs Sharp.xlsx"

file <- "HBO Sharp Objects S1 POST BRA datamap_319554.xlsx"

source("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/Read eCInstant.R", local = TRUE, encoding = "utf-8")
d1 <-  d
qn1 <- qn

file <- "HBO Sharp Objects S1 POST LATAM datamap_319562.xlsx"
source("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/Read eCInstant.R", local = TRUE, encoding = "utf-8")


if(all(colnames(d1) %in% colnames(d)) &
   all(colnames(d) %in% colnames(d1)) &
   ncol(d1) == ncol(d)){
  d <- rbind(d1, d) ; rm(d1) # Leave only d
  rm(qn1) # Leave only qn
} else { stop("Column must match between both data.tables")}



radio <- c("radio", "select", "single", "factor")
radios <- names(qn)[sapply(qn, function(x){x$Type}) %in% radio]
for (r in radios){
  d[,(r) := factor(d[,r, with = FALSE][[1]],
                   levels = qn[r][[1]]$Answer.ID,
                   labels = qn[r][[1]]$Levels)]
}
rm(radios, r, radio)



source("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/HBO Series.R", local = TRUE, encoding = "utf-8")
source("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/Process R.R", local = TRUE, encoding = "utf-8")
source("\\\\fireblade/gabinete/PERFIL/petala.tuy/Desktop/Petala_files/ecinstant/Write R Excel.R",local = TRUE, encoding = "utf-8")
