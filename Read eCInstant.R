# if(!exists('file')){
# 	# Check if the variable "file" exists:
# 	stop("Excel 'file' is not defined. Define 'file' as the Excel file path")
# }
# 
# # eCInstant Data Reading ->

# # Read Excel File:
# readxl.sheet<-function(file, sheet, skip = 0){
#   sheets <- readxl::excel_sheets(file)
#   if(!sheet %in% sheets){ stop(paste0(sheet, "sheet is not in ", file))}
#   sheet.num <- match(sheet, sheets) - 1
#   num.columns <- length(readxl:::xlsx_col_types(file, sheet = sheet.num, nskip = 0, n = 1))
#   readxl::read_excel(file, sheet = sheet, col_types = rep("text", num.columns), skip = skip)
# }
# 
# d <- as.data.table(readxl.sheet(file, sheet = "Data"))
d <- as.data.table(read_excel(file,sheet = "Data"))
# dmp <- as.data.table(readxl.sheet(file, sheet = "Datamap", skip = 10))
dmp <- as.data.table(read_excel(file,sheet = "Datamap", skip = 10))
colnames(dmp) = c("Question.ID", "Question.Code","Answer.ID","Answer.Code", "Type", "Question.Label", "Answer.Label")
rm( file) # Delete the function now that it's used.

# eCInstant Data Cleaning ->

multi = c("multi", "checkbox")
radio = c("radio", "select", "single", "factor", "ranking")

qin = "q"
qout = "Q"

# Change column names:
logic <- dmp$Type %in% multi
if(any(logic)){
	old <- c(paste0(qin, dmp[logic, Question.Code], "_", dmp[logic, Answer.Code]), 
			paste0(qin, unique(dmp[!logic, Question.Code])))
	new <- c(paste0(qout, dmp[logic, Question.ID], "_", dmp[logic, Answer.ID]), 
			paste0(qout, unique(dmp[!logic, Question.ID])))
} else{
	old <- paste0(qin, unique(dmp[!logic, Question.Code]))
	new <- paste0(qout, unique(dmp[!logic, Question.ID]))
}

if(any(duplicated(old)) | any(duplicated(new))){
  print(new[which(duplicated(new))])
	stop(paste0("There are Answer Codes repeated inside ", file, " in Datamap. Not possible to match with column names.",new[which(duplicated(new))]))
} else if(length(old) == length(new)){
	setnames(d, old = old, new = new)
} else {
	print("\tVariable names and length are different. \n\tCheck dmp for NON-multiple-choice questions. \n\tIn case of using ConfirmIt check if variable names are equals between both columns")
} 
rm(old, new, logic)

# Define radios as factors:
radios <- unique(dmp[dmp$Type %in% radio, Question.ID])
for (r in radios){
	name <- paste0("Q", r)
	d[,(name) := factor(d[,name, with = FALSE][[1]],
	       levels = dmp[dmp$Question.ID== r,Answer.Code], 
	       labels = dmp[dmp$Question.ID== r,Answer.ID])]
}
rm(radios, name, r)

# Set initial "qn" variable for processing.
dmp$Question.ID <- paste0(qout, dmp$Question.ID)
qnames <- unique(dmp$Question.ID) # Original order. The setkey command changes the order.
qs <- dmp[, .N, by = .(Question.ID, Type, Question.Label, Question.Code)]
setkey(dmp, Question.ID)
setkey(qs, Question.ID)
qn <- list()
for(qname in qnames){
	qtemp <- list()
	qtemp$Type <- qs[qname,Type]
	qtemp$Title <- qs[qname,Question.Label]
	# qtemp$Code <- qs[qname,Question.Code]
	qtemp$Levels <- dmp[qname, Answer.Label]
	qtemp$Answer.ID <- dmp[qname, Answer.ID]
	qtemp$Dummy.Names <- paste0(qname, "_" , dmp[qname, Answer.ID])
	qn[qname] <- list(qtemp)
}
rm(qtemp, dmp, qs, qnames, qname)
rm(qin, qout, multi, radio)
