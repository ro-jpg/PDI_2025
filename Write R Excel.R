#############################################################
#### ------ DATA VISUALIZATION--- ###########################
#############################################################

setTableFormat <- function(sr, sc, start, prop, wb, sheet, baseName, test, base, gn){
	row = sr ; col = sc
	# ROW NAMES
	setCellStyle(wb, sheet = sheet, row= row, col = col, cellstyle = ftitle1)
	setCellStyle(wb, sheet = sheet, row= row+1, col = col, cellstyle = fbase1)
	setCellStyle(wb, sheet = sheet, row= row+2, col = col, cellstyle = fanswer2)
	for(i in 1:nrow(test)){
		setCellStyle(wb, sheet = sheet, row = row+1+i*2, col = col, cellstyle = fbase1)
		setCellStyle(wb, sheet = sheet, row = row+2+i*2, col = col, cellstyle = fanswer2)
	}
	# TOTAL & DEMOGRAPHS
	col = sc + 1
	gn = cbind(1, gn) # Include total
	for(dem in 1:length(gn)){
		for(level in 1:gn[dem]){
			if(level == gn[dem]){
				setCellStyle(wb, sheet = sheet, row= row, col = col, cellstyle = ftitle1)
				setCellStyle(wb, sheet = sheet, row= row+1, col = col, cellstyle = fbase1)
				setCellStyle(wb, sheet = sheet, row= row+2, col = col, cellstyle = fbase2)
				for(i in 1:nrow(test)){
					setCellStyle(wb, sheet = sheet, row = row+1+i*2, col = col, cellstyle = fanswer1)
					setCellStyle(wb, sheet = sheet, row = row+2+i*2, col = col, cellstyle = fanswer2)
				}
				col = col + 1
			} else {
				setCellStyle(wb, sheet = sheet, row= row, col = col, cellstyle = ftitle2)
				setCellStyle(wb, sheet = sheet, row= row+2, col = col, cellstyle = fbase3)
				for(i in 1:nrow(test)){
					setCellStyle(wb, sheet = sheet, row = row+1+i*2, col = col, cellstyle = fanswer3)
					setCellStyle(wb, sheet = sheet, row = row+2+i*2, col = col, cellstyle = fanswer4)
				}		
				col = col + 1		
			}
		}
	}
}
setCellStyles <- function(){
	setFillForegroundColor(ftitle1, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	setFillPattern(ftitle1, fill = XLC$FILL.SOLID_FOREGROUND)
	# setDataFormat(ftitle1, format = "0%")
	setBorder(ftitle1, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	setBorder(ftitle1, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")
	setBorder(ftitle1, side = c("top"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	setFillForegroundColor(ftitle2, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	setFillPattern(ftitle2, fill = XLC$FILL.SOLID_FOREGROUND)
	# setDataFormat(ftitle2, format = "0%")
	setBorder(ftitle2, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	# setBorder(ftitle2, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")
	setBorder(ftitle2, side = c("top"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fbase1, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fbase1, fill = XLC$FILL.SOLID_FOREGROUND)
	# setDataFormat(fbase1, format = "0%")
	# setBorder(fbase1, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	setBorder(fbase1, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fbase2, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fbase2, fill = XLC$FILL.SOLID_FOREGROUND)
	setDataFormat(fbase2, format = "0%")
	setBorder(fbase2, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	setBorder(fbase2, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fbase3, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fbase3, fill = XLC$FILL.SOLID_FOREGROUND)
	setDataFormat(fbase3, format = "0%")
	setBorder(fbase3, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	# setBorder(fbase3, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fanswer1, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fanswer1, fill = XLC$FILL.SOLID_FOREGROUND)
	setDataFormat(fanswer1, format = "0%")
	# setBorder(fanswer1, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	setBorder(fanswer1, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fanswer2, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fanswer2, fill = XLC$FILL.SOLID_FOREGROUND)
	# setDataFormat(fanswer2, format = "0%")
	setBorder(fanswer2, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	setBorder(fanswer2, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fanswer3, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fanswer3, fill = XLC$FILL.SOLID_FOREGROUND)
	setDataFormat(fanswer3, format = "0%")
	# setBorder(fanswer3, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	# setBorder(fanswer3, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")

	# setFillForegroundColor(fanswer4, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
	# setFillPattern(fanswer4, fill = XLC$FILL.SOLID_FOREGROUND)
	# setDataFormat(fanswer4, format = "0%")
	setBorder(fanswer4, side = c("bottom"), type = XLC$"BORDER.THIN", color = XLC$"COLOR.BLACK")
	# setBorder(fanswer4, side = c("right"), type = XLC$"BORDER.MEDIUM", color = XLC$"COLOR.BLACK")
}
printOpenText <- function(dem = NULL) {
	sheet <- "QText"
	createSheet(wb, name = sheet)
	cols <- c("pid", dem, names(qn)[sapply(qn, function(x){x$Type %in% opentext})])
	data = as.data.frame(d[, .SD, .SDcols = cols])
	writeWorksheet(wb, data = data, sheet = sheet)
	saveWorkbook(wb)
	print("OpenText questions printed.")
}
save.xlsx <- function (file, l = NULL, ..., row.names = TRUE){
	require("xlsx")
	if(!is.matrix(l) & !is.data.frame(l) & is.list(l)){
		objects <- l
		objnames <- names(l)
	} else {
		fargs <- as.list(match.call(expand.dots = TRUE))
    	objects <- list(...)	
		objnames <- as.character(fargs)[-c(1, 2)]
		
	}
	nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i == 1)
            write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = row.names)
        else write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = row.names,
            append = TRUE)
    }
    print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
print.ranking <- function(insights, dem = NULL, logic = TRUE, limit = 25){
# insights <- c("number.comments", "rate", "number.users", "rate.positive", "rate.negative")
	# insights: numberic column names to print the ranking.
	# dem: aditional columna names to print next to the insight.
	# logic: logical vector to filter the data.table.
	# limit: maximum limit of comments to print. 
	att <- c(dem, insights)
	ranking <- function(insight, limit = 100) {
		head(d[, att, with = FALSE][order(get(insight), decreasing = TRUE)], limit)
	}
	r <- lapply(insights, ranking, limit = limit)
	names(r) <- insights
	save.xlsx(paste0(metric, "_Rank.xlsx"), l = r)
}
# EDIT THIS TO SHOW THE TABLE WITHOUT PRINTING ANYTHING!
printPropTest <- function(prop, test, wb, sheet, row, col, qName, gn, wNote = NULL) {
	if(is.null(wNote)){
		wNote <- "Note: Percentages unweighted."
	}
 	setColumnWidth(wb, sheet = sheet, column = col, width = 5000) # -1 autosizes each column
	sr = row
	sc = col
	baseName = "*Base*"

	writeWorksheet(wb, data = t(colnames(test)), sheet = sheet, startRow = row, startCol = col+2, header = FALSE)
	writeWorksheet(wb, data = colnames(prop)[1], sheet = sheet, startRow = row, startCol = col+1, header = FALSE)
	writeWorksheet(wb, data = qName, sheet = sheet, startRow = row, startCol = col, header = FALSE)

	row = row + 1
	if(baseName %in% rownames(prop)) {
		base = data.matrix(prop[1:2,], rownames.force = TRUE)
		writeWorksheet(wb,data = base, sheet = sheet, startRow = row, startCol = col, rownames = "include", header = FALSE)
		row = row + nrow(base)
		start = 3
	} else {
		start = 1
	}
	for(i in start:nrow(prop)) {	
		check = FALSE
		j = 1
		while(check == FALSE) {
			if(rownames(prop)[i] == rownames(test)[j]) {
				writeWorksheet(wb,data = data.matrix(t(test[j,]), rownames.force = TRUE), sheet = sheet, startRow = row+1, startCol = col + 2, header = FALSE)
				check = TRUE			
			}
			j = j + 1
		}
		writeWorksheet(wb, data = t(prop[i,]), sheet = sheet, startRow = row, startCol = col + 1, header = FALSE)
		writeWorksheet(wb, data = rownames(prop)[i], sheet = sheet, startRow = row, startCol = col, header = FALSE)
		row = row + 2
	}
	tabla = matrix(c(paste(wNote, sep = "")),ncol = 1,nrow = 1)
	writeWorksheet(wb, data = tabla, sheet = sheet, startRow = row, startCol = col, header = FALSE)
	setTableFormat(sr, sc, start, prop, wb, sheet, baseName, test, base, gn)
}	

save.xlsx <- function (file, l = NULL, ..., row.names = TRUE){
	require("xlsx")
	if(!is.matrix(l) & !is.data.frame(l) & is.list(l)){
		objects <- l
		objnames <- names(l)
	} else {
		fargs <- as.list(match.call(expand.dots = TRUE))
    	objects <- list(...)	
		objnames <- as.character(fargs)[-c(1, 2)]
		
	}
	nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i == 1)
            write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = row.names)
        else write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = row.names,
            append = TRUE)
    }
    print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
write.sheet <- function(sheet, q2p = NULL){
	# q2p means "Questions to print". Which questions to print.
	createSheet(wb, name = sheet)
	if(is.null(q2p)){
		q2p <- names(R[sheet][[1]]) 
	}
	qprinted <- 1
	row <- 3
	for(qname in q2p){print(paste0("printing ",qname))
		col <- 1
		row <- write.Qs(sheet = sheet, qname = qname, row = row, col = col)
		print(paste0("(", qprinted, "/", length(q2p), "). ", qname, " printed"))
		qprinted <- qprinted + 1 ; 
	}
	saveWorkbook(wb)
}
write.Qs <- function(sheet, qname, row, col){
	qtype <- R[sheet][[1]][qname][[1]]$Type
	title <- R[sheet][[1]][qname][[1]]$Title
	levels <- R[sheet][[1]][qname][[1]]$Levels
	if(qtype %in% c(radio, multi) & length(levels) <= 1) { 
		print(paste0("Question not written due to 1 Level: ", title))
	} else if (qtype %in% c(radio, multi)) { 
		gn <- R[sheet][[1]][qname][[1]]$nLevels
		prop <- R[sheet][[1]][qname][[1]]$Prop
		test <- R[sheet][[1]][qname][[1]]$Test
		abs <- R[sheet][[1]][qname][[1]]$Abs # Print unweighted bases for the absolute values: w = NULL.
		average <- R[sheet][[1]][qname][[1]]$Average
		mult.index <- R[sheet][[1]][qname][[1]]$Mult.Index
		wNote <- R[sheet][[1]][qname][[1]]$wNote

		# Print Title:
		writeWorksheet(wb,
			data = title, 
			sheet = sheet, 
			startRow = row, 
			startCol = col, 
			header = FALSE)
		row <- row+ nrow(title)

		if(!is.null(test)){
			# Print Tests:
			printPropTest(
				prop = prop, 
				test = test, 
				wb = wb, 
				sheet = sheet, 
				row = row, 
				col = col, 
				qName = "", 
				gn = gn, 
				wNote = wNote
			)				
			# Print Average:
			if(!is.null(average)){
				writeWorksheet(
					wb, 
					data = average, 
					sheet = sheet, 
					startRow = row + nrow(prop) + nrow(test)+1, 
					startCol = col, 
					rownames = qname, 
					header = FALSE
				)
			}
			col <- (col + ncol(prop) + 3)
		}	
		# Print Absolute Values
		writeWorksheet(
			wb, 
			data = abs,
			sheet = sheet, 
			startRow = row, 
			startCol = col, 
			rownames = qname
		)
		
		# Print Prop Values and Averages
		col <-  (col + ncol(abs) + 3)
		writeWorksheet(
			wb, 
			data = prop, 
			sheet = sheet, 
			startRow = row, 
			startCol = col, 
			rownames = qname
		)
		if(!is.null(average)){
			writeWorksheet(
				wb, 
				data = average, 
				sheet = sheet, 
				startRow = row + nrow(prop) + 1, 
				startCol = col, 
				rownames = qname, 
				header = FALSE
			)
		}
		rc <- expand.grid(row = (row+2):(row+nrow(prop)), col = (col+1):(col+1+ncol(prop)))
		setCellStyle(wb, sheet = sheet, row= rc$row, col = rc$col, cellstyle = fanswer3)

		if(is.null(test)){
			row <- row + nrow(prop) + 2	
		} else{
			row <- row + nrow(prop) + nrow(test)+ 4
		}

		if (qtype %in% multi & !is.null(mult.index)) {
			rows.mult.index <- 0
			row <- row + 3
			writeWorksheet(
				wb,
				data = mult.index, 
				sheet = sheet, 
				startRow = row, 
				startCol = 1, 
				rownames = qname
			)
			rows.mult.index <- nrow(mult.index) + 2
			row <- row + rows.mult.index + 4
		}
	} else if (qtype %in% number) { print("terceiro if")  ; print(sheet) ; print(row) ; print(col) ; print(qname)
	  print(R[sheet][[1]][qname]);
	  print(R[sheet][[1]][qname][[1]])
	  s <-  R[sheet][[1]][qname][[1]]$Summary ; print(s) ; print(nrow(s))
		writeWorksheet(
			wb, 
			data = s, 
			sheet = sheet, 
			startRow = row, 
			startCol = col, 
			rownames = qname 
		)
		row <- row + nrow(s) + 3; rm(s)
		
		
		
		
		# else if (qtype %in% number) {
		#   # Print Title:
		#   writeWorksheet(wb, data = title, 
		#                  sheet = sheet, startRow = row, 
		#                  startCol = col, header = FALSE)
		#   row <- row + nrow(title)
		#   
		#   s <- R[sheet][[1]][qname][[1]]$Summary
		#   writeWorksheet(wb, data = s,
		#                  sheet = sheet, startRow = row, 
		#                  startCol = col, rownames = qname)
		#   row <- row + nrow(s) + 3 ; rm(s)
		# }
		
		
		
		
		
	} else if (qtype %in% opentext) {

	}
	row
}

# # ------------------- RESULTS PRINTING ----------------------
if(!exists('output')){ output <- "output.xlsx"}
if (file.exists(output)) {file.remove(output)}

options(java.parameters = "- Xmx2g")
require("XLConnect")
wb <- loadWorkbook(output, create = TRUE)
ftitle1 <-  createCellStyle(wb) ; ftitle2 <-  createCellStyle(wb)
fbase1 <-  createCellStyle(wb) ; fbase2 <-  createCellStyle(wb) ; fbase3 <-  createCellStyle(wb)
fanswer1 <-  createCellStyle(wb) ; fanswer2 <-  createCellStyle(wb) ; fanswer3 <-  createCellStyle(wb) ; fanswer4 <-  createCellStyle(wb)
setCellStyles()

lapply(names(R), write.sheet)
printOpenText(dem = dem)