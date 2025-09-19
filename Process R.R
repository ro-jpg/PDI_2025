require(ggplot2) # Used in Graph function.
require(scales) # Used in Graph function.
require(matrixStats) # Used in estUni function.

set.R <- function(){ # Global variables needed: d (with binaries), qs, ql.
	logics <- list()
	if(!is.null(X2ndLevel)){
		logics <- lapply(X2ndLevel, function(x){
	 		l <- levels(as.factor(d[, x, with = FALSE][[1]]))
	 		r <- lapply(l, function(i){
	 			d[, x, with = FALSE][[1]] %in% i
	 		})
	 		names(r) <- l
	 		r
	 	})
 		names(logics) <- X2ndLevel
 		logics <- unlist(logics, recursive = FALSE)
	}
	logics <- logics[names(logics)[!lapply(logics, sum)==0]] # Delete all logics that contains only FALSE values.
	logics <- append(list(rep(TRUE, nrow(d))), logics) # Include the total (all TRUE)
	names(logics)[1] <- "Total"
	R <- lapply(names(logics), function(name){
		logic <- logics[name]
		print(paste0("Processing ", name, "."))
		R <- lapply(qnames, function(qname){
		 	R <- set.Q(
		 		logic = logic,
				type = qn[qname][[1]]$Type,
				qname = qname,
				title = qn[qname][[1]]$Title,
				dem = dem,
				graph = graph,
				average = qname %in% calcAverage
			)
			R
		})
		print(paste0(name, " processed."))
		names(R) <- qnames
		R
	})
	names(R) <- names(logics)
	R
}

set.Q <- function(logic = TRUE, type, qname, title = qname, dem = NULL, graph = FALSE, average = FALSE){
	Q <- list()
	if(is.list(logic)){
		logic <- logic[[1]] # as.logical(unlist(logic))
	}
	if(is.null(dem)){ g <- NULL 	} else {
		g <- as.data.frame(d[logic, .SD, .SDcols = dem])
		g[dem] <- lapply(g, factor)
	} 
	if(is.null(g)){ g <- NULL
	} else if (is.null(ncol(g))){
		Q['nLevels'] <- length(levels(as.factor(g)))
	} else {
		gn = NULL
		for(n in 1:ncol(g)){ 
			gn = cbind(gn,length(levels(as.factor(g[,n]))))
		}
		Q['nLevels'] <- list(gn) ; rm(gn)
	}
	Q['Name'] <- qname
	Q['Title'] <- list(matrix(title, ncol = 1, nrow = 1))
	Q['Type'] <- type
	if(Q$Type %in% c(multi, radio)){
		if(Q$Type %in% radio) {
			f <- d[logic, Q$Name, with = FALSE]
			if(!sum(!is.na(f))==0){
				Q['Levels'] <- list(levels(as.factor(f[[1]])))
				x <- as.data.frame(f[,lapply(Q$Levels,function(x){
					# CHECK IF ITS POSSIBLE TO CHANGE THIS NUMERIC VECTOR TO BOOLEAN. MEMORY USAGE.
					r <- as.numeric(f==x)
					r[r == 0] <- NA ; r
				})])
			} else { x <- NA }
		} else {
			Q['Levels'] <- list(qn[qname][[1]]$Levels)
			Q['Dummy.Names'] <- list(qn[qname][[1]]$Dummy.Names)
			x <- as.data.frame(d[logic, lapply(.SD, as.numeric), .SDcols = Q$Dummy.Names])
		}
		if(sum(x, na.rm = TRUE) == 0){
			print(paste0(Q$Name, " is empty. No processing made."))
		} else if (length(Q$Levels) == 1){
			print(paste0(Q$Name, " has only 1 level. It will not be processed."))
		} else {
			if("Data_Weight" %in% colnames(d)){
				w <- as.data.frame(d[logic, Data_Weight])
			} else{
				w <- data.matrix(rep(1, nrow(d[logic])))
			}
			Q['Abs'] <- list(table.abs(x, g, labels = Q$Levels)) 
			# Q['Abs'] <- list(d[logic, sum(Data_Weight, na.rm = TRUE), keyby = c(Q$Dummy.Names, dem)])
			if(sum(!w == 1) > 0){ # Check if there are weights. 
				Q['Abs_Weighted'] <- list(table.abs(x, g, w, labels = Q$Levels)) # Q['Abs'] <- list(d[logic, lapply(.SD, sum, na.rm = TRUE), .SDcols = Q$Dummy.Names, keyby = dem])
			}
			Q['Prop'] <- list(table.prop(x, g, w, labels = Q$Levels))
			Q['Test'] <- list(table.test(x, g, w, labels = Q$Levels))
			if(type %in% multi){
				Q['Mult.Index'] <- list(table.mult.index(x, g, basic = TRUE))
			}
			if(average){
				Q['Average'] <- list(factor.average(x, g, w, labels = Q$Levels))
				# Q['Average.Test'] <- # This test can be done with t-test, not proportions.
			}
			freq <- Q$Prop[!rownames(Q$Prop) %in% c("*Base*", "*% Base*"),] 
			if(graph){ Q['Graph'] <- list(Graph(freq, title = Q$Title , qname = qname)) }	
			print(paste(qname, " processed."))
		}
	} else if(type %in% number) {
	  Q['Summary'] <- list(table.mult.index(
	    x = as.data.frame(d[logic, lapply(.SD, as.numeric), .SDcols = qname]),
	    g = g, basic = TRUE))
	  
		# Q['Ranking'] <- 
		# Q['Summary'] <- table.mult.index
		# Q['Average.Test']
	} else if(type %in% opentext) {
		# Q['Word.Count'] <- 
		# Q['Word.Ranking'] <- "Temp"
		# Q['Word.Clouds'] <- 
	}
	Q
}
table.abs <- function(x, f = NULL, w = NULL, labels  = NULL, includeBase = TRUE, includeTotal = TRUE, order = FALSE, totalName = "Total") {
	if (is.null(f) & !includeTotal){
		stop("Parameter 'f' should be set if 'includeTotal' is set to FALSE")
	}
	if (is.null(labels)) {
		if(length(colnames(x)) == length(unique(colnames(x)))){
			labels <- colnames(x)	
		} else {
			labels <- c(1:ncol(x))
		}
	}
	x <- data.matrix(x)
	if (is.null(w)) {
		w <- rowSums(x, na.rm = TRUE)
		w[w>0] <- 1
	} else {
		temp <- rowSums(x, na.rm = TRUE) ; temp[temp>0] <- 1 
		w <- temp * w ; rm(temp)
	}
	w <- data.matrix(w)
	baseName = "*Base*" ; tabla = NULL ; base = NULL ; final <- NULL
	
	xw <- x * matrix(rep(w, ncol(x)), ncol = ncol(x))
	xw[is.na(xw)] <- 0
	tabla <- data.matrix(colSums(xw)) ; colnames(tabla) <- totalName
	base <- data.matrix(sum(w)) ; colnames(base) <- totalName
	if(!(is.null(f))) {
		if(!is.null(ncol(f))){
			for (k in 1:ncol(f)) {
				tabla = cbind(tabla, t(xtabs(xw ~ f[,k]))) 
				base = cbind(base, t(xtabs(w ~ f[,k]))) 
			}
		} else {
			tabla = cbind(tabla, t(xtabs(xw ~ f))) 
			base = cbind(base, t(xtabs(w ~ f))) 
		}
	}
	rownames(tabla) <- labels
	if(order) {
		if(is.null(f)){
			tabla <- data.matrix(tabla[order(-as.data.frame(tabla)[,totalName]),])
			colnames(tabla) <- totalName
		} else {
			tabla <- tabla[order(-as.data.frame(tabla)[,totalName]),]		
		}
	}
	if(includeBase) {
		final <- rbind(base, tabla) 
		rownames(final)[1] <- baseName
	} else {
		final <- rbind(final, tabla) 
	}
	if(!includeTotal) {
		final <- final[,-which(colnames(final) %in% c(totalName))]
	}
	final
}

#Proportion Table for Checkbox type columns (Multiple Choice Responses)
# INPUT	x as matrix with 1's and NA's for each question, g as matrix of information to do crosstab: each vector has one factor variable, labels: column names.
# OUTPUT	Table with proportion numbers from the input crosstabs.
table.prop <- function(x, f = NULL, w = NULL, labels  = NULL, includeBase = TRUE, includeTotal = TRUE, order = FALSE){
	baseName = "*Base*" ;  basePerName = "*% Base*" ; tabla = NULL ; base = NULL ; final <- NULL ; totalName = "Total"
	abs <- table.abs(x, f, w, labels = labels, includeTotal = includeTotal, order = order, includeBase = TRUE)
	base <- abs[baseName,]
	nums <- abs[!rownames(abs) %in% baseName,]
	if(is.null(f)){
		prop <- data.matrix(nums/base) ; colnames(prop) <- totalName
		basePer <- sum(prop, na.rm = TRUE)
	} else {
		prop <- t(apply(nums, 1, function(x){x / base}))
		basePer <- colSums(prop, na.rm = TRUE)
	}
	if(includeBase){
		baseUnw <- table.abs(x, f, labels = labels, includeTotal = includeTotal, order = order, includeBase = TRUE)[baseName,]
		prop <- rbind("*Base*" = baseUnw, 
			"*% Base*" = basePer, 
			prop)
	}
	prop
}

# Significance table. Taken from: http://pages.stat.wisc.edu/~yandell/st571/R/append12.pdf
# INPUT	Two vectors, ONE logical (TRUE or FALSE) and ONE wit factors to be compared.
# OUTPUT	Una fila con las letras correspondientes a la prueba de significancia estadística.
p.table <- function(vx, vg, w, p.adjust.method = "holm" , level = 0.05) {	
	vg <- as.factor(vg) 
	retorno = matrix("",nrow = 1, ncol = length(levels(vg))) 
	colnames(retorno) = paste(levels(vg)," (",LETTERS[1:length(levels(vg))],")",sep="") 
	if (!(sum(vx, na.rm = TRUE) == 0) & (!(sum(is.na(vx)) == length(vx)))) {	
		p = pairwise.t.test.weight(vx, vg, w, alternative = "two.sided")$p.value
		if (length(p)>0) {
			p = 1 * (p <= level) # 0 = no difference, 1 = difference
			p[is.na(p)] = 0
			p = rbind(0, cbind(p, 0))
			p = p + t(p)
			diag(p) = 0
			rownames(p)[1] = colnames(p)[1]
			colnames(p)[ncol(p)] = rownames(p)[nrow(p)]
			m <- xtabs(vx * w ~ vg)/xtabs(w ~ vg)
			m[is.na(m)] <- 0
			for ( c in 1:length(m))	{
				for( r in c:length(m)) {
					temp <- p[rownames(p) == names(m)[r],colnames(p) == names(m)[c]] 
					if (length(temp) >0 ) {
						if (temp==1) {
							if (m[r]<m[c]) {	
									retorno[c] = paste(retorno[c]," ",LETTERS[r],sep="")
							} else {	
								retorno[r] = paste(retorno[r]," ",LETTERS[c],sep="")
							}
						}
					}
				}
			}
		}
	}
	retorno
}

# Pair-Proportion Test
# INPUT	x as matrix with 1's and NA'on each column, g as demographic information per column, label: ordered names of x's column (same order)
# OUTPUT	Table with the Pair-Proportion Test
table.test <- function(x, g, w, labels = NULL) {	
	x <- data.matrix(x)
	w <- data.matrix(w)
	if(is.null(labels)) {	
		labels = c(1:ncol(x)) 
	}
	test = NULL
	if(!is.null(g)){ # In case it is required to print only totals.
		for (j in 1:ncol(x)) {	
			t = NULL
			if(is.null(ncol(g))){
				t = cbind(t,p.table(vx = x[,j], vg = as.factor(g), w = w))
			} else {
				for (k in 1:ncol(g)) {	
					t = cbind(t, p.table(vx = x[,j], vg = as.factor(g[,k]), w = w))
				}
			}
			test = rbind(test,t)
			rownames(test)[nrow(test)] = labels[j]
		}
	} else {
		# test <- table.prop(x, includeBase = FALSE, labels = labels)
		test <- NULL
	}
	test
}

pairwise.prop.test.weight <- function(){
	# Reference: http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_HypothesisTest-Means-Proportions/BS704_HypothesisTest-Means-Proportions_print.html
}

# Reference for Standard Deviation: http://stats.stackexchange.com/questions/6534/how-do-i-calculate-a-weighted-standard-deviation-in-excel
pairwise.t.test.weight <- function (vx, vg, w, p.adjust.method = p.adjust.methods[1], alternative = c("two.sided", "less", "greater"),  ...) {
	DNAME <- paste(deparse(substitute(vx)), "and", deparse(substitute(vg)))
	vg <- factor(vg)
	METHOD <- "t weighted tests with pooled SD"
	xbar <- xtabs(vx * w ~ vg)/xtabs(w ~ vg)
	n <- xtabs( ~ vg)
	xbarVector <- vg ; levels(xbarVector) <- xbar
	xbarVector <- as.numeric(data.matrix(xbarVector))
	numerador <- xtabs(w*((vx - xbarVector)^2) ~ vg)
	den <- w  ; den[den!=0] = 1
	M = xtabs(den ~ vg)
	denominador =  xtabs(w ~ vg) * (xtabs(den ~ vg) - 1) / xtabs(den ~ vg)
	s <- sqrt(numerador / denominador)
	degf <- n - 1
	total.degf <- sum(degf)
	pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
	compare.levels <- function(i, j) {
		dif <- xbar[i] - xbar[j]
		se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
		t.val <- dif/se.dif
		if (alternative == "two.sided") 
			2 * pt(-abs(t.val), total.degf)
		else pt(t.val, total.degf, lower.tail = (alternative == "less"))
	}
	PVAL <- pairwise.table(compare.levels, levels(vg), p.adjust.method)
	ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
	class(ans) <- "pairwise.htest"
	ans
}
# Multiplicity Index for Checkboxes
# Input: x vector full of 1's/NA's columns. g as variables to do crosstabs
# Output: Table with the multiplicity index from each checkbox question based on crosstabs inputed
table.mult.index <- function(x, g = NULL, basic = FALSE, na.rm = TRUE, totalName = "Total") {
	x <- data.table(x)
	logic <- rowSums(data.matrix(x), na.rm = na.rm) >0
	mult.index <- x[logic,
		data.table(estUni(rowSums(data.matrix(.SD), na.rm = na.rm), na.rm = na.rm, basic = basic), 
		keep.rownames = TRUE)]
	setkey(mult.index, rn)
	colnames(mult.index)[2] <- totalName
	if(!is.null(g)){
		temp <- function(f){
			f <- f[logic]
			temp <- x[logic, 
				data.table(estUni(rowSums(data.matrix(.SD), na.rm = na.rm), na.rm = na.rm, basic = basic), keep.rownames = TRUE), 
				keyby = f]
			add <- dcast.data.table(temp, formula = "rn~f", value.var = "V1")
			setkey(add, rn)
			add
		}
		if(is.null(ncol(g))){
			mult.index <- mult.index[temp(g)]
		} else {
			for(j in 1:ncol(g)) {
				mult.index <- mult.index[temp(g[,j])]
			}
		}
	}	
	data.matrix(data.frame(mult.index[c(6, 4, 3, 2, 1, 5)], row.names = 'rn', check.names = FALSE))
}
estUni <- function(x = NULL, na.rm = FALSE, basic = FALSE, OutliersLimit = 10, AssymetryLimit = 1) {
	coef <- function(dif, power, sd, n, na.rm) {
		dif <- dif ^ power
		dif <- colSums(dif, na.rm = na.rm) / (sd ^ power)
		dif * (1/n)
	}
	x = as.matrix(x)
	if (na.rm) {
		hasNA <- TRUE
		nNA <- colCounts(x, value = NA_real_, na.rm = FALSE)
		n <- nrow(x) - nNA
		hasNA <- any(nNA > 0L)
		if (hasNA) {
			n[n <= 1L] <- NA_integer_
		} else {
           		na.rm <- FALSE
		}
	} else {
     		n <- nrow(x)
	}	
	r = NULL
	if(is.null(x)){
		warning("No data set as input")
	} else if (length(x) == 0 ) {
		nr <- ifelse(basic, 6, 14)
		r = rbind(r, matrix(rep(NA, nr)))
	} else {	
		r = rbind(r, Sum = colSums(x, na.rm = na.rm))
		r = rbind(r, N = n)
		r = rbind(r, Minimum = colMins(x, na.rm = na.rm))
		r = rbind(r, Mean = colMeans(x, na.rm = na.rm))
		r = rbind(r, Maximum = colMaxs(x, na.rm = na.rm))
		r = rbind(r, StandDev = sqrt(colVars(x, na.rm = na.rm)))
		if(!basic) {
			r = rbind(r, Variance = colVars(x, na.rm = na.rm))
			r = rbind(r, CoefVariance = sqrt(r["Variance",]/(r["Mean",]^2)))
			r = rbind(r, Skewness = coef(dif = t(t(x)  - colMeans(x)), power = 3, sd = sqrt(r["Variance",]), n = n, na.rm = na.rm))
			r = rbind(r, CoefFisher = coef(dif = t(t(x)  - colMeans(x)), power = 4, sd = sqrt(r["Variance",]), n = n, na.rm = na.rm))
			r = rbind(r, Asymmetric = abs(r["Skewness",])>AssymetryLimit)
			r = rbind(r, "Outliers" = abs(r["CoefFisher",])>OutliersLimit)
			r = rbind(r, MAD = colMads(x, constant = 1.4826, na.rm = na.rm))
			r = rbind(r, Missings = colSums(is.na(x), na.rm = na.rm))			
		}
	}
	r
}

factor.average <- function(x, g, w, labels, includeTest = TRUE) {
	pr <- table.prop(x,f = g,w, labels, includeBase = FALSE)
	p <- suppressWarnings(as.numeric(substr(rownames(pr), 1, 2)))
	if(any(is.na(p))){
		p <- 1:length(p)
	}
	print <- t(t(as.matrix(pr)) %*% as.matrix(p))
	rownames(print) <- "Average: "
	print						
}

multi = c("multi", "checkbox")
radio = c("radio", "select", "single", "factor", "ranking")
grid = c('grid')
number = c("numeric", "integer", "double")
opentext = c("text", "opentext", "opentextlist", "character")
dates = c("date")

#############################################################
#### ------ DATA ANALYSIS ------- ###########################
#############################################################

if(!exists('dem')){ dem <- NULL }
if(!exists('X2ndLevel')){ X2ndLevel <- NULL }
if(!exists('graph')){ graph <- FALSE }
if(!exists('calcAverage')){ calcAverage <- NULL }
qnames <- names(qn)
R <- set.R()