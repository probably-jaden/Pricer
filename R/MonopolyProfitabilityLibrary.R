#rm(list = ls())

check_packages <- function() {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
    library(readxl)
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    install.packages("gridExtra")
    library(gridExtra)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    install.packages("scales")
    library(scales)
  }
}




#testing <- function(bool){
#  if(bool){
#   testBool <<- TRUE
#   cat(paste("In testing mode, using dorsal pack data"))
#  } else{
#    testBool <<- FALSE
#    cat(paste("testing turned off"))
#  }
#}



#testBool <- TRUE
#test dataset for durable breaking Sigmoid
#if(testBool) dp <- dplyr::tibble(wtp = c(64, 18, 46, 92, 110, 138, 113, 89, 0, 258, 205, 0, 18, 202, 46, 258, 0, 141, 0, 46, 61, 101, 64, 215, 95, 43, 46, 46, 132, 21, 18, 113, 9, 18, 21, 18, 104, 6, 0, 101, 6, 224, 322, 18, 316, 156, 104, 322, 285, 208, 316, 0, 288, 95, 6, 52, 46, 0, 18, 64, 98, 248, 18, 110, 0, 67, 0, 18, 0, 89, 132, 101, 18, 215, 18, 0, 0, 104, 285, 3, 46, 141, 322, 291, 89, 0, 101, 113, 67, 3, 132, 215, 224, 291, 9, 291, 267, 6, 6, 61, 178, 285, 64, 126, 0, 101, 15))






conNum_short <- function(number){
  rounded_Num <- abs(round(number, 3))
  formatted_Num <- if(rounded_Num >= 1e12) {
    paste0(format(rounded_Num / 1e12, digits = 2), "t")
  } else if(rounded_Num >= 1e9) {
    paste0(format(rounded_Num / 1e9, digits = 2), "b")
  } else if(rounded_Num >= 1e6) {
    paste0(format(rounded_Num / 1e6, digits = 2), "m")
  } else if(rounded_Num >= 1e3) {
    paste0(format(rounded_Num / 1e3, digits = 2), "k")
  } else {
    as.character(rounded_Num)  # Keep the value unchanged for var below 1K
  }
  return(formatted_Num)
}

conNum_long <- function(number) {
  rounded_Num <- round(number, 3)

  formatted_Num <- if (abs(rounded_Num) >= 1e9) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e9, digits = 3), " \ billion")
  } else if (abs(rounded_Num) >= 1e6) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e6, digits = 3), " \ million")
  } else if (abs(rounded_Num) >= 1e3) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e3, digits = 3), " \ thousand")
  } else {
    as.character(rounded_Num)  # Keep the value unchanged for values below 1K
  }
  return(formatted_Num)
}

# 1. Check if the data coming in is a csv or excel

# 2. Make the data into a tibble

quantityCreation <- function(data){
  check_packages()

  data <- data %>%
    group_by(wtp) %>%
    summarize(count = n()) %>%
    arrange(desc(wtp)) %>%
    mutate(quantity = cumsum(count))

  return(data)
}

revenueCreation <- function(data){
  check_packages()

  data <- data %>%
    mutate(revenue = wtp * quantity)

  return(data)
}

removeDollarSigns <- function(vector){
  check_packages()

  vector <- vector %>%
    mutate(wtp = as.numeric(gsub("\\$", "", wtp)))
  return(vector)
}




demandDurable <- function(data){
  check_packages()
  # Data scrubbing Willingness to Pay variable
  # if vector is labelled
  if(!any(colnames(data) %in% "wtp")){

    if("WTP" %in% colnames(data)){
      data <- data %>%
        rename(wtp = WTP)
      print("Changed WTP column to wtp")

    } else{
      names(data)[1] <- "wtp"
      print("Changed 1st column to wtp")
    }
  }

  # if vector is numeric / $ signs
  if(any(grepl("\\$", data$wtp))){
    data <- removeDollarSigns(data)
    print('removed Dollar signs')
  }

  # Remove any NA's
  # Make sure wtp is numeric
  data <- data %>%
    filter(!is.na(wtp)) %>%
    mutate(wtp = as.numeric(wtp))

  # Make the quantity variable
  data <- quantityCreation(data)
  data <- revenueCreation(data)

  return(data)
}

pivotData <- function(data, columns, valueName, columnName){
  data <- data %>%
    pivot_longer(cols = {{columns}}) %>%
    rename(!!columnName := 'name',
           !!valueName := 'value')

  return(data)
}


groupByPrice_ThenSum <- function(data, price, varToSum, newName){
  check_packages()

  data <- data %>%
    group_by({{price}}) %>%
    mutate({{newName}} := sum({{varToSum}}))

  return(data)
}

demandNonDurable <- function(data, price, quantityPerPerson){
  check_packages()
  data <- groupByPrice_ThenSum(data, {{price}}, {{quantityPerPerson}}, "quantity") %>%
    mutate(revenue = {{price}} * quantity) %>%
    rename(wtp = {{price}}) %>%
    select(wtp, quantity, revenue)

  return(data)
}

scatterPlot <- function(data, xColumn, yColumn){
  check_packages()
  # Check if columns exist in the data
  if (!(xColumn %in% names(data) && yColumn %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }

  title <- paste0(xColumn, " vs. ", yColumn)
  # Create scatter plot
  ggplot(data, aes(x = !!sym(xColumn), y = !!sym(yColumn))) +
    labs(title = title)+
    geom_point(color = "darkorchid") +
    theme_minimal()+
    labs(x = xColumn, y = yColumn)
}
# if(testBool) scatterPlot(tb, "quantity", "wtp")


demandScatterPlot <- function(data){
  check_packages()
  sPlot<- scatterPlot(data, 'wtp', 'quantity')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "darkorange", size = 1.5) +
    labs(x = "Willingness to Pay ($'s)", y = "Quantity", title = "Demand") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  return(plot)
}
# if(testBool) demandScatterPlot(tb)


revenueScatterPlot <- function(data){
  check_packages()
  sPlot <- scatterPlot(data, 'wtp', 'revenue')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "deepskyblue", size = 1.5)+
    labs(x = "Price ($'s)", y = "Revenue ($'s)", title = "Revenue") +
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))
  return(plot)
}
# if(testBool) revenueScatterPlot(tb)


logErr <- .0000000000000000001

# returns the model for your data
linModel <- function(data, x, y) lm(as.formula(paste(y, "~", x)), data = data)
expModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~", x)), data = data)
logModel <- function(data, x, y) lm(as.formula(paste(y, "~ log(", x, " + ", logErr, ")")), data = data)
powModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~ log(", x, " + ", logErr, ")")), data = data)

sigModel <- function(data, x, y){
  check_packages()
  output <- NA
  tryCatch({
    model <- nls(as.formula(paste(y, " ~ SSlogis(", x, ",Asym, xmid, scal)")), data = data)
  }, error = function(e) {
    output <- NA
    return(output)
  }) -> output
  return (output)
}



anyModel <- function(data, type, x, y){
  check_packages()
  switch(type,
         Linear      = do.call("linModel", list(data, x, y)),
         Exponential = do.call("expModel", list(data, x, y)),
         Log         = do.call("logModel", list(data, x, y)),
         Power       = do.call("powModel", list(data, x, y)),
         Sigmoid     = do.call("sigModel", list(data, x, y)),
         stop("Invalid type"))
}

demandModel <- function(data, type) return(anyModel(data, type, "wtp", "quantity"))
# if(testBool) demandModel(tb, "Sigmoid")

modelSummary <- function(data, type, x, y){
  check_packages()
  if(type == "Sigmoid") return(NA)
  return(summary(anyModel(data, type, x, y)))
}

demandSummary <- function(data, type) modelSummary(data, type, "wtp", "quantity")

# returns the model function for your data
linFun <- function(data, x, y) function(p) coef(linModel(data, x, y))[[1]] + coef(linModel(data, x, y))[[2]] * p
expFun <- function(data, x, y) function(p) exp(coef(expModel(data, x, y))[[1]] + coef(expModel(data, x, y))[[2]] * p)
logFun <- function(data, x, y) function(p) coef(logModel(data, x, y))[[1]] + coef(logModel(data, x, y))[[2]] * log(p)
powFun <- function(data, x, y) function(p) exp(coef(powModel(data, x, y))[[1]] + coef(powModel(data, x, y))[[2]] * log(p))

sigFun <- function(data, x, y) {
  check_packages()
  sModel <- sigModel(data, x, y)
  fun <- NA
  if(class(sModel) != class(NA)) fun <- function(p) coef(sModel)[[1]] / (1 + exp(-(p - coef(sModel)[[2]]) / coef(sModel)[[3]]))
  return(fun)
}



modelFun <- function(data, type, x, y){
  check_packages()
  switch(type,
         Linear      = do.call("linFun", list(data, x, y)),
         Exponential = do.call("expFun", list(data, x, y)),
         Log         = do.call("logFun", list(data, x, y)),
         Power       = do.call("powFun", list(data, x, y)),
         Sigmoid     = do.call("sigFun", list(data, x, y)),
         stop("Invalid type"))
}

modelFunction <- function(price, data, type, x, y){
  check_packages()
  return(modelFun(data, type, x, y)(price))
}

scaleFunction <- function(data, type, x, y, pop, sample = NA, fun = NA){
  check_packages()
  if(class(fun) == class(NA)) fun <- modelFun(data, type, x, y)
  if(class(fun) == class(NA)) return(NA)
  if(class(sample) == class(NA)) sample <- nrow(data)
  scaler <- (pop/sample)
  newFun <-  function(input) fun(input) * scaler
  return(newFun)
}

fQ <- function(data, type, population, sample = NA){
  check_packages()
  fQ <- scaleFunction(data, type, "wtp", "quantity", population, sample)
  if(class(fQ) == class(NA)) return(NA)
  return(fQ)
}




demandFunction <- function(price, data, type, population, sample = NA){
  check_packages()
  fQ <- scaleFunction(data, type, "wtp", "quantity", population, sample)
  if(class(fQ) == class(NA)) return(NA)

  price <- round(price, 2)

  show_price <- paste0('$', format(price, big.mark = ","))
  show_quantity <- format(round(fQ(price)), big.mark = ",")


  title = paste0("Quantity when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp))+
    geom_function(fun = fQ, color = "orange", lwd = 1.8, alpha = .8)+
    geom_point(x = price, y = fQ(price), color = 'darkorange', size = 3) +
    geom_segment(x = price, y = 0, xend = price, yend = fQ(price),
                 linetype = "dashed", color = "darkorange", lwd = .6)+
    geom_segment(x = 0, y = fQ(price), xend = price, yend = fQ(price),
                 linetype = "dashed", color = "orange", lwd = .4)+
    labs(title = title, x = "Price ($'s)", y = "Quantity Sold ") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1.5, hjust = 1, size = 4,
             color = "darkorange2") +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Quantity:", show_quantity)),
             vjust = 3.5, hjust = 1, size = 4,
             color = "darkorange2") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fQ(price))
}

# if(testBool) demandFunction(20, tb, "Sigmoid", 1e6)


fR <- function(data, type, pop, sample = NA) function(p) fQ(data, type, pop, sample)(p) * p

# if(testBool) fR(tb, "Linear", 1e6, 100)(10)

revenueFunction <- function(price, data, type, population, sample = NA){
  check_packages()

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  fR <- function(price) fQ(price) * price

  price <- round(price, 2)

  show_price <- paste0('$', format(price, big.mark = ","))
  show_revenue <- paste0('$', format(round(fR(price)), big.mark = ","))

  title = paste0("Revenue when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp))+
    geom_function(fun = fR, color = "skyblue", lwd = 1.8, alpha = .8)+
    geom_point(x = price, y = fR(price), color = 'skyblue3', size = 3) +
    geom_segment(x = price, y = 0, xend = price, yend = fR(price),
                 linetype = "dashed", color = "skyblue3", lwd = .6)+
    geom_segment(x = 0, y = fR(price), xend = price, yend = fR(price),
                 linetype = "dashed", color = "skyblue", lwd = .4)+
    labs(title = title, x = "Price ($'s)", y = "Revenue ($'s) ") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1.5, hjust = 1, size = 4,
             color = "skyblue3") +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Revenue:", show_revenue)),
             vjust = 3.5, hjust = 1, size = 4,
             color = "skyblue3") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fR(price))
}

# if(testBool) revenueFunction(20, tb, "Sigmoid", 1e6)




#Don't use, definitely DONT LET STUDENTS USE
superSwitch <- function(data, type, stage, x = "wtp", y = "quantity"){
  check_packages()
  switch(stage,
         Model   = do.call("anyModel", list(data, type, x, y)),
         Summary = do.call("modelSummary", list(data, type, x, y)),
         Function = do.call("modelFun", list(data, type, x, y)),
         stop("Invalid type"))
}



rSquared <- function(data, type, x, y){
  check_packages()
  if(type == "Sigmoid"){
    model <- sigModel(data, x, y)

    if(class(model) != class(NA)){
      predicted <- predict(model)
      residuals <- data[[y]] - predicted

      # Calculate residual sum of squares (RSS) and total sum of squares (TSS)
      RSS <- sum(residuals^2)
      mean_y <- mean(data[[y]])
      TSS <- sum((data[[y]] - mean_y)^2)

      # Calculate pseudo-R-squared (PRE)
      pseudo_R_squared <- 1 - (RSS / TSS)
      rSq <- pseudo_R_squared

    } else {
      rSq <- 0
    }
  } else {
    model <- superSwitch(data, type, "Summary", x, y)
    rSq <- model$adj.r.squared
  }
  return(rSq)
}

# if(testBool) rSquared(tb, "Sigmoid", 'wtp', 'quantity')
rSquaredDemand <- function(data, type) rSquared(data, type, "wtp", "quantity")

modelPlot <- function(data, type, x, y){
  check_packages()
  sPlot <- scatterPlot(data, x, y)
  title <- paste0(x, " vs. ", y, ": ", type)
  rSq <- round(rSquared(data, type, x, y), 2)
  modelFun <- modelFun(data, type, x, y)
  if(class(modelFun) == class(NA)) return()

  newPlot <- sPlot +
    geom_function(fun = modelFun, color = "orchid", lwd = 1.5, alpha =.4) +
    labs(title = title)+
    annotate("text", x = Inf, y = Inf,
             label = bquote(R^2 == .(rSq)),
             vjust = 2, hjust = 1.1, size = 5,
             color = "darkorchid4", alpha = .1)

  return((newPlot))
}
# if(testBool) modelPlot(tb, "Sigmoid", "wtp", "quantity")

demandPlot <- function(data, type, population, sample = NA){
  check_packages()
  title <- paste("Demand:", type)
  rSq <- round(rSquaredDemand(data, type), 2)
  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA))return()

  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  newTibble <- data %>%
    mutate(scaled_quantity = quantity * scalar)

  newPlot <- ggplot(data = newTibble)+
    geom_function(fun = fQ,
                  color = "orange", lwd = 1.5, alpha = .8) +
    geom_point(mapping = aes(x = wtp, y = scaled_quantity), color = "darkorange", size = 2)+
    labs(title = title, x = "Price ($'s)", y = "Quantity Sold ") +
    annotate("text", x = Inf, y = Inf,
             label = bquote(R^2 == .(rSq)),
             vjust = 2, hjust = 1.1, size = 5,
             color = "darkorange", alpha = .3)+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return()
}
# if(testBool) demandPlot(tb, "Sigmoid", 1e6, 100)

linFormulaFancy <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short((coef(demandModel(data, "Linear"))[[1]] * scalar))
  slope <- conNum_short(coef(demandModel(data, "Linear"))[[2]] * scalar)

  latex_string <- sprintf("%s \\ - \\left( \\ Price \\ \\times \\ %s \\right)", int, slope)
  return(latex_string)
}

expFormulaFancy <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar)
  slope <- conNum_short(exp(coef(demandModel(data, "Exponential"))[[2]]))

  latex_string <- sprintf("%s \\ \\times -%s^{Price}", int, slope)
  return(latex_string)
}


logFormulaFancy <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(coef(demandModel(data, "Log"))[[1]] * scalar)
  slope <- conNum_short(coef(demandModel(data, "Log"))[[2]] * scalar)

  latex_string <- sprintf("%s - %s \\times \\log(Price)", int, slope)
  return(latex_string)
}


powFormulaFancy <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(coef(demandModel(data, "Linear"))[[1]] * scalar)
  slope <- conNum_short(exp(coef(demandModel(data, "Linear"))[[2]]))

  latex_string <- sprintf("%s \\ \\times  %s^{ \\ log(Price)} ", int, slope)
  return(latex_string)
}

sigFormulaFancy <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- conNum_short(coef(model)[[1]] * scalar)
  B_Mid <- conNum_short(coef(model)[[2]])
  C_scale <- conNum_short(coef(model)[[3]])

  latex_string <- sprintf("\\frac{%s}{1 + \\mbox{e}^{\\frac{%s - \\text{Price}}{%s}}}", A_Int, B_Mid, C_scale)
  return(latex_string)
}


formulaFancy<- function(data, type, population, sample = NA){
  check_packages()
  switch(type,
         Linear      = do.call("linFormulaFancy", list(data, population, sample)),
         Exponential = do.call("expFormulaFancy", list(data, population, sample)),
         Log         = do.call("logFormulaFancy", list(data, population, sample)),
         Power       = do.call("powFormulaFancy", list(data, population, sample)),
         Sigmoid     = do.call("sigFormulaFancy", list(data, population, sample)),
         stop("Invalid type"))
}


linInterpret <- function(data, population, sample = NA){
  check_packages()

  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_long(coef(demandModel(data, "Linear"))[[1]] * scalar)
  slope <- conNum_long(coef(demandModel(data, "Linear"))[[2]] * scalar)

  intInterpret<-paste0("* **Intercept** : If the price was $0 we expect to sell **", int, "** unit(s)\n")
  slopeInterpret <-paste0("* **Slope** : For every $1 dollar increase in price, we loose **", slope, "** sale(s) on average\n")

  return(list(intInterpret, slopeInterpret))
}

# if(testBool)linInterpret(tb,102)

expInterpret <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample,2)

  intercept <- conNum_long(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar)
  slope <- coef(demandModel(data, "Exponential"))[[2]]
  slopePercent <- conNum_long((exp(slope)-1) * 100)

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope**: For every $1 dollar increase in price, our sales will drop by **", slopePercent, "%**\n")

  return(list(intInterpret, slopeInterpret))
}

# if(testBool) expInterpret(tb, 102)

logInterpret <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- conNum_long(coef(demandModel(data, "Log"))[[1]] * scalar)
  slope <- coef(demandModel(data, "Log"))[[2]] * scalar
  slopeRate <- conNum_short(round(slope/100, 2))

  intInterpret <- paste0(" * **Intercept** : If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope** : For every 1% increase in price, we loose **", slopeRate, "** sales on average\n")

  return(list(intInterpret, slopeInterpret))
}

# if(testBool) logInterpret(tb, 102)

powInterpret <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- conNum_short(exp(coef(demandModel(data, "Power"))[[1]]) * scalar)
  slope <- coef(demandModel(data, "Power"))[[2]]
  slopePercent <- conNum_short(slope * 100)

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope**: For every 1% increase in price, our sales will drop by **", slopePercent,"%** \n")

  return(list(intInterpret, slopeInterpret))
}

# if(testBool) powInterpret(tb, 102)

sigInterpret <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- conNum_short(coef(model)[[1]] * scalar)
  B_Mid <- conNum_short(round(coef(model)[[2]], 2))
  #C_scale <- coef(model)[[3]]

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", A_Int, "** unit(s)\n")
  midPointInterpret <- paste0(" * **Mid-Point**: The curve will change from decreasing at an increasing rate to a decreasing rate when the price equals **$", B_Mid, "**\n")

  return(list(intInterpret, midPointInterpret))

}

# if(testBool) sigInterpret(tb, 102)

demandInterpret <- function(data, type, population, sample = NA){
  check_packages()
  switch(type,
         Linear      = do.call("linInterpret", list(data, population, sample)),
         Exponential = do.call("expInterpret", list(data, population, sample)),
         Log         = do.call("logInterpret", list(data, population, sample)),
         Power       = do.call("powInterpret", list(data, population, sample)),
         Sigmoid     = do.call("sigInterpret", list(data, population, sample)),
         stop("Invalid type"))
}



linFormula <- function(data, population, sample = NA) {
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- round(coef(demandModel(data, "Linear"))[[1]] * scalar, 3)
  slope <- round(coef(demandModel(data, "Linear"))[[2]] * scalar,3)

  cat("Linear Demand: \n \n  Quantity = Intercept + (Slope * Price)  \n")
  cat(paste0("  Quantity = ", int, " + (", slope, " * Price)\n"))

  cat(paste0("\nIntercept: If the price was $0 we expect to sell ", int, " unit(s)\n"))
  cat(paste0("Slope: For every $1 dollar increase in price, we loose ", slope, " sale(s) on average \n\n"))

}
# if(testBool) linFormula(tb, 102)


expFormula <- function(data, population, sample = NA) {
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample,2)

  lin_intercept <- round(coef(demandModel(data, "Exponential"))[[1]], 2)
  lin_slope <- round(coef(demandModel(data, "Exponential"))[[2]], 2)

  intercept <- round(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar, 2)
  slope <- round(exp(coef(demandModel(data, "Exponential"))[[2]]),2)

  slopePercent <- round((slope - 1) * 100, 3)


  cat("Exponential Demand: \n\n")
  cat("  Linearized (transformed): \n    log(Quantity) = Scalar * (intercept + (Slope * Price)) \n")
  cat(paste0("    log(Quantity) = ", scalar, " * (", lin_intercept, " + (", lin_slope, " * Price))"))
  cat("\n   \n  Actual (untransformed): \n    Quantity = intercept * exp{Slope * Price} \n")
  cat(paste0("    Quantity = ", intercept, " * (", slope, " ^ Price)"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", intercept, " unit(s)\n"))
  cat(paste0("Slope: For every $1 dollar increase in price, our sales will drop by ", slopePercent, "% \n\n"))
}

# if(testBool) expFormula(tb, 102)


logFormula <- function(data, population, sample = NA) {
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- round(coef(demandModel(data, "Log"))[[1]] * scalar, 2)
  slope <- round(coef(demandModel(data, "Log"))[[2]] * scalar,2)
  slopeRate <- -round(slope/100, 3)

  lin_intercept <- round(coef(demandModel(data, "Exponential"))[[1]], 2)

  cat("Log Demand: \n\n")
  cat("    Quantity = intercept * (Slope * log(Price))\n")
  cat(paste0("    Quantity = ", intercept, " * (", slope, " * log(Price))"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", round(intercept), " unit(s)\n"))
  cat(paste0("Slope: For every 1% increase in price, we loose ", slopeRate, " sales on average \n\n"))
}
# if(testBool) logFormula(tb, 102)

powFormula <- function(data, population, sample = NA) {
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- round(exp(coef(demandModel(data, "Power"))[[1]]) * scalar, 2)
  slope <- round(coef(demandModel(data, "Power"))[[2]],2)
  slopePercent <- round(slope * 100, 3)

  lin_intercept <- round(coef(demandModel(data, "Power"))[[1]], 2)

  cat("Power Demand: \n\n")
  cat("  Linearized (transformed): \n    log(Quantity) = Scalar * (intercept + (Slope * log(Price))) \n")
  cat(paste0("    log(Quantity) = ", scalar, " * (", lin_intercept, " + (", slope, " * log(Price)))"))
  cat("\n\n  Actual (untransformed): \n    Quantity = intercept * exp{Slope * log(Price)} \n")
  cat(paste0("    Quantity = ", intercept, " * exp{", slope, " * log(Price)}"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", intercept, " unit(s)\n"))
  cat(paste0("Slope: For every 1% increase in price, our sales will drop by ", -slopePercent,"% \n\n"))
}

# if(testBool) powFormula(tb, 102)


sigFormula <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- round(coef(model)[[1]] * scalar, 2)
  B_Mid <- round(coef(model)[[2]], 2)
  C_scale <- round(coef(model)[[3]], 2)

  cat("Sigmoid Demand: \n\n")
  cat("    Quantity = intercept / (1 + exp{(Mid-Point - Price) / Scale})\n")
  cat(paste0("    Quantity = ", A_Int, " / (1 + exp{(", B_Mid, " - Price) / ", C_scale, " } \n"))

  cat(paste0("\nIntercept: If the price was $0 we expect to sell ", A_Int, " unit(s)\n"))
  cat(paste0("Mid-Point: The curve will change from decreasing at an increasing \n           rate to a decreasing rate when the price equals $", B_Mid, "\n\n"))
}
# if(testBool) sigFormula(tb, 102)


demandFormula <- function(data, type, population, sample = NA){
  check_packages()
  switch(type,
         Linear      = do.call("linFormula", list(data, population, sample)),
         Exponential = do.call("expFormula", list(data, population, sample)),
         Log         = do.call("logFormula", list(data, population, sample)),
         Power       = do.call("powFormula", list(data, population, sample)),
         Sigmoid     = do.call("sigFormula", list(data, population, sample)),
         stop("Invalid type"))
}

# if(testBool) demandFormula(tb, "Sigmoid", 1,1)


revenueOptimize <- function(data, type, population, sample = NA){
  check_packages()
  fR <- fR(data, type, population, sample)
  opt_Rev <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[2]]
  opt_Price <- round(optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[1]],2)

  revenueFunction(opt_Price, data, type, population, sample)

  return(list(opt_Rev, opt_Price))
}
# if(testBool) revenueOptimize(tb, "Linear", 1e6, 100)

revenuePlot <- function(data, type, population, sample = NA){
  check_packages()
  title <- paste("Revenue:", type)

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return()
  fR <- fR(data, type, population, sample)

  opt_Rev <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[2]]
  opt_Price <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[1]]

  show_Rev <- paste0('$', conNum_short(opt_Rev))
  show_Price <- paste0('$', conNum_short(round(opt_Price,2)))

  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  newTibble <- data %>%
    mutate(scaled_revenue = quantity * scalar * wtp)

  newPlot <- ggplot(data = newTibble)+
    geom_segment(x = opt_Price, y = 1, xend = opt_Price, yend = opt_Rev,
                 linetype = "dashed", color = "deepskyblue", lwd = .3)+
    geom_function(fun = fR, color = "deepskyblue", lwd = 1.3, alpha = .3)+
    geom_point(mapping = aes(x = wtp, y = scaled_revenue), color = "deepskyblue3", alpha = .5)+
    labs(title = title, x = "Price ($'s)", y = "Revenue ($'s)") +
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_Price),
             vjust = 1.5, hjust = 1, size = 3,
             color = "deepskyblue3", alpha = .8) +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Rev:", show_Rev)),
             vjust = 3.5, hjust = 1, size = 3,
             color = "deepskyblue3", alpha = .8) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme_minimal()+
    theme(plot.title = element_text(face = "bold"))

  return(newPlot)
}
# if(testBool) revenuePlot(tb, "Sigmoid", 1e6, 100)

allLinearSummaries <- function(data){
  check_packages()
  lin_summary <- demandSummary(data, "Linear")
  exp_summary <- demandSummary(data, "Exponential")
  log_summary <- demandSummary(data, "Log")
  pow_summary <- demandSummary(data, "Power")

  return(list(lin_summary, exp_summary, log_summary, pow_summary))
}
# if(testBool) allLinearSummaries(tb)

nBestModels <- function(data, x, y, n){
  check_packages()
  r_squared_values <- c(
    Linear = rSquared(data, "Linear", x, y),
    Exponential = rSquared(data, "Exponential", x, y),
    Log = rSquared(data, "Log", x, y),
    Power = rSquared(data, "Power", x, y),
    Sigmoid = rSquared(data, "Sigmoid", x, y)
  )

  # Sort models by R-squared and select top 4
  sorted_models <- sort(r_squared_values, decreasing = TRUE)
  top_models <- names(sorted_models)[1:n]

  return(top_models)
}
nBestDemandModels <- function(data, n) nBestModels(data, "wtp", "quantity", n)
bestModel <- function(data, x, y) nBestModels(data, x, y, 1)
bestDemand <- function(data) nBestDemandModels(data, 1)
# if(testBool) nBestModels(tb, "wtp", "quantity", 3)

modelCompare <- function(data, x, y, n = 4) {
  check_packages()
  top_types <- nBestModels(data, x, y, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- modelPlot(data, type, x, y)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- do.call(grid.arrange, plot_list)
  return(final_plot)
}
# if(testBool) modelCompare(tb, "wtp", "quantity", 4)

demandCompare <- function(data, population, sample = NA, n = 3) {
  check_packages()
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- demandPlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <-  suppressWarnings(do.call(grid.arrange, c(plot_list, ncol = 2)))
  return(final_plot)
}
# if(testBool) demandCompare(tb, 1e6)

revenueCompare <- function(data, population, sample = NA, n = 3) {
  check_packages()
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- revenuePlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <-  suppressWarnings(do.call(grid.arrange, c(plot_list, ncol = 2)))
  return(final_plot)
}
#Test
# if(testBool) revenueCompare(tb, 1e6, 10)


v <- 20
f <- 10000
Pop <- 1e7

# for internal code use
fC <- function(variable, fixed, fQ) {
  fC <- function(p) fixed + variable * fQ(p)
  return(fC)
}

# for student use
costFunction <- function(price, data, type, variable, fixed, population, sample = NA){
  check_packages()
  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  return(fC(variable, fixed, fQ)(price))
}
# if(testBool) costFunction(100, tb, "Sigmoid", 5, 2e5, 1e6, 100)

# for internal code use
fPi <- function(fR, fC){
  fPi <- function(p) fR(p) - fC(p)
  return(fPi)
}

profitFunction <- function(price, data, type, variable, fixed, population, sample = NA){
  check_packages()

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  fR <- fR(data, type, population, sample)
  fC <- fC(variable, fixed, fQ)
  fPi <- function(p) fR(p) - fC(p)

  price <- round(price, 2)

  show_price <- paste0('$', format(price, big.mark = ","))
  show_profit <- paste0(format(round(fPi(price)), big.mark = ","))

  title = paste0("Profit when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp))+
    geom_function(fun = fPi, color = "green3", lwd = 1.8, alpha = .8)+
    geom_point(x = price, y = fPi(price), color = 'green4', size = 3) +
    geom_segment(x = price, y = 0, xend = price, yend = fPi(price),
                 linetype = "dashed", color = "green4", lwd = .6)+
    geom_segment(x = 0, y = fPi(price), xend = price, yend = fPi(price),
                 linetype = "dashed", color = "green3", lwd = .4)+
    labs(title = title, x = "Price ($'s)", y = "Profit ($'s) ") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1.5, hjust = 1, size = 4,
             color = "darkgreen") +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Profit: $", show_profit)),
             vjust = 3.5, hjust = 1, size = 4,
             color = "darkgreen") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fPi(price))
}

# if(testBool) profitFunction(30.5683, tb, "Exponential", v, f, 1e6)

profitOptimize <- function(data, type, variable, fixed, population, sample = NA){
  check_packages()

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  fR <- fR(data, type, population, sample)
  fC <- fC(variable, fixed, fQ)
  fPi <- fPi(fR, fC)

  opt_Profit <- (optimize(fPi, lower = 0, upper = data$wtp, maximum = TRUE )[[2]])
  opt_Price <- round(optimize(fPi, lower = 0, upper = data$wtp, maximum = TRUE )[[1]],2)

  profitFunction(opt_Price, data, type, variable, fixed, population, sample)

  return(list(opt_Profit, opt_Price))
}

# if(testBool) profitOptimize(tb, "Sigmoid", 21, 1e5, 1e5)

profitPlot <- function(data, type, variable, fixed, population, sampleSize = NA, yCap = 0){
  check_packages()
  fQ <- fQ(data, type, population, sampleSize)
  if(class(fQ) == class(NA)) return(NA)
  fR <- fR(data, type, population, sampleSize)
  fC <- fC(variable, fixed, fQ)
  fPi <- fPi(fR, fC)

  title <- paste("Profit:", type)

  opt_Profit <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]
  opt_Price <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[1]]
  opt_Revenue <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]

  show_Profit <- paste0('$', conNum_short(opt_Profit))
  show_Price <- paste0('$', conNum_short(round(opt_Price,2)))

  if(yCap == 0){
    yCap <- opt_Revenue
  }

  plot<-ggplot(data = data)+
    xlim(0, max(data$wtp))+
    geom_segment(x = opt_Price, y = 1, xend = opt_Price, yend = opt_Profit,
                 linetype = "dashed", color = "lightgreen", lwd = .3)+
    geom_function(fun = fR, color = "deepskyblue2", lwd = 1.2, alpha = .7)+
    geom_function(fun = fC, color = "brown1", lwd = 1.2, alpha = .7)+
    geom_function(fun = fPi, color ="green3",lwd = 1.2, alpha = .7) +
    labs(x = "Price ($'s)", y = "Profit ($'s)") +
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_Price),
             vjust = 1.5, hjust = 1.5, size = 3,
             color = "forestgreen", alpha = .8) +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Profit:", show_Profit)),
             vjust = 3.5, hjust = 1.3, size = 3,
             color = "forestgreen", alpha = .8) +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme_minimal()+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, yCap))+
    labs(title = title)+
    theme(plot.title = element_text(face = "bold"))

  return(plot)
}
#Test
# if(testBool) profitPlot(tb, "Exponential", v, f, Pop)


profitRevFunction <- function(price, data, type, variable, fixed, population, sample = NA, yCap = 0){
  check_packages()
  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  fR <- fR(data, type, population, sample)
  fC <- fC(variable, fixed, fQ)
  fPi <- function(p) fR(p) - fC(p)

  opt_Revenue <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]

  if(yCap == 0){
    yCap <- opt_Revenue
  }

  show_price <- paste0('$', format(price, big.mark = ","))
  show_revenue <- paste0('$', format(fR(price), big.mark = ","))
  show_profit <- paste0('$', format(round(fPi(price)), big.mark = ","))

  title = paste0("Profit and Revenue when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp))+
    geom_function(fun = fR, color = "deepskyblue2", lwd = 1.2, alpha = .7)+
    geom_function(fun = fPi, color = "green3", lwd = 1.8, alpha = .8)+
    geom_segment(x = price, y = 0, xend = price, yend = fPi(price),
                 linetype = "dashed", color = "green4", lwd = .6)+
    geom_segment(x = 0, y = fPi(price), xend = price, yend = fPi(price),
                 linetype = "dashed", color = "green3", lwd = .2)+
    geom_segment(x = price, y = fPi(price), xend = price, yend = fR(price),
                 linetype = "dashed", color = "deepskyblue3", lwd = .6)+
    geom_segment(x = 0, y = fR(price), xend = price, yend = fR(price),
                 linetype = "dashed", color = "skyblue2", lwd = .2)+
    labs(title = title, x = "Price ($'s)", y = "Profit ($'s) ") +
    geom_point(x = price, y = fPi(price), color = 'green4', size = 3) +
    geom_point(x = price, y = fR(price), color = 'deepskyblue3', size = 3) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, yCap))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    annotate("text", x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1.5, hjust = 1, size = 4,
             color = "grey30") +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Revenue:", show_revenue)),
             vjust = 3.5, hjust = 1, size = 4,
             color = "deepskyblue3") +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Profit:", show_profit)),
             vjust = 5.5, hjust = 1, size = 4,
             color = "green3") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fPi(price))
}

# if(testBool) profitRevFunction(100, tb, "Exponential", v, f, 1e6, 100)


nBestProfitPlots <- function(data, n, variable, fixed, pop, sample = NA){
  check_packages()
  top_models<-nBestDemandModels(data, n)
  plot_list <- list()

  for (model in top_models) {
    nextPlot <- profitPlot(data, model, variable, fixed, pop, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  final_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  return(final_plot)
}


profitCompare <- function(data, variable, fixed, population, sample = NA) nBestProfitPlots(data, 3, variable, fixed, population, sample)


#Test
# if(testBool) profitCompare(tb, v, f, Pop, 100)
















