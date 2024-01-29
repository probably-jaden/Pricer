#rm(list = ls())
library(tidyverse)
library(readxl)
library(gridExtra)
library(scales)


quantityCreation <- function(data){
  data <- data %>%
    group_by(wtp) %>%
    summarize(count = n()) %>%
    arrange(desc(wtp)) %>%
    mutate(quantity = cumsum(count))

  return(data)
}

revenueCreation <- function(data){
  data <- data %>%
    mutate(revenue = wtp * quantity)

  return(data)
}

removeDollarSigns <- function(vector){
  vector <- vector %>%
    mutate(wtp = as.numeric(gsub("\\$", "", wtp)))
  return(vector)
}


demandDurable <- function(data){
  # Data scrubbing Willingness to Pay variable
  # if vector is labelled
  if(!any(colnames(data) %in% "wtp")){

    if(any(colnames(data) %in% "WTP")){
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

  print(data)
  return(data)
}



groupByPrice_ThenSum <- function(data, price, varToSum, newName){
  data <- data %>%
    group_by({{price}}) %>%
    mutate({{newName}} := sum({{varToSum}}))  # Use := to create a new column

  return(data)
}

demandNonDurable <- function(data, price, quantityPerPerson){
  data <- groupByPrice_ThenSum(data, {{price}}, {{quantityPerPerson}}, "quantity") %>%
    mutate(revenue = {{price}} * quantity) %>%
    rename(wtp = {{price}}) %>%
    select(wtp, quantity, revenue)

  print(data)
  return(data)
}

scatterPlot <- function(data, xColumn, yColumn){
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



plotQuantityData <- function(data){
  sPlot<- scatterPlot(data, 'wtp', 'quantity')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "darkorange", size = 1.5) +
    labs(x = "Willingness to Pay ($'s)", y = "Quantity", title = "Demand") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))

  return(plot)
}



plotRevenueData <- function(data){
  sPlot <- scatterPlot(data, 'wtp', 'revenue')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "deepskyblue", size = 1.5)+
    labs(x = "Price ($'s)", y = "Revenue ($'s)", title = "Revenue") +
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))
  return(plot)
}



logErr <- .0000000000000000001

# returns the model for your data
linModel <- function(data, x, y) lm(as.formula(paste(y, "~", x)), data = data)
expModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~", x)), data = data)
logModel <- function(data, x, y) lm(as.formula(paste(y, "~ log(", x, " + ", logErr, ")")), data = data)
powModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~ log(", x, " + ", logErr, ")")), data = data)

sigModel <- function(data, x, y){
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
  switch(type,
         Linear      = do.call("linModel", list(data, x, y)),
         Exponential = do.call("expModel", list(data, x, y)),
         Log         = do.call("logModel", list(data, x, y)),
         Power       = do.call("powModel", list(data, x, y)),
         Sigmoid     = do.call("sigModel", list(data, x, y)),
         stop("Invalid type"))
}

demandModel <- function(data, type) return(anyModel(data, type, "wtp", "quantity"))


modelSummary <- function(data, type, x, y){
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
  sModel <- sigModel(data, x, y)
  fun <- NA
  if(class(sModel) != "logical") fun <- function(p) coef(sModel)[[1]] / (1 + exp(-(p - coef(sModel)[[2]]) / coef(sModel)[[3]]))
  return(fun)
}

modelFun <- function(data, type, x, y){
  switch(type,
         Linear      = do.call("linFun", list(data, x, y)),
         Exponential = do.call("expFun", list(data, x, y)),
         Log         = do.call("logFun", list(data, x, y)),
         Power       = do.call("powFun", list(data, x, y)),
         Sigmoid     = do.call("sigFun", list(data, x, y)),
         stop("Invalid type"))
}


scaleFunction <- function(data, type, x, y, pop, sample = NA, fun = NA){
  if(class(fun) == "logical") fun <- modelFun(data, type, x, y)
  if(class(fun) == "logical") return(NA)
  if(class(sample) == "logical") sample <- nrow(data)
  scaler <- (pop/sample)
  newFun <-  function(input) fun(input) * scaler
  return(newFun)
}

demandFunction <- function(data, type, pop, sample = NA){
  fQ <- scaleFunction(data, type, "wtp", "quantity", pop, sample)
  if(class(fQ) == "logical") return(NA)
  return(fQ)
}


revenueFunction <- function(data, type, pop, sample = NA) function(p) demandFunction(data, type, pop, sample)(p) * p




#Don't use, definitely DONT LET STUDENTS USE
superSwitch <- function(data, type, stage, x = "wtp", y = "quantity"){
  switch(stage,
         Model   = do.call("anyModel", list(data, type, x, y)),
         Summary = do.call("modelSummary", list(data, type, x, y)),
         Function = do.call("modelFun", list(data, type, x, y)),
         stop("Invalid type"))
}

rSquared <- function(data, type, x, y){
  if(type == "Sigmoid"){
    model <- sigModel(data, x, y)

    if(class(model) == "nls"){
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


rSquaredDemand <- function(data, type) rSquared(data, type, "wtp", "quantity")

plotModel <- function(data, type, x, y){
  sPlot <- scatterPlot(data, x, y)
  title <- paste0(x, " vs. ", y, ": ", type)
  rSq <- round(rSquared(data, type, x, y), 2)
  modelFun <- modelFun(data, type, x, y)
  if(class(modelFun) == "logical") return()

  newPlot <- sPlot +
    geom_function(fun = modelFun, color = "orchid", lwd = 1.5, alpha =.4) +
    labs(title = title)+
    annotate("text", x = Inf, y = Inf,
             label = bquote(R^2 == .(rSq)),
             vjust = 2, hjust = 1.1, size = 5,
             color = "darkorchid4", alpha = .1)

  return(newPlot)
}


demandPlot <- function(data, type, population, sample = NA){
  title <- paste("Demand:", type)
  rSq <- round(rSquaredDemand(data, type), 2)
  fQ <- demandFunction(data, type, population, sample)
  if(class(fQ) == "logical")return()

  if(class(sample) == "logical") sample <- nrow(data)
  scalar <- population/sample

  newTibble <- data %>%
    mutate(scaled_quantity = quantity * scalar)

  newPlot <- ggplot(data = newTibble)+
    geom_function(fun = fQ,
                  color = "orange", lwd = 1.5, alpha = .8) +
    geom_point(mapping = aes(x = wtp, y = scaled_quantity), color = "darkorange", size = 2)+
    labs(title = title, x = "Willingness to Pay ($'s)", y = "Quantity") +
    annotate("text", x = Inf, y = Inf,
             label = bquote(R^2 == .(rSq)),
             vjust = 2, hjust = 1.1, size = 5,
             color = "darkorange", alpha = .3)+
    scale_y_continuous(labels = label_number_si(),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))

  return(newPlot)
}



convNum <- function(number){
  rounded_Num <- round(number, 0)

  formatted_Num <- if(rounded_Num >= 1e9) {
    paste0(format(rounded_Num / 1e9, digits = 2), "B")
  } else if(rounded_Num >= 1e6) {
    paste0(format(rounded_Num / 1e6, digits = 2), "M")
  } else if(rounded_Num >= 1e3) {
    paste0(format(rounded_Num / 1e3, digits = 2), "K")
  } else {
    as.character(rounded_Num)  # Keep the value unchanged for var below 1K
  }
  dollar_Num <- paste0("$", formatted_Num)
  return(dollar_Num)
}

optimizeRevenue <- function(data, type, population, sample = NA){
  fR <- revenueFunction(data, type, population, sample)
  opt_Rev <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[2]]
  opt_Price <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[1]]

  return(list(opt_Rev, opt_Price))
}


revenuePlot <- function(data, type, population, sample = NA){
  title <- paste("Revenue:", type)

  fQ <- demandFunction(data, type, population, sample)
  if(class(fQ) == "logical") return()
  fR <- revenueFunction(data, type, population, sample)

  opt_Rev <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[2]]
  opt_Price <- optimize(fR, lower = 0, upper = data$wtp, maximum = TRUE )[[1]]

  show_Rev <- convNum(opt_Rev)
  show_Price <- convNum(opt_Price)

  if(class(sample) == "logical") sample <- nrow(data)
  scalar <- population/sample

  newTibble <- data %>%
    mutate(scaled_revenue = quantity * scalar * wtp)

  newPlot <- ggplot(data = newTibble)+
    geom_segment(x = opt_Price, y = 1, xend = opt_Price, yend = opt_Rev,
                 linetype = "dashed", color = "deepskyblue", lwd = .3)+
    geom_function(fun = fR, color = "deepskyblue", lwd = 1.3, alpha = .3)+
    geom_point(mapping = aes(x = wtp, y = scaled_revenue), color = "deepskyblue3", alpha = .5)+
    labs(title = title, x = "Willingness to Pay ($'s)", y = "Revenue ($'s)") +
    annotate("text", x = Inf, y = Inf,
             label = paste("P:", show_Price),
             vjust = 1.5, hjust = 1, size = 3,
             color = "deepskyblue3", alpha = .8) +
    annotate("text", x = Inf, y = Inf,
             label =(paste("R:", show_Rev)),
             vjust = 3.5, hjust = 1, size = 3,
             color = "deepskyblue3", alpha = .8) +
    scale_y_continuous(labels = label_number_si(),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme_minimal()+
    theme(plot.title = element_text(face = "bold"))

  return(newPlot)
}


allLinearSummaries <- function(data){
  lin_summary <- demandSummary(data, "Linear")
  exp_summary <- demandSummary(data, "Exponential")
  log_summary <- demandSummary(data, "Log")
  pow_summary <- demandSummary(data, "Power")

  return(list(lin_summary, exp_summary, log_summary, pow_summary))
}


nBestModels <- function(data, x, y, n){
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


compareModels <- function(data, x, y, n = 4) {
  top_types <- nBestModels(data, x, y, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- plotModel(data, type, x, y)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- do.call(grid.arrange, plot_list)
  return(final_plot)
}


compareDemand <- function(data, population, sample = NA, n = 3) {
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- demandPlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  return(final_plot)
}


compareRevenue <- function(data, population, sample = NA, n = 3) {
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- revenuePlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  return(final_plot)
}
#Test



v <- 20
f <- 10000
Pop <- 1e7

# for internal code use
costFun <- function(variable, fixed, fQ) {
  fC <- function(p) fixed + variable * fQ(p)
  return(fC)
}

# for student use
costFunction <- function(data, type, variable, fixed, population, sample = NA){
  fQ <- demandFunction(data, type, population, sample)
  if(class(fQ) == "logical") return(NA)
  return(costFun(variable, fixed, fQ))
}


# for internal code use
profitFun <- function(fR, fC){
  fPi <- function(p) fR(p) - fC(p)
  return(fPi)
}

# for student use
profitFunction <- function(data, type, variable, fixed, population, sample = NA){
  fQ <- demandFunction(data, type, population, sample)
  if(class(fQ) == "logical") return(NA)
  fR <- revenueFunction(data, type, population, sample)
  fC <- costFun(variable, fixed, fQ)
  fPi <- function(p) fR(p) - fC(p)
  return(fPi)
}


optimizeProfit <- function(data, type, variable, fixed, population, sample = NA){

  fQ <- demandFunction(data, type, population, sample)
  if(class(fQ) == "logical") return(NA)
  fR <- revenueFunction(data, type, population, sample)
  fC <- costFun(variable, fixed, fQ)
  fPi <- profitFun(fR, fC)

  opt_Profit <- (optimize(fPi, lower = 0, upper = data$wtp, maximum = TRUE )[[2]])
  opt_Price <- round(optimize(fPi, lower = 0, upper = data$wtp, maximum = TRUE )[[1]],2)

  return(list(opt_Profit, opt_Price))
}



profitPlot <- function(data, type, variable, fixed, population, sampleSize = NA, yCap = 0){
  fQ <- demandFunction(data, type, population, sampleSize)
  if(class(fQ) == "logical") return(NA)
  fR <- revenueFunction(data, type, population, sampleSize)
  fC <- costFun(variable, fixed, fQ)
  fPi <- profitFun(fR, fC)

  title <- paste("Profit:", type)

  opt_Profit <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]
  opt_Price <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[1]]
  opt_Revenue <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]

  show_Profit <- convNum(opt_Profit)
  show_Price <- convNum(opt_Price)

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
             label = paste("P:", show_Price),
             vjust = 1.5, hjust = 1.5, size = 3,
             color = "forestgreen", alpha = .8) +
    annotate("text", x = Inf, y = Inf,
             label =(paste("Pi:", show_Profit)),
             vjust = 3.5, hjust = 1.3, size = 3,
             color = "forestgreen", alpha = .8) +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme_minimal()+
    scale_y_continuous(labels = label_number_si(),
                       breaks = scales::extended_breaks(),
                       limits = c(0, yCap))+
    labs(title = title)

  return(plot)
}
#Test



nBestProfitPlots <- function(data, n, variable, fixed, pop, sample = NA){
  top_models<-nBestDemandModels(data, n)
  plot_list <- list()

  for (model in top_models) {
    nextPlot <- profitPlot(data, model, variable, fixed, pop, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  final_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  return(final_plot)
}


compareProfit <- function(data, variable, fixed, population, sample = NA) nBestProfitPlots(data, 3, variable, fixed, population, sample)


#Test

















