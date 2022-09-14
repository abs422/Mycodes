packages = c("openxlsx", "tables", "dplyr","here")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

require(openxlsx)
require(tables)
require(dplyr)

descriptive <-
  function(ADS,
           vars,
           type = "categorical",
           strata,
           remove_zeroes = F,
           numeric_summary = "min+mean+median+sd+max",
           percent = "col",
           vars_label = c(),
           test = NULL) {
    ############################## Checking the inputs #############################
    
    #checking if the input is of type dataframe
    if (!is.data.frame(ADS)) {
      stop("The ADS should be of type data frame")
    }
    
    
    #checking the length of the label
    if (!is.null(vars_label)) {
      if (length(vars_label) != length(vars)) {
        stop(
          "The number of elements in the 'vars_label' vector is not equal to
          number of elements in the 'vars' vector"
        )
      }
      }
    
    #checking if the type has only allowed values
    if (!(type == "categorical" | type == "continuous" |
          type == "binary")) {
      stop("Unexpected value for type. Try 'categorical' or 'binary' or 'continuous'")
    }
    
    
    #checking if the variable entered exists in the dataset
    for (i in c(vars, unique(unlist(strata)))) {
      if (!i %in% colnames(ADS)) {
        message <-
          paste("The variable named '",
                i,
                "' doesn't exist in the input data frame")
        stop(message)
      }
    }
    
    #converting the variables to factors
    ADS[c(unique(unlist(strata)))] <- lapply(ADS[c(unique(unlist(strata)))], as.character)
    
    #Replacing strata has NA values with "NA"
    for(i in c(unique(unlist(strata)))){
      ADS[is.na(ADS[,i]), i] <- "NA"
    }
    
    
    #checking if strata has atleast two levels
    for (i in c(unique(unlist(strata)))) {
      if (length(unique(ADS[,i]))<2) {
        message <-
          paste(
            "The stratification variable named '",
            i,
            "' has only one level. The stratification variables must have atleast two levels"
          )
        stop(message)
      }
    }
    
    
    #checking if the test has only values that are allowed
    if (!(is.null(test))) {
      if (!(test == "chi.sq" | test == "fisher")) {
        stop("test has values that are not expected, try 'chi.sq' or 'fisher'")
      }else{
        if(sum(is.na(ADS[,vars]))>0){
          stop("Chi square test won't work with NA values, remove them and try again")
        }
      }
    }
    
    #checking if chi.sq is applicable
    if (!is.null(test)) {
      if (test == "chi.sq") {
        if (type == "continuous") {
          stop("Chi square test is not applicable for continuous variables")
        }
      }
      
      if (length(unlist(strata)) > 1) {
        stop("Chi square works with only one level of stratification as of now")
      }
    }
    
    #####################################################################################
    
    if (type == "categorical") {
      #adding an empty column
      ADS$empty <- "emptyspace"
      
      #converting the variables to factors
      ADS[vars] <- lapply(ADS[vars], as.character)
      
      #Replace NAs with a "NA" string
      for(i in vars){
        ADS[is.na(ADS[,i]), i] <- "NA"
      }
      
      
      #checking if the levels are alright
      vars_with_1_level <- c()
      labels_to_remove <- c()
      for (i in vars) {
        if (length(unique(ADS[, i])) == 1) {
          vars_with_1_level <- c(vars_with_1_level, i)
          labels_to_remove <-
            c(labels_to_remove, vars_label[match(i, vars)])
          message <-
            paste(
              "The variable",
              i,
              "has only 1 level and it has been removed. All input variables must have atleast two levels"
            )
          warning(message)
        }
      }
      vars <- vars[!vars %in% vars_with_1_level]
      vars_label <- vars_label[!vars_label %in% labels_to_remove]
      
      ADS[vars] <- lapply(ADS[vars], as.factor)
      ADS[unique(unlist(strata))] <-
        lapply(ADS[unique(unlist(strata))], as.factor)
      
      
      
      #creating the percentage function
      perc <- function(x) {
        x / 100
      }
      
      
      column <- ""
      if (is.null(percent)) {
        
      } else if (percent == "row") {
        strata_count <- 0
        for (list in strata) {
          all = '(1+Format(perc()) * Percent("row")+'
          if (column != "") {
            column <- paste(column, "+")
          }
          if (strata_count == 0) {
            column <- paste(column, all)
          }
          
          level_count <- 0
          for (level in list) {
            if (level_count == 0) {
              column <- paste(column, level, " * ")
            } else{
              column <-
                paste(column,
                      '(1+Format(perc()) * Percent("row")+',
                      level,
                      ' * ')
            }
            
            level_count <- level_count + 1
          }
          if (level_count == 1) {
            column <- paste(column, '(1+Format(perc()) * Percent("row"))')
          } else{
            column <- paste(column, '(1+Format(perc()) * Percent("row"))')
          }
          start_bracket <-
            sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
          end_bracket <-
            sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
          if (end_bracket - start_bracket != 0) {
            column <-
              paste(column, paste(rep(
                ")", start_bracket - end_bracket - 1
              ), collapse = ""))
          }
          strata_count <- strata_count + 1
        }
        start_bracket <-
          sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
        end_bracket <-
          sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
        if (end_bracket - start_bracket != 0) {
          column <-
            paste(column, paste(rep(")", start_bracket - end_bracket), collapse = ""))
        }
        
        
      } else{
        strata_count <- 0
        for (list in strata) {
          all = '(1+Format(perc()) * Percent("col")+'
          if (column != "") {
            column <- paste(column, "+")
          }
          if (strata_count == 0) {
            column <- paste(column, all)
          }
          
          level_count <- 0
          for (level in list) {
            if (level_count == 0) {
              column <- paste(column, level, " * ")
            } else{
              column <-
                paste(column,
                      '(1+Format(perc()) * Percent("col")+',
                      level,
                      ' * ')
            }
            
            level_count <- level_count + 1
          }
          if (level_count == 1) {
            column <- paste(column, '(1+Format(perc()) * Percent("col"))')
          } else{
            column <- paste(column, '(1+Format(perc()) * Percent("col"))')
          }
          start_bracket <-
            sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
          end_bracket <-
            sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
          if (end_bracket - start_bracket != 0) {
            column <-
              paste(column, paste(rep(
                ")", start_bracket - end_bracket - 1
              ), collapse = ""))
          }
          strata_count <- strata_count + 1
        }
        start_bracket <-
          sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
        end_bracket <-
          sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
        if (end_bracket - start_bracket != 0) {
          column <-
            paste(column, paste(rep(")", start_bracket - end_bracket), collapse = ""))
        }
        
        
      }
      rows <- c(rbind(rep("empty", length(vars)), vars))
      
      formula <- paste('1+',
                       paste(rows, collapse = "+"), '~',
                       column)
      
      
      
      #creating the tabular
      desc <- tables::tabular(as.formula(formula), ADS)
      # write.csv.tabular(desc,"desc.csv")
      
      #converting the tabular to a data frame
      desc_df <- as.data.frame.matrix(desc)
      desc_df[] <- lapply(desc_df[], as.numeric)
      
      
      #adding row labels
      desc_df$variables <-
        as.data.frame.matrix(rowLabels(desc))[, 1]
      desc_df$labels <- as.data.frame.matrix(rowLabels(desc))[, 2]
      
      #re-arranging the dataframe
      desc_df <-
        desc_df[, c(ncol(desc_df) - 1, ncol(desc_df), 1:(ncol(desc_df) - 2))]
      
      #creating the empty row
      desc_df[desc_df$labels == "empty", ] <- NA
      
      #merging the variable and it's label in a single column
      desc_df$variables <- dplyr::lead(desc_df$variables)
      
      
      desc_df[, c(1, 2)] <-
        lapply(desc_df[, c(1, 2)][], as.character)
      desc_df$labels <-
        ifelse(
          desc_df$labels == "" | is.na(desc_df$labels),
          as.character(desc_df$variables),
          desc_df$labels
        )
      
      #removing the zeroes
      # if (remove_zeroes) {
      #  desc_df <- desc_df[desc_df$labels != 0,]
      # }
      #removing the variable column
      desc_df <- desc_df[, -1]
      
      #adding the column labels
      desc_heading <- as.data.frame.matrix(colLabels(desc))
      desc_heading[] <- lapply(desc_heading[], as.character)
      desc_heading[(max(lengths(strata)) * 2), c(1, 2)] <- "Overall"
      desc_columns <-
        sapply(desc_heading[], function(x)
          paste(x, collapse = " | "))
      V0 <- rep("Variables", nrow(desc_heading))
      desc_heading <- cbind(V0 , desc_heading)
      desc_heading[c(1:(max(lengths(strata)) * 2)), c(2, 3)] <-
        "Overall"
      
      #filling the empty values
      for (j in 1:ncol(desc_heading)) {
        for (i in 1:nrow(desc_heading)) {
          non_empty_row <- 0
          if (desc_heading[i, j] == "") {
            for (k in (i + 1):nrow(desc_heading)) {
              if (desc_heading[k, j] != "") {
                non_empty_row <- k
                break()
              }
            }
            if (desc_heading[k, j] == "All" |
                desc_heading[k, j] == "Percent") {
              desc_heading[i, j] <- "Overall"
            }
            else{
              desc_heading[i, j] <- desc_heading[k, j]
            }
            
          }
        }
        if (desc_heading[nrow(desc_heading), j] == "All") {
          desc_heading[nrow(desc_heading), j] <- "N"
        }
        if (desc_heading[nrow(desc_heading), j] == "Percent") {
          desc_heading[nrow(desc_heading), j] <- "%"
        }
      }
      
      
      desc_columns[1] <- gsub(".*\\| O", "O", desc_columns[1])
      desc_columns[2] <- gsub(".*\\| O", "O", desc_columns[2])
      colnames(desc_df) <- c("Variables", desc_columns)
      
      #dividing the percentage column by 100
      desc_df[, which(grepl("Percent", colnames(desc_df)))] <-
        desc_df[, which(grepl("Percent", colnames(desc_df)))] / 100
      
      if (!is.null(test)) {
        #adding the chi.square value
        if (test == "chi.sq") {
          chisq_vec <- c()
          for (i in vars) {
            chisq_vec <- c(chisq_vec,
                           sprintf(chisq.test(c(ADS[, i]),
                                              ADS[, unlist(strata[[1]])])$p.value,
                                   fmt = '%#.2f'))
          }
          chisq_df <-
            as.data.frame(cbind(Variables = vars, p_value = chisq_vec))
          desc_df$Chi.sq <- NA
          for (i in 1:nrow(desc_df)) {
            for (j in 1:nrow(chisq_df)) {
              if (desc_df[i, "Variables"] == chisq_df[j, "Variables"]) {
                desc_df[i, "Chi.sq"] = as.numeric(as.character(chisq_df[j, "p_value"]))
              }
            }
          }
          #adding the chisquare column in the heading
          desc_heading$Chi.sq <- "Chi.sq"
          desc_heading[nrow(desc_heading), "Chi.sq"] <- "p-value"
        }
      }
      
      #changing the variable name with their labels
      if (!is.null(vars_label)) {
        desc_df[, 1] <-
          replace(desc_df[, 1], match(vars, desc_df[, 1]), vars_label)
      }
      
      #returning the final data frame
      return(list(
        table = desc_df,
        heading = desc_heading,
        tabular = desc,
        type = type
      ))
      
    }
    else if (type == "continuous") {
      mean<-function(x) base::mean(x,na.rm=TRUE)
      median<-function(x) stats::median(x,na.rm=TRUE)
      sd<-function(x) stats::sd(x,na.rm=TRUE)
      sum<-function(x) base::sum(x,na.rm=TRUE)
      min<-function(x) base::min(x,na.rm=TRUE)
      max<-function(x) base::max(x,na.rm=TRUE)
      All<-function(x) length(x[!is.na(x)])
      #converting the variables to factors/numeric
      for (i in vars) {
        if (!is.numeric(ADS[, i])) {
          ADS[, i] <- as.character(ADS[, i])
          ADS[, i] <- as.numeric(ADS[, i])
        }
      }
      ADS[unique(unlist(strata))] <-
        lapply(ADS[unique(unlist(strata))], as.factor)
      
      summary <- paste("(All+", numeric_summary, ")")
      rows <- vars
      row_formula <- "1+"
      for (i in 1:length(rows)) {
        row_formula <- paste(row_formula, rows[i], "*", summary, "+")
      }
      row_formula <- substr(row_formula, 1, nchar(row_formula) - 1)
      
      column_formula <- "1"
      for (i in strata) {
        count <- 0
        for (j in i) {
          count <- count + 1
          temp <- paste("(", j, ")")
          if (count == 1) {
            k <- temp
          } else{
            pos <- regexpr(pattern = ')', k)[1]
            k <-
              paste(
                substr(k, 1, pos - 1),
                "*",
                substr(temp, 1, 1),
                "1+",
                substr(temp, 2, nchar(temp)),
                substr(k, pos, nchar(k))
              )
          }
        }
        column_formula <- paste(column_formula, "+", k)
      }
      formula <- paste(row_formula, "~", column_formula)
      
      options(scipen = 999)
      #creating the tabular
      
      desc <- tables::tabular(as.formula(formula), ADS)
      
      #converting the tabular to a data frame
      desc_df <- as.data.frame.matrix(desc)
      desc_df[] <- lapply(desc_df[], as.numeric)
      
      #adding row labels
      desc_df$variables <-
        as.data.frame.matrix(rowLabels(desc))[, 1]
      desc_df$labels <- as.data.frame.matrix(rowLabels(desc))[, 2]
      
      #re-arranging the dataframe
      desc_df <-
        desc_df[, c(ncol(desc_df) - 1, ncol(desc_df), 1:(ncol(desc_df) - 2))]
      
      #converting the first two columns to characters
      desc_df[, c(1, 2)] <-
        lapply(desc_df[, c(1, 2)][], as.character)
      
      #adding the column labels
      desc_heading <- as.data.frame.matrix(colLabels(desc))
      desc_heading[] <- lapply(desc_heading[], as.character)
      desc_columns <-
        sapply(desc_heading[], function(x)
          paste(x, collapse = " | "))
      V00 <- rep("Variables", nrow(desc_heading))
      V01 <- rep("Labels", nrow(desc_heading))
      desc_heading <- cbind(V00, V01, desc_heading)
      desc_heading[c(1:(max(lengths(strata)) * 2)), c(3)] <-
        "Overall"
      
      
      #filling the empty values
      for (j in 1:ncol(desc_heading)) {
        for (i in 1:nrow(desc_heading)) {
          non_empty_row <- 0
          if (desc_heading[i, j] == "") {
            for (k in (i + 1):nrow(desc_heading)) {
              if (desc_heading[k, j] != "") {
                non_empty_row <- k
                break()
              }
            }
            if (desc_heading[k, j] == "All" |
                desc_heading[k, j] == "Percent") {
              desc_heading[i, j] <- "Overall"
            }
            else{
              desc_heading[i, j] <- desc_heading[k, j]
            }
            
          }
        }
        if (desc_heading[nrow(desc_heading), j] == "All") {
          desc_heading[nrow(desc_heading), j] <- "Overall"
        }
      }
      
      
      desc_columns[1] <- gsub(".*\\| A", "A", desc_columns[1])
      colnames(desc_df) <- c("Variables", "Labels", desc_columns)
      desc_df[1, 1] <- "All"
      
      #changing the variable name with their labels
      if (!is.null(vars_label)) {
        desc_df[, 1] <-
          replace(desc_df[, 1], match(vars, desc_df[, 1]), vars_label)
      }
      
      #returning the final data frame
      return(list(
        table = desc_df,
        heading = desc_heading,
        tabular = desc,
        type = type
      ))
      
    }
    else{
      #adding an empty column
      ADS$empty <- "emptyspace"
      
      #converting the variables to factors
      ADS[vars] <- lapply(ADS[vars], as.character)
      
      
      #Replace NAs with a "NA" string
      for(i in vars){
        if(sum(is.na(ADS[,i]))>0){
          message<-paste(
            "The variable named '",
            i,
            "' has NAs and it will be replaced with 0"
          )
          warning(message)
        }
        ADS[is.na(ADS[,i]), i] <- 0
      }
      
      
      #checking if the levels are alright
      vars_with_1_level <- c()
      labels_to_remove <- c()
      for (i in vars) {
        if (length(unique(ADS[, i])) == 1) {
          vars_with_1_level <- c(vars_with_1_level, i)
          labels_to_remove <-
            c(labels_to_remove, vars_label[match(i, vars)])
          message <-
            paste(
              "The variable",
              i,
              "has only 1 level and it has been removed. All input variables must have atleast two levels"
            )
          warning(message)
        }
      }
      vars <- vars[!vars %in% vars_with_1_level]
      vars_label <- vars_label[!vars_label %in% labels_to_remove]
      
      ADS[vars] <- lapply(ADS[vars], as.factor)
      ADS[unique(unlist(strata))] <-
        lapply(ADS[unique(unlist(strata))], as.factor)
      
      #creating the percentage function
      perc <- function(x) {
        x / 100
      }
      column <- ""
      if (is.null(percent)) {
        
      } else if (percent == "row") {
        for (list in strata) {
          all = '(1+Format(perc()) * Percent("row")+'
          if (column != "") {
            column <- paste(column, "+")
          }
          column <- paste(column, all)
          level_count <- 0
          for (level in list) {
            if (level_count == 0) {
              column <- paste(column, level, " * ")
            } else{
              column <-
                paste(column,
                      '(1+Format(perc()) * Percent("row")+',
                      level,
                      ' * ')
            }
            
            level_count <- level_count + 1
          }
          if (level_count == 1) {
            column <- paste(column, '(1+Format(perc()) * Percent("row")))')
          } else{
            column <- paste(column, '(1+Format(perc()) * Percent("row"))')
          }
          start_bracket <-
            sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
          end_bracket <-
            sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
          column <-
            paste(column, paste(rep(
              ")", start_bracket - end_bracket - 1
            ), collapse = ""))
        }
        start_bracket <-
          sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
        end_bracket <-
          sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
        column <-
          paste(column, paste(rep(")", start_bracket - end_bracket), collapse = ""))
        
      } else{
        strata_count <- 0
        for (list in strata) {
          all = '(1+Format(perc()) * Percent("col")+'
          if (column != "") {
            column <- paste(column, "+")
          }
          if (strata_count == 0) {
            column <- paste(column, all)
          }
          
          level_count <- 0
          for (level in list) {
            if (level_count == 0) {
              column <- paste(column, level, " * ")
            } else{
              column <-
                paste(column,
                      '(1+Format(perc()) * Percent("col")+',
                      level,
                      ' * ')
            }
            
            level_count <- level_count + 1
          }
          if (level_count == 1) {
            column <- paste(column, '(1+Format(perc()) * Percent("col"))')
          } else{
            column <- paste(column, '(1+Format(perc()) * Percent("col"))')
          }
          start_bracket <-
            sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
          end_bracket <-
            sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
          if (end_bracket - start_bracket != 0) {
            column <-
              paste(column, paste(rep(
                ")", start_bracket - end_bracket - 1
              ), collapse = ""))
          }
          strata_count <- strata_count + 1
        }
        start_bracket <-
          sum(gregexpr("(", column, fixed = TRUE)[[1]] > 0)
        end_bracket <-
          sum(gregexpr(")", column, fixed = TRUE)[[1]] > 0)
        if (end_bracket - start_bracket != 0) {
          column <-
            paste(column, paste(rep(")", start_bracket - end_bracket), collapse = ""))
        }
        
        
      }
      rows <- c(rbind(rep("empty", length(vars)), vars))
      
      formula <- paste('1+',
                       paste(rows, collapse = "+"), '~',
                       column)
      
      
      #creating the tabular
      desc <- tables::tabular(as.formula(formula), ADS)
      
      #converting the tabular to a data frame
      desc_df <- as.data.frame.matrix(desc)
      desc_df[] <- lapply(desc_df[], as.numeric)
      
      
      #adding row labels
      desc_df$variables <-
        as.data.frame.matrix(rowLabels(desc))[, 1]
      desc_df$labels <- as.data.frame.matrix(rowLabels(desc))[, 2]
      
      #re-arranging the dataframe
      desc_df <-
        desc_df[, c(ncol(desc_df) - 1, ncol(desc_df), 1:(ncol(desc_df) - 2))]
      
      #creating the empty row
      desc_df[desc_df$labels == "empty", ] <- NA
      
      #merging the variable and it's label in a single column
      desc_df$variables <- dplyr::lag(desc_df$variables)
      
      
      desc_df[, c(1, 2)] <-
        lapply(desc_df[, c(1, 2)][], as.character)
      desc_df <-
        desc_df[(desc_df$labels == 1 |
                   desc_df$labels == "All") &
                  !is.na(desc_df$labels), ]
      
      desc_df$labels <-
        ifelse(
          desc_df$labels == "" | is.na(desc_df$labels) | desc_df$labels == 1,
          as.character(desc_df$variables),
          desc_df$labels
        )
      
      
      #removing the variable column
      desc_df <- desc_df[, -1]
      
      #adding the column labels
      desc_heading <- as.data.frame.matrix(colLabels(desc))
      desc_heading[] <- lapply(desc_heading[], as.character)
      desc_heading[max(lengths(strata)) * 2, c(1, 2)] <- "Overall"
      desc_columns <-
        sapply(desc_heading[], function(x)
          paste(x, collapse = " | "))
      V0 <- rep("Variables", nrow(desc_heading))
      desc_heading <- cbind(V0 , desc_heading)
      desc_heading[c(1:(max(lengths(strata)) * 2)), c(2, 3)] <-
        "Overall"
      #filling the empty values
      for (j in 1:ncol(desc_heading)) {
        for (i in 1:nrow(desc_heading)) {
          non_empty_row <- 0
          if (desc_heading[i, j] == "") {
            for (k in (i + 1):nrow(desc_heading)) {
              if (desc_heading[k, j] != "") {
                non_empty_row <- k
                break()
              }
            }
            if (desc_heading[k, j] == "All" |
                desc_heading[k, j] == "Percent") {
              desc_heading[i, j] <- "Overall"
            }
            else{
              desc_heading[i, j] <- desc_heading[k, j]
            }
            
          }
        }
        if (desc_heading[nrow(desc_heading), j] == "All") {
          desc_heading[nrow(desc_heading), j] <- "N"
        }
        if (desc_heading[nrow(desc_heading), j] == "Percent") {
          desc_heading[nrow(desc_heading), j] <- "%"
        }
      }
      desc_columns[1] <- gsub(".*\\| O", "O", desc_columns[1])
      desc_columns[2] <- gsub(".*\\| O", "O", desc_columns[2])
      colnames(desc_df) <- c("Variables", desc_columns)
      
      #dividing the percentage column by 100
      desc_df[, which(grepl("Percent", colnames(desc_df)))] <-
        desc_df[, which(grepl("Percent", colnames(desc_df)))] / 100
      
      #adding the chi.square value
      if (!is.null(test)) {
        if (test == "chi.sq") {
          chisq_vec <- c()
          for (i in vars) {
            chisq_vec <- c(chisq_vec,
                           sprintf(chisq.test(c(ADS[, i]),
                                              ADS[, unlist(strata[[1]])])$p.value,
                                   fmt = '%#.2f'))
          }
          chisq_df <-
            as.data.frame(cbind(Variables = vars, p_value = chisq_vec))
          desc_df$Chi.sq <- NA
          for (i in 1:nrow(desc_df)) {
            for (j in 1:nrow(chisq_df)) {
              if (desc_df[i, "Variables"] == chisq_df[j, "Variables"]) {
                desc_df[i, "Chi.sq"] = as.numeric(as.character(chisq_df[j, "p_value"]))
              }
            }
          }
          #adding the chisquare column in the heading
          desc_heading$Chi.sq <- "Chi.sq"
          desc_heading[nrow(desc_heading), "Chi.sq"] <- "p-value"
        }
      }
      
      
      
      #changing the variable name with their labels
      if (!is.null(vars_label)) {
        desc_df[, 1] <-
          replace(desc_df[, 1], match(vars, desc_df[, 1]), vars_label)
      }
      
      #returning the final data frame
      return(list(
        table = desc_df,
        heading = desc_heading,
        tabular = desc,
        type = type
      ))
    }
    
    
    }

sheet <- function(tables = c(),
                  subtitle = "Subtitle",
                  sheetname = "sheet1") {
  return(list(tables, subtitle, sheetname))
  
}

wb <- function(x, ...) {
  return(list(x, ...))
}

export <-
  function(Workbook,
           file_name,
           index = T,
           title = "Descriptive report",
           heading_fill = "#C00000") {
    
    if(is.null(file_name)){
      file_name<-title
    }
    
    file_name <- gsub("[[:blank:]]", "_", file_name)
    if (grepl(".xlsx", file_name) == FALSE) {
      file_name <- paste(file_name, ".xlsx", sep = "")
    }
    
    #defining the formats
    hs2 <- openxlsx::createStyle(
      fontColour = "#ffffff",
      fgFill = heading_fill,
      halign = "center",
      valign = "center",
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle = getOption("openxlsx.borderStyle", "thin")
    )
    titleStyle <- openxlsx::createStyle(
      fontSize = 18,
      fontColour = "black",
      textDecoration = "bold"
    )
    subtitleStyle <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "black",
      textDecoration = "bold"
    )
    bold <- openxlsx::createStyle(textDecoration = "bold")
    grey_bg <- openxlsx::createStyle(
      fgFill = "#F2F2F2",
      bgFill = NULL,
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle = getOption("openxlsx.borderStyle", "thin")
    )
    bold_grey <- openxlsx::createStyle(
      fgFill = "#F2F2F2",
      bgFill = NULL,
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle = getOption("openxlsx.borderStyle", "thin")
    )
    bold_grey_center <- openxlsx::createStyle(
      fgFill = "#F2F2F2",
      bgFill = NULL,
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      halign = "center",
      valign = "center",
      borderStyle = getOption("openxlsx.borderStyle", "thin")
    )
    borderStyle <- openxlsx::createStyle(
      halign = "CENTER",
      border = "Top",
      borderColour = "red",
      borderStyle = "thick"
    )
    # % style
    percentColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##0.00%",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    
    # Add N style
    NColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    # Add N.00 style
    NDecColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##0.00",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    # % style
    percentBoldColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##0.00%",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = "bold",
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    
    # Add N style
    NBoldColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = "bold",
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    # Add N.00 style
    NDecBoldColStyle <- openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = "11",
      fontColour = "black",
      numFmt = "#,##0.00",
      border = "TopBottomLeftRight",
      borderColour = "#A9A9A9",
      borderStyle =
        getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = "bold",
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
    # JnJ logo
    novartis_logo <-
      "/mnt/share/home/srivaabg/Macro Backend/Descriptive/novartis_logo.png"
    
    
    #creating the workbook
    outwb <- createWorkbook()
    
    if (index) {
      #get the name of all the worksheets
      sheet_names <- c()
      subtitle <- "Index page"
      for (sheet in workbook) {
        sheet_names <- c(sheet_names, unlist(sheet[3]))
      }
      index_table <-
        as.data.frame(rbind(cbind(Sheet = sheet_names, Link = sheet_names)))
      
      #adding the worksheet
      openxlsx::addWorksheet(outwb, "Index", zoom = 90)
      openxlsx::showGridLines(outwb, "Index", showGridLines = FALSE)
      
      #writing the title and the subtitles
      openxlsx::insertImage(
        outwb,
        "Index",
        novartis_logo,
        startRow = 1,
        startCol = 1,
        width = 0.7,
        height = 0.8,
        units = "in"
      )
      openxlsx::writeData(
        outwb,
        "Index",
        title,
        startCol = 2,
        startRow = 1,
        borders = "none"
      )
      openxlsx::addStyle(
        outwb,
        sheet = "Index",
        titleStyle,
        rows = 1,
        cols = 2,
        gridExpand = TRUE
      )
      openxlsx::addStyle(
        outwb,
        sheet = "Index",
        borderStyle,
        rows = 2,
        cols = 2:16384,
        gridExpand = TRUE
      )
      openxlsx::writeData(
        outwb,
        "Index",
        subtitle,
        startCol = 2,
        headerStyle = subtitleStyle,
        startRow = 3,
        borders = "none"
      )
      openxlsx::addStyle(
        outwb,
        sheet = "Index",
        subtitleStyle,
        rows = 3,
        cols = 2,
        gridExpand = TRUE
      )
      #writing the data to the worksheet
      openxlsx::writeData(
        outwb,
        "Index",
        index_table,
        startCol = 2,
        startRow = 7,
        borders = "all",
        borderColour = "#bfbfbf",
        headerStyle = hs2,
        borderStyle = "thin"
      )
      
      for (i in 1:nrow(index_table)) {
        x <- index_table[i, 1]
        writeFormula(
          outwb,
          "Index",
          startRow = 7 + i,
          startCol = 3,
          x = makeHyperlinkString(
            sheet = x,
            row = 1,
            col = 1,
            text = x
          )
        )
      }
      width_vec <- apply(index_table, 2,
                         function(x)
                           max(nchar(as.character(x)) + 1,
                               na.rm = TRUE))
      width_vec_header <- nchar(colnames(index_table)) + 1
      max_vec_header_param <- pmax(width_vec, width_vec_header)
      openxlsx::setColWidths(outwb,
                             "Index",
                             cols = 2:(ncol(index_table) + 1),
                             widths = max_vec_header_param)
      
      
    }
    
    
    #going through each sheet
    
    for (sheet in workbook) {
      #subtitle
      subtitle <- unlist(sheet[2])
      #sheet name
      sheet_name <- unlist(sheet[3])
      #sheet type
      sheet_type <- unlist(sheet[[1]]$type)
      #heading name
      table_heading <- sheet[[1]]$heading
      #table
      table <- as.data.frame(sheet[[1]]$table)
      
      #adding the worksheet
      openxlsx::addWorksheet(outwb, sheet_name, zoom = 90)
      openxlsx::showGridLines(outwb, sheet_name, showGridLines = FALSE)
      
      #writing the title and the subtitles
      openxlsx::insertImage(
        outwb,
        sheet_name,
        novartis_logo,
        startRow = 1,
        startCol = 1,
        width = 0.7,
        height = 0.8,
        units = "in"
      )
      openxlsx::writeData(
        outwb,
        sheet_name,
        title,
        startCol = 2,
        startRow = 1,
        borders = "none"
      )
      openxlsx::addStyle(
        outwb,
        sheet = sheet_name,
        titleStyle,
        rows = 1,
        cols = 2,
        gridExpand = TRUE
      )
      openxlsx::addStyle(
        outwb,
        sheet = sheet_name,
        borderStyle,
        rows = 2,
        cols = 2:16384,
        gridExpand = TRUE
      )
      openxlsx::writeData(
        outwb,
        sheet_name,
        subtitle,
        startCol = 2,
        headerStyle = subtitleStyle,
        startRow = 3,
        borders = "none"
      )
      openxlsx::addStyle(
        outwb,
        sheet = sheet_name,
        subtitleStyle,
        rows = 3,
        cols = 2,
        gridExpand = TRUE
      )
      
      
      #writing the data to the worksheet
      openxlsx::writeData(
        outwb,
        sheet_name,
        table,
        startCol = 2,
        startRow = 7 + nrow(sheet[[1]]$heading),
        borders = "all",
        borderColour = "#bfbfbf",
        headerStyle = hs2,
        borderStyle = "thin"
      )
      
      #writing the heading
      openxlsx::writeData(
        outwb,
        sheet_name,
        table_heading,
        startCol = 2,
        startRow = 7,
        borders = "all",
        borderColour = "#bfbfbf",
        borderStyle = "thin"
      )
      
      if (sheet_type == "categorical" | sheet_type == "binary") {
        #writing the logic to merge
        tfdf <-
          as.data.frame(matrix(
            rep(F, nrow(table_heading) * ncol(table_heading)),
            nrow = nrow(table_heading),
            ncol = ncol(table_heading)
          ))
        j <- 1
        while (j <= ncol(table_heading)) {
          merge_column_start <- j
          merge_column_end <- j
          i <- 1
          while (i < nrow(table_heading)) {
            if (tfdf[i, j] == T) {
              i <- i + 1
              next()
            }
            temp <- table_heading[i, j]
            merge_row_start <- i
            merge_row_end <- i
            merge_column_end <- j
            for (k in ((i + 1):(nrow(table_heading) - 1))) {
              if (table_heading[k, j] == temp) {
                merge_row_end <- k
                next()
              } else{
                break()
              }
            }
            while ((all(unique(table_heading[merge_row_start:merge_row_end, merge_column_end + 1]) ==
                        rep(temp, length(
                          unique(table_heading[merge_row_start:merge_row_end, merge_column_end + 1])
                        )))) &
                   merge_column_end < ncol(table_heading)) {
              merge_column_end <- merge_column_end + 1
            }
            if (merge_column_end == 1) {
              openxlsx::mergeCells(
                outwb,
                sheet_name,
                cols = 1 + c(merge_column_start:merge_column_end),
                rows = 7 + c(merge_row_start:(merge_row_end +
                                                1))
              )
            } else{
              openxlsx::mergeCells(
                outwb,
                sheet_name,
                cols = 1 + c(merge_column_start:merge_column_end),
                rows = 7 + c(merge_row_start:merge_row_end)
              )
            }
            
            tfdf[merge_row_start:merge_row_end, merge_column_start:merge_column_end] <-
              T
            i <- i + 1
          }
          j <- j + 1
        }
        
        
        # Add N style to table
        for (i in (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) +
                                             7)) {
          openxlsx::addStyle(
            outwb,
            sheet_name,
            NColStyle,
            rows = i,
            cols = c(1 + which(grepl(
              "All", colnames(table)
            ))),
            gridExpand = TRUE,
            stack = FALSE
          )
        }
        # Add % style to table
        for (i in (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) +
                                             7)) {
          openxlsx::addStyle(
            outwb,
            sheet_name,
            percentColStyle,
            rows = i,
            cols = c(1 + which(grepl(
              "Percent", colnames(table)
            ))),
            gridExpand = TRUE,
            stack = FALSE
          )
          
        }
        #Adding format style to the table header
        openxlsx::addStyle(
          outwb,
          sheet = sheet_name,
          hs2,
          rows = 8:(nrow(table_heading) + 7),
          cols = 2:(ncol(table_heading) + 1),
          gridExpand = TRUE
        )
        deleteData(
          outwb,
          sheet = sheet_name,
          cols = 2:(2 + ncol(table_heading)),
          rows = 7,
          gridExpand = TRUE
        )
        
        
        if (sum(rowSums(is.na(table))) == 0) {
          grey_style <- bold_grey
        } else{
          grey_style <- grey_bg
        }
        
        #adding grey to the labels
        addStyle(
          outwb,
          sheet = sheet_name,
          grey_style,
          rows = (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) +
                                              7),
          cols = 2,
          gridExpand = TRUE
        )
        
        #adding bold grey to the variables
        addStyle(
          outwb,
          sheet = sheet_name,
          bold_grey,
          rows = c((7 + nrow(
            table_heading
          ) + (
            which(is.na(table[, 2]))
          ))),
          cols = 2:(ncol(table) + 1),
          gridExpand = TRUE
        )
        
        # Add N style to table
        
        openxlsx::addStyle(
          outwb,
          sheet_name,
          NBoldColStyle,
          rows = 8 + nrow(table_heading),
          cols = c(1 + which(grepl(
            "All", colnames(table)
          ))),
          gridExpand = TRUE,
          stack = FALSE
        )
        # Add % style to table
        
        openxlsx::addStyle(
          outwb,
          sheet_name,
          percentBoldColStyle,
          rows = 8 + nrow(table_heading),
          cols = c(1 + which(grepl(
            "Percent", colnames(table)
          ))),
          gridExpand = TRUE,
          stack = FALSE
        )
        
        openxlsx::addStyle(
          outwb,
          sheet_name,
          bold_grey,
          rows = 8 + nrow(table_heading) ,
          cols = 2,
          gridExpand = TRUE,
          stack = FALSE
        )
        
        
        
      } else{
        #writing the logic to merge
        tfdf <-
          as.data.frame(matrix(
            rep(F, nrow(table_heading) * ncol(table_heading)),
            nrow = nrow(table_heading),
            ncol = ncol(table_heading)
          ))
        j <- 1
        while (j <= ncol(table_heading)) {
          merge_column_start <- j
          merge_column_end <- j
          i <- 1
          while (i < nrow(table_heading)) {
            if (tfdf[i, j] == T) {
              i <- i + 1
              next()
            }
            temp <- table_heading[i, j]
            merge_row_start <- i
            merge_row_end <- i
            merge_column_end <- j
            
            for (k in ((i + 1):(nrow(table_heading)))) {
              if (table_heading[k, j] == temp) {
                merge_row_end <- k
                next()
              } else{
                break()
              }
            }
            while ((all(unique(table_heading[merge_row_start:merge_row_end, merge_column_end + 1]) ==
                        rep(temp, length(
                          unique(table_heading[merge_row_start:merge_row_end, merge_column_end + 1])
                        )))) &
                   merge_column_end < ncol(table_heading)) {
              merge_column_end <- merge_column_end + 1
            }
            
            openxlsx::mergeCells(
              outwb,
              sheet_name,
              cols = 1 + c(merge_column_start:merge_column_end),
              rows = 7 + c(merge_row_start:merge_row_end)
            )
            
            
            tfdf[merge_row_start:merge_row_end, merge_column_start:merge_column_end] <-
              T
            i <- i + 1
          }
          j <- j + 1
        }
        
        
        #logic to merge the rows
        final <- c()
        for (i in 2:nrow(table)) {
          after <- table[i, 1]
          if (!is.na(after)) {
            final <- c(final, (i - 1))
          }
        }
        final <- c(final, nrow(table))
        final <- final + 8 + nrow(table_heading)
        for (i in 1:(length(final) - 1)) {
          openxlsx::mergeCells(outwb,
                               sheet_name,
                               rows = c(final[i]:(final[i + 1] - 1)),
                               cols = 2)
        }
        
        
        
        for (i in (9 + nrow(table_heading)):(8 + nrow(table_heading) + nrow(table))) {
          
        }
        #Adding format style to the table header
        openxlsx::addStyle(
          outwb,
          sheet = sheet_name,
          hs2,
          rows = 8:(nrow(table_heading) + 7),
          cols = 2:(ncol(table_heading) + 1),
          gridExpand = TRUE
        )
        deleteData(
          outwb,
          sheet = sheet_name,
          cols = 2:(2 + ncol(table_heading)),
          rows = 7,
          gridExpand = TRUE
        )
        
        
        
        #merge the All row
        openxlsx::mergeCells(outwb,
                             sheet_name,
                             rows = 8 + nrow(table_heading),
                             cols = 2:3)
        
        # Add N style to table
        for (i in (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) +
                                             7)) {
          openxlsx::addStyle(
            outwb,
            sheet_name,
            NDecColStyle,
            rows = i,
            cols = 4:(1 + ncol(table_heading)),
            gridExpand = TRUE,
            stack = FALSE
          )
        }
        #adding grey to the labels
        addStyle(
          outwb,
          sheet = sheet_name,
          grey_bg,
          rows = (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) +
                                              7),
          cols = 3,
          gridExpand = TRUE
        )
        #adding bold grey to the variables
        addStyle(
          outwb,
          sheet = sheet_name,
          bold_grey_center,
          rows = (8 + nrow(table_heading)):(nrow(table) + nrow(table_heading) + 7),
          cols = 2,
          gridExpand = TRUE
        )
        
        
        openxlsx::addStyle(
          outwb,
          sheet_name,
          NColStyle,
          rows = 7 + nrow(table_heading) + which(grepl("All", table[, 2])),
          cols = 4:(1 + ncol(table_heading)),
          gridExpand = TRUE,
          stack = FALSE
        )
        openxlsx::addStyle(
          outwb,
          sheet_name,
          NBoldColStyle,
          rows = 8 + nrow(table_heading),
          cols = 4:(1 + ncol(table_heading)),
          gridExpand = TRUE,
          stack = FALSE
        )
        
      }
      
      
      
      #setting the width of the column
      width_vec <- apply(table, 2,
                         function(x)
                           max(nchar(as.character(x)) + 2,
                               na.rm = TRUE))
      width_vec_header <- nchar(colnames(table_heading)) + 2
      max_vec_header <- pmax(width_vec, width_vec_header)
      openxlsx::setColWidths(outwb,
                             sheet_name,
                             cols = 2:(ncol(table) + 1),
                             widths = max_vec_header)
      
    }
    
    
    
    #saving the workbook
    openxlsx::saveWorkbook(outwb, file = file_name, overwrite = TRUE)
    
  }
