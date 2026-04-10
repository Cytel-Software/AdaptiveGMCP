# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#' Wrapper function that takes all inputs through a single R dataframe
#' Ani: Added sOutPath so that output can be written to the CSV file after every scenario
#' is simulated rather than at the end of all scenarios.
#' @param InputDF R Dataframe: This is the csv/excel input data in the R dataframe format
#' @param sOutPath String: File path to save the output csv file
#' @example ./internalData/RunBatches12-04-24.R
#' @export
simMAMSMEP_Wrapper <- function(InputDF, sOutPath) {
  # Update the dataframe column names in the following mapping in case
  # the names in the input csv/excel changes
  library(dplyr)
  library(tidyr)

  # ### Ani:
  # browser()
  # ###
  #
  lOut <- list()
  allRawPValues <- data.frame() # Initialize dataframe to collect raw p-values

  # Open file connection for the entire batch
  fileConn <- file(sOutPath, "w")  # Open in write mode
  on.exit(close(fileConn))  # Ensure connection is closed when function exits

  for (nModelNum in 1:nrow(InputDF)) {
    # Start timer for this iteration
    start_time <- Sys.time()

    print(paste0("Running Model ", nModelNum, " out of ", nrow(InputDF),
                 ": ModelID = ", InputDF[nModelNum, "ModelID"]))

    out <- tryCatch(
      {
        run1TestCase(InputDF = InputDF[nModelNum, ])
      },
      error = function(err) {
        paste0("Model ", nModelNum, " execution failed.")
      }
    )

    # End timer and calculate time taken in seconds
    end_time <- Sys.time()
    time_taken <- as.numeric(difftime(end_time, start_time, units = "hours"))

    errorLog <- c(!grepl("Invalid", out[[1]]), !is.character(out), !is.null(out))

    if (!any(errorLog == F)) {
      # extract the power table from the output
      dfOverall_Powers_long <- out$Overall_Powers_df

      #extracting confidence intervals
      conf_intervals <- dfOverall_Powers_long$ConfIntv_95perc
      dfOverall_Powers_long <- dfOverall_Powers_long %>% select(-ConfIntv_95perc)
      # Add new columns for each confidence interval
      for (i in 1:nrow(dfOverall_Powers_long)) {
        col_name <- paste0("CI_95perc_", dfOverall_Powers_long$Overall_Powers[i])
        dfOverall_Powers_long[ nrow(dfOverall_Powers_long) + 1,] <- c(col_name, conf_intervals[i])
      }

      # convert the power table to wide format and add serial number column
      dfOverall_Powers_wide <- dfOverall_Powers_long %>%
        pivot_wider(names_from = Overall_Powers, values_from = Values) %>%
        mutate(Sno = nModelNum) %>%
        relocate(Sno, .before = everything())
      # Model ID and Seed Number to reproduce
      mInfo <- data.frame("ModelID" = InputDF[nModelNum, "ModelID"], "seed" = out$Seed)

      # Output Table
      OutTab <- cbind(mInfo, data.frame(dfOverall_Powers_wide[, -1]))

      # Add stage wise rejection columns
      OutTab$StagewiseRejection_Count <- paste(out$stagewiseRejections$Count, collapse = ",")
      OutTab$StagewiseRejection_Percentage <- paste(out$stagewiseRejections$Percentage, collapse = ",")
      # Add TimeTaken column
      OutTab$HoursTaken <- time_taken


      # add each iterations power table to a list
      lOut[[nModelNum]] <- OutTab

      # Combining input and output for this model
      dfInpSep <- data.frame(Input = ">>>", stringsAsFactors = FALSE)
      dfCombined <- cbind(OutTab, dfInpSep, InputDF[nModelNum,
                                          !(names(InputDF) %in% "ModelID")])

      # Reorder columns to put Scenario right after ModelID
      col_order <- c("ModelID", "Scenario",
                     setdiff(names(dfCombined), c("ModelID", "Scenario")))
      dfCombined <- dfCombined[, col_order]

      # Write to output file using the open connection
      write.table(dfCombined, file = fileConn, sep = ",",
                 row.names = FALSE, col.names = (nModelNum == 1))
      flush(fileConn)  # Ensure data is written to disk after each model

      # Collect raw p-values if they exist
      if (!is.null(out$rawPValues) && is.data.frame(out$rawPValues) && nrow(out$rawPValues) > 0) {
        # Add model number column
        modelRawPValues <- out$rawPValues
        modelRawPValues$ModelNum <- nModelNum
        modelRawPValues$ModelID <- InputDF[nModelNum, "ModelID"]

        # InputDF[nModelNum, "info_frac"]
        modelRawPValues$Look <- rep(1:length(eval(
          parse(text = InputDF[nModelNum, ]$info_frac)
          )), length.out = nrow(out$rawPValues))

        dfTemp <- modelRawPValues %>% select(-c(ModelNum, ModelID, Look))
        dfTemp <- cbind(modelRawPValues$ModelNum, modelRawPValues$ModelID,
                        modelRawPValues$Look, dfTemp)
        modelRawPValues <- dfTemp %>% rename(ModelNum = V1, ModelID = V2,
                                             Look = V3)

        # Append to the combined dataframe
        # allRawPValues <- rbind(allRawPValues, modelRawPValues)
        allRawPValues <- bind_rows(allRawPValues, modelRawPValues)
      }

      passedTxt <- paste0("Model ", nModelNum, " execution completed successfully.")
      cat(passedTxt, "\n")
      print(paste0("Power table for model ", nModelNum, ":"))
      print(OutTab)
      cat("\n")
    } else if (grepl("Invalid", out[[1]])) {
      failTxt <- paste0("Model ", nModelNum, " execution failed.")
      cat(failTxt, "\n")
      cat("Details \n")
      print(unlist(out))
      cat("\n")
    } else {
      print(out)
      cat("\n")
    }
  }

  # rbind power tables for each iteration to produce a single table
  dfOut <- do.call(rbind, lOut)
  dfOut <- dplyr::left_join(dfOut, InputDF, by = "ModelID")

  ## Uncomment this code block to save raw p-values to a CSV file
  # Save combined raw p-values to CSV if we have data
  # if (nrow(allRawPValues) > 0) {
  #   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  #   csvFilePath <- paste0("internalData/RawPValues_", timestamp, ".csv")
  #   write.csv(allRawPValues, file = csvFilePath, row.names = FALSE)
  #   cat("\nRaw p-values saved to:", csvFilePath, "\n")
  # }

  return(dfOut)
}


run1TestCase <- function(InputDF) {
  # mapping to link simMAMSMEP function arguments with csv columns
  Method <- InputDF$Method
  SampleSize <- InputDF$SampleSize
  alpha <- InputDF$alpha
  TestStatCont <- InputDF$TestStatCont
  TestStatBin <- InputDF$TestStatBin
  UseCC <- InputDF$UseCC
  FWERControl <- InputDF$FWERControl
  nArms <- InputDF$nArms
  nEps <- InputDF$nEps
  lEpType <- eval(parse(text =InputDF$lEpType))
  Arms.Mean <- eval(parse(text = InputDF$Arms.Mean))
  Arms.Prop <- eval(parse(text = InputDF$Arms.Prop))
  Arms.std.dev <- eval(parse(text = InputDF$Arms.std.dev))
  Arms.alloc.ratio <- eval(parse(text = InputDF$Arms.alloc.ratio))
  EP.Corr <- eval(parse(text = InputDF$EP.Corr))
  WI <- eval(parse(text = InputDF$WI))
  G <- eval(parse(text = InputDF$G))
  test.type <- InputDF$test.type
  info_frac <- eval(parse(text = InputDF$info_frac))
  typeOfDesign <- ifelse(is.na(InputDF$typeOfDesign), "asOF", InputDF$typeOfDesign)
  MultipleWinners <- InputDF$MultipleWinners
  MultipleWinners <- ifelse(is.na(MultipleWinners),FALSE,MultipleWinners)
  Selection <- InputDF$Selection
  Selection <- ifelse(is.na(Selection),FALSE, Selection)
  CommonStdDev <- ifelse(is.na(InputDF$CommonStdDev), F, InputDF$CommonStdDev)
  SelectionLook <- InputDF$SelectionLook
  SelectEndPoint <- InputDF$SelectEndPoint
  SelectionScale <- InputDF$SelectionScale
  SelectionCriterion <- InputDF$SelectionCriterion
  SelectionParameter <- InputDF$SelectionParameter
  KeepAssociatedHypo <- InputDF$KeepAssociatedHypo
  ImplicitSSR <- InputDF$ImplicitSSR
  ImplicitSSR <- ifelse(is.na(ImplicitSSR),FALSE,ImplicitSSR)
  nSimulation <- InputDF$nSimulation
  Seed <- InputDF$Seed
  SummaryStat <- InputDF$SummaryStat
  plotGraphs <- InputDF$plotGraphs
  Parallel <- InputDF$Parallel
  nSimulation_Stage2 <- InputDF$nSimulation_Stage2
  # put the following code in try catch so the loop continues even if one iteration fails
  Seed <- if (!is.na(suppressWarnings(as.numeric(Seed)))) as.numeric(Seed) else Seed

  # #############################
  # browser()
  # #############################

  out <- simMAMSMEP(
    alpha = alpha, SampleSize = SampleSize, nArms = nArms, nEps = nEps,lEpType=lEpType,
    TestStatCont = TestStatCont, TestStatBin = TestStatBin, UseCC = UseCC, FWERControl = FWERControl,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev,Arms.Prop = Arms.Prop, Arms.alloc.ratio = Arms.alloc.ratio,
    EP.Corr = EP.Corr, WI = WI, G = G, test.type = test.type, info_frac = info_frac,
    typeOfDesign = typeOfDesign, MultipleWinners = MultipleWinners,
    Selection = Selection, SelectionLook = SelectionLook, SelectEndPoint = SelectEndPoint, SelectionScale = SelectionScale,
    SelectionCriterion = SelectionCriterion, SelectionParameter = SelectionParameter, KeepAssociatedHypo = KeepAssociatedHypo,
    ImplicitSSR = ImplicitSSR, nSimulation = nSimulation, Seed = Seed, SummaryStat = SummaryStat,
    Method = Method, plotGraphs = plotGraphs, Parallel = Parallel,CommonStdDev = CommonStdDev,
    nSimulation_Stage2 = nSimulation_Stage2, Verbose = TRUE
  )
  out
}


#' Create table and plots of given format
#' @param PowerType : Type of power to be extracted from the output dataframe. Must be one of "Global.Power", "Conjunctive.Power", "Disjunctive.Power", "FWER".
#' @param dfOut : output from simMAMSMEP_Wrapper
#' @param TableTemDF : Template table dataframe specifying the scenarios and treatment selection rules
#' @export
genPowerTablePlots <- function(PowerType, dfOut, TableTemDF) {
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  library(tidyr)

  p <- sapply(TableTemDF$ModelID, function(mID) {
    p <- dfOut[dfOut$ModelID == mID, PowerType]
    if (length(p) == 0) {
      p <- NA
    }
    p
  })
  tab1 <- TableTemDF[, -which(names(TableTemDF) == "ModelID")]
  tab1[PowerType] <- p
  scenarios <- unique(tab1$Level1)
  plots <- list()

  tab3 <- reshape(
    data = tab1, idvar = c("Level1", "Level2"),
    v.names = PowerType, timevar = "Method", direction = "wide"
  )
  tab3$Difference <- tab3[, 4] - tab3[, 3]

  colnames(tab3) <- c(
    "Scenario",
    "Treatment Selection Rule",
    gsub(paste0("^", PowerType, "\\."), "", names(tab3)[3:4]),
    "Difference"
  )
  SelectionRuleOrder <- c("Conservative", "Normal", "Aggressive", "Ultra")
  tab3$`Treatment Selection Rule` <- factor(tab3$`Treatment Selection Rule`, levels = SelectionRuleOrder)
  tabLong <- tab3 %>%
    select(!Difference) %>%
    pivot_longer(
      cols = !c("Scenario", "Treatment Selection Rule"),
      names_to = "MAMS",
      values_to = "value",
      values_drop_na = TRUE
    )

  list("TableWide" = tab3, "TableLong" = tabLong)
}
