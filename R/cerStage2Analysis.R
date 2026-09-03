# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

PerformStage2Test <- function(mcpObj, AdaptStage2) {
  Stage1Objs <- mcpObj$Stage1Obj
  cumulative_stage2_pvalues <- NA
  if (!AdaptStage2) {
    WH_modified_idx <- as.vector(apply(
      mcpObj$WH[, grep("H", names(mcpObj$WH))], 1,
      function(x) {
        paste(x, collapse = "")
      }
    ))

    WH_old_idx <- as.vector(apply(
      mcpObj$WH_Prev[, grep("H", names(mcpObj$WH_Prev))], 1,
      function(x) {
        paste(x, collapse = "")
      }
    ))

    oldIdx <- unlist(lapply(
      1:nrow(mcpObj$WH),
      function(i) {
        which(WH_old_idx == WH_modified_idx[i])
      }
    ))

    boundary <- Stage1Objs$plan_Bdry$Stage2Bdry[oldIdx, ]

    Stage2Analysis <- closedTest(
      WH = mcpObj$WH,
      boundary = boundary,
      pValues = mcpObj$p_raw,
      Stage1RejStatus = mcpObj$rej_flag_Prev
    )

    intHypTab <- Stage2Analysis$IntersectHypoTest
    HypoTab <- intHypTab[, grep("H", names(intHypTab))]
    InterHyp <- apply(HypoTab, 1, function(h) {
      paste(names(HypoTab)[which(h == 1)], collapse = ",")
    })

    intRejStat <- sapply(intHypTab$Rejected, function(x) {
      ifelse(x, "Rejected", "Not_Rejected")
    })
    IntersectHypoTest <- knitr::kable(data.frame(
      "Hypotheses" = InterHyp,
      "Status" = intRejStat,
      row.names = NULL
    ), align = "c")

    rejTab <- Stage2Analysis$PrimaryHypoTest
    FinRejStat <- sapply(rejTab, function(x) {
      ifelse(x, "Rejected", "Not_Rejected")
    })
    FinalRejTab <- knitr::kable(data.frame(
      "Hypotheses" = names(rejTab),
      "Status" = FinRejStat, row.names = NULL
    ), align = "c")

    Stage2Tables <- list(
      "Test_Intersection_Hypothesis" = IntersectHypoTest,
      "Final_Rejection_Status" = FinalRejTab
    )
  } else {
    ss_stage2_incr <- mcpObj$Stage2AllocSampleSize
    ss_stage2_incr[2,] <- ss_stage2_incr[2,] - ss_stage2_incr[1,]
    # calculate adapted information fraction for each hypothesis at stage 2
    v_adapted_info_fraction <- numeric(0)
    ss_control <- as.vector(unlist(ss_stage2_incr['Control']))
    calculate_hm <- function(vControl, vTreatment, stage) (1/vControl[stage] + 1/vTreatment[stage])^-1
    for (hypothesis in names(mcpObj$p_raw)) {
      if (is.na(mcpObj$p_raw[hypothesis])) {
        adapted_info_fraction <- NA
      } else
      {
        treatment_id <- mcpObj$HypoMap[mcpObj$HypoMap$Hypothesis == hypothesis, "Treatment"] - 1
        ss_treatment <- as.vector(unlist(ss_stage2_incr[paste0("Treatment",treatment_id)]))
        adapted_info_fraction <- calculate_hm(ss_control, ss_treatment, 1)/(calculate_hm(ss_control, ss_treatment, 1) + calculate_hm(ss_control, ss_treatment, 2))
      }
      v_adapted_info_fraction <- c(v_adapted_info_fraction, adapted_info_fraction)
    }

    adapted_p_value_stage2 <- 1 - pnorm(sqrt(v_adapted_info_fraction)*qnorm(1 - mcpObj$p_raw_stage1) +
                                          sqrt(1 - v_adapted_info_fraction)*qnorm(1 - mcpObj$p_raw))
    cumulative_stage2_pvalues <- adapted_p_value_stage2
    cat("Cumulative Stage2 p-values:\n")
    print(adapted_p_value_stage2)
    cat("\n")
    Stage2Analysis <- closedTest(
      WH = mcpObj$WH,
      boundary = mcpObj$AdaptObj$Stage2AdjBdry,
      pValues = adapted_p_value_stage2,
      Stage1RejStatus = mcpObj$rej_flag_Prev
    )
    intHypTab <- Stage2Analysis$IntersectHypoTest
    HypoTab <- intHypTab[, grep("H", names(intHypTab))]
    InterHyp <- apply(HypoTab, 1, function(h) {
      paste(names(HypoTab)[which(h == 1)], collapse = ",")
    })

    intRejStat <- sapply(intHypTab$Rejected, function(x) {
      ifelse(x, "Rejected", "Not_Rejected")
    })
    IntersectHypoTest <- knitr::kable(data.frame(
      "Hypotheses" = InterHyp,
      "Status" = intRejStat,
      row.names = NULL
    ), align = "c")

    rejTab <- Stage2Analysis$PrimaryHypoTest
    FinRejStat <- sapply(rejTab, function(x) {
      ifelse(x, "Rejected", "Not_Rejected")
    })
    FinalRejTab <- knitr::kable(data.frame(
      "Hypotheses" = names(rejTab),
      "Status" = FinRejStat, row.names = NULL
    ), align = "c")

    Stage2Tables <- list(
      "Adapt_Test_Tables" = mcpObj$AdaptObj$Stage2Tables,
      "Test_Intersection_Hypothesis" = IntersectHypoTest,
      "Final_Rejection_Status" = FinalRejTab
    )
  }

  list(
    "Stage2Tables" = Stage2Tables,
    "RejStat" = Stage2Analysis$PrimaryHypoTest,
    "CumulativeStage2PValues" = cumulative_stage2_pvalues
  )
}

# To modify the stage-2 sample size(SSR)
do_ModifyStage2Sample <- function(allocRatio, ArmsPresent, AllocSampleSize) {
  ArmsPresent <- sort(ArmsPresent, decreasing = F)
  newAllocSampleSize <- AllocSampleSize
  newallocRatio <- allocRatio

  newAllocSampleSize[2, which(!(1:ncol(newAllocSampleSize) %in% ArmsPresent))] <-
    newallocRatio[which(!(1:ncol(newAllocSampleSize) %in% ArmsPresent))] <- NA

  SSRFlag <- readline(prompt = paste("Modify Stage-2 cumulative Sample Size (y/n) :"))
  if (SSRFlag == "y") {
    availArmsName <- names(AllocSampleSize)[ArmsPresent]
    cat("Planned Sample Size for reference: \n")
    print(AllocSampleSize)
    eg_text <- paste("(e.g.",
                     paste(100+1:length(availArmsName),collapse = ","),")")
    cat(
      "Enter Stage-2 Cumulative sample size for ", paste(availArmsName, collapse = ", "),
      eg_text, "\n"
    )
    newSS <- readline()

    newAllocSampleSize[2, ArmsPresent] <- as.numeric(stringr::str_trim(
      unlist(strsplit(newSS, split = ",")),
      "both"
    ))
    newallocRatio <- as.numeric(newAllocSampleSize[2, ]) / as.numeric(newAllocSampleSize[2, 1])

    list(
      "newAllocSampleSize" = newAllocSampleSize,
      "newallocRatio" = newallocRatio
    )
  } else {
    list(
      "newAllocSampleSize" = AllocSampleSize,
      "newallocRatio" = allocRatio
    )
  }
}
