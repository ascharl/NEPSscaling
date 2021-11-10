#' Collapse categories with too low cell frequencies (< 200) for stable PC
#'   estimation (guidelines laid out in Pohl & Carstensen (2012))
#'
#' @param resp Data.frame containing the item responses of the test takers
#' @param SC String representation of starting cohort
#' @param wave String representation of current wave of test assessment
#' @param domain String representation of current competency domain
#'
#' @return updated resp data.frame with polytomous items collapsed according to
#' main scaling
#' @noRd
collapse_categories_pcm <- function(resp, SC, wave, domain) {
  if (SC == "SC6") {
    if (domain == "RE") {
      # if (wave == "w3") { # SC6 RE w3 and w5 already collapsed in SUF
      #   resp[["rea3012s_c"]][resp[["rea3012s_c"]] %in% c(0, 1)] <- 0
      #   resp[["rea3012s_c"]][resp[["rea3012s_c"]] == 2] <- 1
      #   resp[["rea3012s_c"]][resp[["rea3012s_c"]] == 3] <- 2
      #   resp[["rea3015s_c"]][resp[["rea3015s_c"]] == 3] <- 2

      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] %in% c(0, 1)] <- 0
      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] == 2] <- 1
      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] == 3] <- 2
      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] == 4] <- 3
      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] == 5] <- 4
      #   resp[["rea3028s_c"]][resp[["rea3028s_c"]] == 6] <- 5

      #   resp[["rea3038s_c"]][resp[["rea3038s_c"]] %in% c(0, 1)] <- 0
      #   resp[["rea3038s_c"]][resp[["rea3038s_c"]] == 2] <- 1
      #   resp[["rea3038s_c"]][resp[["rea3038s_c"]] == 3] <- 2

      #   resp[["rea3052s_c"]][resp[["rea3052s_c"]] %in% c(0, 1, 2)] <- 0
      #   resp[["rea3052s_c"]][resp[["rea3052s_c"]] == 3] <- 1

      #   resp[["rea3054s_c"]][resp[["rea3054s_c"]] %in% c(0, 1)] <-  0
      #   resp[["rea3054s_c"]][resp[["rea3054s_c"]] %in% c(2, 3)] <-  1
      #   resp[["rea3054s_c"]][resp[["rea3054s_c"]] %in% c(4, 5)] <-  2
      # }
      if (wave == "w9") {
        # collapse categories
        resp[["rea90101s_c"]][resp[["rea90101s_c"]] == 1] <- 0
        resp[["rea90101s_c"]][resp[["rea90101s_c"]] == 2] <- 1
        resp[["rea90101s_c"]][resp[["rea90101s_c"]] == 3] <- 2
        resp[["rea90101s_c"]][resp[["rea90101s_c"]] == 4] <- 3

        resp[["rea90206s_c"]][resp[["rea90206s_c"]] == 4] <- 3

        resp[["rea90403s_c"]][resp[["rea90403s_c"]] == 1] <- 0
        resp[["rea90403s_c"]][resp[["rea90403s_c"]] == 2] <- 1
        resp[["rea90403s_c"]][resp[["rea90403s_c"]] == 3] <- 2
        resp[["rea90403s_c"]][resp[["rea90403s_c"]] == 4] <- 3
        resp[["rea90403s_c"]][resp[["rea90403s_c"]] == 5] <- 4
      }
    }
    if (domain == "MA") {
      if (wave == "w9") {
        # collapse categories
        resp[["maa9v28s_c"]][resp[["maa9v28s_c"]] %in% c(0, 1, 2, 3)] <- 0
        resp[["maa9v28s_c"]][resp[["maa9v28s_c"]] %in% c(4, 5, 6)] <- 1

        resp[["maa9v27s_c"]][resp[["maa9v27s_c"]] == 1] <- 0
        resp[["maa9v27s_c"]][resp[["maa9v27s_c"]] == 2] <- 1
        resp[["maa9v27s_c"]][resp[["maa9v27s_c"]] == 3] <- 2

        resp[["maa9r26s_c"]][resp[["maa9r26s_c"]] == 1] <- 0
        resp[["maa9r26s_c"]][resp[["maa9r26s_c"]] == 2] <- 1
        resp[["maa9r26s_c"]][resp[["maa9r26s_c"]] == 3] <- 2
        resp[["maa9r26s_c"]][resp[["maa9r26s_c"]] == 4] <- 3

        # left out because of extreme amount of missing values (almost 100%!)
        # resp[["maa9d23s_c"]][resp[["maa9d23s_c"]] == 1] <- 0
        # resp[["maa9d23s_c"]][resp[["maa9d23s_c"]] == 2] <- 1
        # resp[["maa9r18s_c"]][resp[["maa9d18s_c"]] == 2] <- 1

        resp[["maa9d13s_c"]][resp[["maa9d13s_c"]] %in% c(1, 2, 3)] <- 0
        resp[["maa9d13s_c"]][resp[["maa9d13s_c"]] == 4] <- 1
        resp[["maa9d13s_c"]][resp[["maa9d13s_c"]] %in% c(5, 6)] <- 2

        resp[["maa9d09s_c"]][resp[["maa9d09s_c"]] == 1] <- 0
        resp[["maa9d09s_c"]][resp[["maa9d09s_c"]] == 2] <- 1
        resp[["maa9d09s_c"]][resp[["maa9d09s_c"]] == 3] <- 2
        resp[["maa9d09s_c"]][resp[["maa9d09s_c"]] == 4] <- 3
        resp[["maa9d09s_c"]][resp[["maa9d09s_c"]] == 5] <- 4

        resp[["maa9v07s_c"]][resp[["maa9v07s_c"]] == 2] <- 1

        resp[["maa9r03s_c"]][resp[["maa9r03s_c"]] == 1] <- 0
        resp[["maa9r03s_c"]][resp[["maa9r03s_c"]] == 2] <- 1
        resp[["maa9r03s_c"]][resp[["maa9r03s_c"]] == 3] <- 2
      }
    }
    if (domain == "IC") {
      resp[["ica5018s_c"]][resp[["ica5018s_c"]] %in% c(2, 3)] <- 1
      resp[["ica5018s_c"]][resp[["ica5018s_c"]] %in% c(4, 5, 6, 7)] <- 2
      resp[["ica5018s_c"]][resp[["ica5018s_c"]] == 8] <- 3

      resp[["ica5015s_c"]][resp[["ica5015s_c"]] %in% c(0, 1)] <- 0
      resp[["ica5015s_c"]][resp[["ica5015s_c"]] == 2] <- 1
      resp[["ica5015s_c"]][resp[["ica5015s_c"]] == 3] <- 2

      resp[["ica5020s_c"]][resp[["ica5020s_c"]] %in% c(0, 1)] <- 0
      resp[["ica5020s_c"]][resp[["ica5020s_c"]] == 2] <- 1
      resp[["ica5020s_c"]][resp[["ica5020s_c"]] == 3] <- 2
      resp[["ica5020s_c"]][resp[["ica5020s_c"]] == 4] <- 3

      resp[["ica5050s_c"]][resp[["ica5050s_c"]] %in% c(0, 1)] <- 0
      resp[["ica5050s_c"]][resp[["ica5050s_c"]] == 2] <- 1
      resp[["ica5050s_c"]][resp[["ica5050s_c"]] == 3] <- 2
      resp[["ica5050s_c"]][resp[["ica5050s_c"]] == 4] <- 3

      resp[["ica5004s_c"]][resp[["ica5004s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5004s_c"]][resp[["ica5004s_c"]] == 3] <- 1
      resp[["ica5004s_c"]][resp[["ica5004s_c"]] == 4] <- 2
      resp[["ica5004s_c"]][resp[["ica5004s_c"]] == 5] <- 3

      resp[["ica5017s_c"]][resp[["ica5017s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5017s_c"]][resp[["ica5017s_c"]] == 3] <- 1
      resp[["ica5017s_c"]][resp[["ica5017s_c"]] == 4] <- 2
      resp[["ica5017s_c"]][resp[["ica5017s_c"]] == 5] <- 3

      resp[["ica5016s_c"]][resp[["ica5016s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5016s_c"]][resp[["ica5016s_c"]] == 3] <- 1
      resp[["ica5016s_c"]][resp[["ica5016s_c"]] == 4] <- 2
      resp[["ica5016s_c"]][resp[["ica5016s_c"]] == 5] <- 3

      resp[["ica5047s_c"]][resp[["ica5047s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5047s_c"]][resp[["ica5047s_c"]] == 3] <- 1
      resp[["ica5047s_c"]][resp[["ica5047s_c"]] == 4] <- 2
      resp[["ica5047s_c"]][resp[["ica5047s_c"]] == 5] <- 3

      resp[["ica5021s_c"]][resp[["ica5021s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5021s_c"]][resp[["ica5021s_c"]] == 3] <- 1
      resp[["ica5021s_c"]][resp[["ica5021s_c"]] == 4] <- 2
      resp[["ica5021s_c"]][resp[["ica5021s_c"]] == 5] <- 3

      resp[["ica5052s_c"]][resp[["ica5052s_c"]] %in% c(0, 1, 2)] <- 0
      resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 3] <- 1
      resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 4] <- 2
      resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 5] <- 3
    } else if (domain == "SC") {
      resp[["sca5652s_c"]][resp[["sca5652s_c"]] %in% c(0, 1)] <- 0
      resp[["sca5652s_c"]][resp[["sca5652s_c"]] == 2] <- 1
      resp[["sca5652s_c"]][resp[["sca5652s_c"]] == 3] <- 2
      resp[["sca5652s_c"]][resp[["sca5652s_c"]] == 4] <- 3

      resp[["sca5091s_c"]][resp[["sca5091s_c"]] %in% c(0, 1)] <- 0
      resp[["sca5091s_c"]][resp[["sca5091s_c"]] == 2] <- 1
      resp[["sca5091s_c"]][resp[["sca5091s_c"]] == 3] <- 2
      resp[["sca5091s_c"]][resp[["sca5091s_c"]] == 4] <- 3
    }
  } else if (SC == "SC5") {
    # EF (w12) already collapsed in SUF
    if (domain == "RE") {
      # reading w1 already collapsed in SUF
      if (wave == "w12") {
        # collapse categories
        resp[["rea90201s_sc5s12_c"]][resp[["rea90201s_sc5s12_c"]] == 1] <- 0
        resp[["rea90201s_sc5s12_c"]][resp[["rea90201s_sc5s12_c"]] == 2] <- 1
        resp[["rea90201s_sc5s12_c"]][resp[["rea90201s_sc5s12_c"]] == 3] <- 2
        resp[["rea90201s_sc5s12_c"]][resp[["rea90201s_sc5s12_c"]] == 4] <- 3

        resp[["rea90206s_sc5s12_c"]][resp[["rea90206s_sc5s12_c"]] == 4] <- 3

        resp[["rea90402s_sc5s12_c"]][resp[["rea90402s_sc5s12_c"]] == 1] <- 0
        resp[["rea90402s_sc5s12_c"]][resp[["rea90402s_sc5s12_c"]] == 2] <- 1
        resp[["rea90402s_sc5s12_c"]][resp[["rea90402s_sc5s12_c"]] == 3] <- 2
        resp[["rea90402s_sc5s12_c"]][resp[["rea90402s_sc5s12_c"]] == 4] <- 3
        resp[["rea90402s_sc5s12_c"]][resp[["rea90402s_sc5s12_c"]] == 5] <- 4
      }
    }
    if (domain == "MA") {
      if (wave == "w1") {
        resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] %in% c(1, 2)] <- 0
        resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] == 3] <- 1
        resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] == 4] <- 2
      }
      if (wave == "w12") {
        # collapse categories
        resp[["maa9v28s_sc5s12_c"]][resp[["maa9v28s_sc5s12_c"]] %in% c(0, 1, 2, 3)] <- 0
        resp[["maa9v28s_sc5s12_c"]][resp[["maa9v28s_sc5s12_c"]] %in% c(4, 5, 6)] <- 1

        resp[["maa9v27s_sc5s12_c"]][resp[["maa9v27s_sc5s12_c"]] == 1] <- 0
        resp[["maa9v27s_sc5s12_c"]][resp[["maa9v27s_sc5s12_c"]] == 2] <- 1
        resp[["maa9v27s_sc5s12_c"]][resp[["maa9v27s_sc5s12_c"]] == 3] <- 2

        resp[["maa9r26s_sc5s12_c"]][resp[["maa9r26s_sc5s12_c"]] == 1] <- 0
        resp[["maa9r26s_sc5s12_c"]][resp[["maa9r26s_sc5s12_c"]] == 2] <- 1
        resp[["maa9r26s_sc5s12_c"]][resp[["maa9r26s_sc5s12_c"]] == 3] <- 2
        resp[["maa9r26s_sc5s12_c"]][resp[["maa9r26s_sc5s12_c"]] == 4] <- 3

        # left out because of extreme amount of missing values (almost 100%!)
        # resp[["maa9d23s_sc5s12_c"]][resp[["maa9d23s_sc5s12_c"]] == 1] <- 0
        # resp[["maa9d23s_sc5s12_c"]][resp[["maa9d23s_sc5s12_c"]] == 2] <- 1

        # resp[["maa9r18s_sc5s12_c"]][resp[["maa9r18s_sc5s12_c"]] == 2] <- 1

        resp[["maa9d13s_sc5s12_c"]][resp[["maa9d13s_sc5s12_c"]] %in% c(1, 2, 3)] <- 0
        resp[["maa9d13s_sc5s12_c"]][resp[["maa9d13s_sc5s12_c"]] == 4] <- 1
        resp[["maa9d13s_sc5s12_c"]][resp[["maa9d13s_sc5s12_c"]] %in% c(5, 6)] <- 2

        resp[["maa9d09s_sc5s12_c"]][resp[["maa9d09s_sc5s12_c"]] == 1] <- 0
        resp[["maa9d09s_sc5s12_c"]][resp[["maa9d09s_sc5s12_c"]] == 2] <- 1
        resp[["maa9d09s_sc5s12_c"]][resp[["maa9d09s_sc5s12_c"]] == 3] <- 2
        resp[["maa9d09s_sc5s12_c"]][resp[["maa9d09s_sc5s12_c"]] == 4] <- 3
        resp[["maa9d09s_sc5s12_c"]][resp[["maa9d09s_sc5s12_c"]] == 5] <- 4

        resp[["maa9v07s_sc5s12_c"]][resp[["maa9v07s_sc5s12_c"]] == 2] <- 1

        resp[["maa9r03s_sc5s12_c"]][resp[["maa9r03s_sc5s12_c"]] == 1] <- 0
        resp[["maa9r03s_sc5s12_c"]][resp[["maa9r03s_sc5s12_c"]] == 2] <- 1
        resp[["maa9r03s_sc5s12_c"]][resp[["maa9r03s_sc5s12_c"]] == 3] <- 2
      }
    } else if (domain == "IC") {
      resp[["ics3002s_c"]][resp[["ics3002s_c"]] == 1] <- 0
      resp[["ics3002s_c"]][resp[["ics3002s_c"]] == 2] <- 1

      resp[["ics3029s_c"]][resp[["ics3029s_c"]] == 1] <- 0
      resp[["ics3029s_c"]][resp[["ics3029s_c"]] == 2] <- 1
      resp[["ics3029s_c"]][resp[["ics3029s_c"]] == 3] <- 2

      resp[["ics3035s_c"]][resp[["ics3035s_c"]] == 1] <- 0
      resp[["ics3035s_c"]][resp[["ics3035s_c"]] == 2] <- 1
      resp[["ics3035s_c"]][resp[["ics3035s_c"]] == 3] <- 2
      resp[["ics3035s_c"]][resp[["ics3035s_c"]] == 4] <- 3

      resp[["ics3049s_c"]][resp[["ics3049s_c"]] == 1] <- 0
      resp[["ics3049s_c"]][resp[["ics3049s_c"]] == 2] <- 1
      resp[["ics3049s_c"]][resp[["ics3049s_c"]] == 3] <- 2
    } else if (domain == "SC") {
      resp[["scs3623s_c"]][resp[["scs3623s_c"]] == 1] <- 0
      resp[["scs3623s_c"]][resp[["scs3623s_c"]] == 2] <- 1
      resp[["scs3623s_c"]][resp[["scs3623s_c"]] == 3] <- 2

      resp[["scs3021s_c"]][resp[["scs3021s_c"]] == 1] <- 0
      resp[["scs3021s_c"]][resp[["scs3021s_c"]] == 2] <- 1
      resp[["scs3021s_c"]][resp[["scs3021s_c"]] == 3] <- 2

      resp[["scs3022s_c"]][resp[["scs3022s_c"]] == 1] <- 0
      resp[["scs3022s_c"]][resp[["scs3022s_c"]] == 2] <- 1
      resp[["scs3022s_c"]][resp[["scs3022s_c"]] == 3] <- 2
      resp[["scs3022s_c"]][resp[["scs3022s_c"]] == 4] <- 3

      resp[["scs3643s_c"]][resp[["scs3643s_c"]] == 1] <- 0
      resp[["scs3643s_c"]][resp[["scs3643s_c"]] == 2] <- 1
      resp[["scs3643s_c"]][resp[["scs3643s_c"]] == 3] <- 2
      resp[["scs3643s_c"]][resp[["scs3643s_c"]] == 4] <- 3

      resp[["scs3642s_c"]][resp[["scs3642s_c"]] == 1] <- 0
      resp[["scs3642s_c"]][resp[["scs3642s_c"]] == 2] <- 1
      resp[["scs3642s_c"]][resp[["scs3642s_c"]] == 3] <- 2

      resp[["scs3031s_c"]][resp[["scs3031s_c"]] == 1] <- 0
      resp[["scs3031s_c"]][resp[["scs3031s_c"]] == 2] <- 1
      resp[["scs3031s_c"]][resp[["scs3031s_c"]] == 3] <- 2

      resp[["scs3112s_c"]][resp[["scs3112s_c"]] == 1] <- 0
      resp[["scs3112s_c"]][resp[["scs3112s_c"]] == 2] <- 1
      resp[["scs3112s_c"]][resp[["scs3112s_c"]] == 3] <- 2

      resp[["scs3132s_c"]][resp[["scs3132s_c"]] == 1] <- 0
      resp[["scs3132s_c"]][resp[["scs3132s_c"]] == 2] <- 1
      resp[["scs3132s_c"]][resp[["scs3132s_c"]] == 3] <- 2
      resp[["scs3132s_c"]][resp[["scs3132s_c"]] == 4] <- 3

      resp[["scs3133s_c"]][resp[["scs3133s_c"]] %in% c(1, 2)] <- 0
      resp[["scs3133s_c"]][resp[["scs3133s_c"]] == 3] <- 1
      resp[["scs3133s_c"]][resp[["scs3133s_c"]] == 4] <- 2

      resp[["scs3012s_c"]][resp[["scs3012s_c"]] %in% c(1, 2)] <- 0
      resp[["scs3012s_c"]][resp[["scs3012s_c"]] == 3] <- 1
      resp[["scs3012s_c"]][resp[["scs3012s_c"]] == 4] <- 2

      resp[["scs3061s_c"]][resp[["scs3061s_c"]] %in% c(1, 2, 3)] <- 0
      resp[["scs3061s_c"]][resp[["scs3061s_c"]] == 4] <- 1
    }
  } else if (SC == "SC4") {
    if (domain == "RE") {
      # reading w1 already collapsed in SUF
      if (wave == "w7") {
        resp[["reg12014s_c"]][resp[["reg12014s_c"]] == 2] <- 1

        resp[["reg12021s_c"]][resp[["reg12021s_c"]] == 1] <- 0
        resp[["reg12021s_c"]][resp[["reg12021s_c"]] == 2] <- 1

        resp[["reg12024s_c"]][resp[["reg12024s_c"]] == 1] <- 0
        resp[["reg12024s_c"]][resp[["reg12024s_c"]] == 2] <- 1
        resp[["reg12024s_c"]][resp[["reg12024s_c"]] == 3] <- 2
        resp[["reg12024s_c"]][resp[["reg12024s_c"]] == 4] <- 3

        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 1] <- 0
        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 2] <- 1
        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 3] <- 2
        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 4] <- 3
        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 5] <- 4
        resp[["reg12026s_c"]][resp[["reg12026s_c"]] == 6] <- 5

        resp[["reg12042s_c"]][resp[["reg12042s_c"]] == 1] <- 0
        resp[["reg12042s_c"]][resp[["reg12042s_c"]] == 2] <- 1

        resp[["reg12055s_c"]][resp[["reg12055s_c"]] == 1] <- 0
        resp[["reg12055s_c"]][resp[["reg12055s_c"]] == 2] <- 1
        resp[["reg12055s_c"]][resp[["reg12055s_c"]] == 3] <- 2

        resp[["reg12071s_c"]][resp[["reg12071s_c"]] == 1] <- 0
        resp[["reg12071s_c"]][resp[["reg12071s_c"]] == 2] <- 0
        resp[["reg12071s_c"]][resp[["reg12071s_c"]] == 3] <- 1

        resp[["reg12075s_c"]][resp[["reg12075s_c"]] == 1] <- 0
        resp[["reg12075s_c"]][resp[["reg12075s_c"]] == 2] <- 1
        resp[["reg12075s_c"]][resp[["reg12075s_c"]] == 3] <- 2
      }
      if (wave == "w10") {
        resp[["rea90101s_sc4a10_c"]][resp[["rea90101s_sc4a10_c"]] == 1] <- 0
        resp[["rea90101s_sc4a10_c"]][resp[["rea90101s_sc4a10_c"]] == 2] <- 1
        resp[["rea90101s_sc4a10_c"]][resp[["rea90101s_sc4a10_c"]] == 3] <- 2
        resp[["rea90101s_sc4a10_c"]][resp[["rea90101s_sc4a10_c"]] == 4] <- 3

        resp[["rea90206s_sc4a10_c"]][resp[["rea90206s_sc4a10_c"]] == 4] <- 3

        resp[["rea90403s_sc4a10_c"]][resp[["rea90403s_sc4a10_c"]] == 1] <- 0
        resp[["rea90403s_sc4a10_c"]][resp[["rea90403s_sc4a10_c"]] == 2] <- 1
        resp[["rea90403s_sc4a10_c"]][resp[["rea90403s_sc4a10_c"]] == 3] <- 2
        resp[["rea90403s_sc4a10_c"]][resp[["rea90403s_sc4a10_c"]] == 4] <- 3
        resp[["rea90403s_sc4a10_c"]][resp[["rea90403s_sc4a10_c"]] == 5] <- 4
      }
    } else if (domain == "MA") {
      # math w1 already collapsed in SUF
      if (wave == "w7") {
        resp[["mas1q02s_sc4g12_c"]][resp[["mas1q02s_sc4g12_c"]] == 1] <- 0
        resp[["mas1q02s_sc4g12_c"]][resp[["mas1q02s_sc4g12_c"]] == 2] <- 1
        resp[["mas1q02s_sc4g12_c"]][resp[["mas1q02s_sc4g12_c"]] == 3] <- 2
        resp[["mas1q02s_sc4g12_c"]][resp[["mas1q02s_sc4g12_c"]] == 4] <- 3
      }
      if (wave == "w10") {
        # collapse categories
        resp[["maa9v28s_sc4a10_c"]][resp[["maa9v28s_sc4a10_c"]] %in% c(0, 1, 2, 3)] <- 0
        resp[["maa9v28s_sc4a10_c"]][resp[["maa9v28s_sc4a10_c"]] %in% c(4, 5, 6)] <- 1

        resp[["maa9v27s_sc4a10_c"]][resp[["maa9v27s_sc4a10_c"]] == 1] <- 0
        resp[["maa9v27s_sc4a10_c"]][resp[["maa9v27s_sc4a10_c"]] == 2] <- 1
        resp[["maa9v27s_sc4a10_c"]][resp[["maa9v27s_sc4a10_c"]] == 3] <- 2

        resp[["maa9r26s_sc4a10_c"]][resp[["maa9r26s_sc4a10_c"]] == 1] <- 0
        resp[["maa9r26s_sc4a10_c"]][resp[["maa9r26s_sc4a10_c"]] == 2] <- 1
        resp[["maa9r26s_sc4a10_c"]][resp[["maa9r26s_sc4a10_c"]] == 3] <- 2
        resp[["maa9r26s_sc4a10_c"]][resp[["maa9r26s_sc4a10_c"]] == 4] <- 3

        # left out because of extreme amount of missing values (almost 100%!)
        # resp[["maa9d23s_sc4a10_c"]][resp[["maa9d23s_sc4a10_c"]] == 1] <- 0
        # resp[["maa9d23s_sc4a10_c"]][resp[["maa9d23s_sc4a10_c"]] == 2] <- 1
        #
        # resp[["maa9r18s_sc4a10_c"]][resp[["maa9r18s_sc4a10_c"]] == 2] <- 1

        resp[["maa9d13s_sc4a10_c"]][resp[["maa9d13s_sc4a10_c"]] %in% c(1, 2, 3)] <- 0
        resp[["maa9d13s_sc4a10_c"]][resp[["maa9d13s_sc4a10_c"]] == 4] <- 1
        resp[["maa9d13s_sc4a10_c"]][resp[["maa9d13s_sc4a10_c"]] %in% c(5, 6)] <- 2

        resp[["maa9d09s_sc4a10_c"]][resp[["maa9d09s_sc4a10_c"]] == 1] <- 0
        resp[["maa9d09s_sc4a10_c"]][resp[["maa9d09s_sc4a10_c"]] == 2] <- 1
        resp[["maa9d09s_sc4a10_c"]][resp[["maa9d09s_sc4a10_c"]] == 3] <- 2
        resp[["maa9d09s_sc4a10_c"]][resp[["maa9d09s_sc4a10_c"]] == 4] <- 3
        resp[["maa9d09s_sc4a10_c"]][resp[["maa9d09s_sc4a10_c"]] == 5] <- 4

        resp[["maa9v07s_sc4a10_c"]][resp[["maa9v07s_sc4a10_c"]] == 2] <- 1

        resp[["maa9r03s_sc4a10_c"]][resp[["maa9r03s_sc4a10_c"]] == 1] <- 0
        resp[["maa9r03s_sc4a10_c"]][resp[["maa9r03s_sc4a10_c"]] == 2] <- 1
        resp[["maa9r03s_sc4a10_c"]][resp[["maa9r03s_sc4a10_c"]] == 3] <- 2
      }
    } else if (domain == "IC") {
      if (wave == "w1") {
        resp[["icg9102s_c"]][resp[["icg9102s_c"]] == 1] <- 0
        resp[["icg9102s_c"]][resp[["icg9102s_c"]] == 2] <- 1
        resp[["icg9102s_c"]][resp[["icg9102s_c"]] == 3] <- 2
        resp[["icg9102s_c"]][resp[["icg9102s_c"]] == 4] <- 3

        resp[["icg9107s_c"]][resp[["icg9107s_c"]] == 1] <- 0
        resp[["icg9107s_c"]][resp[["icg9107s_c"]] == 2] <- 1
        resp[["icg9107s_c"]][resp[["icg9107s_c"]] == 3] <- 2
        resp[["icg9107s_c"]][resp[["icg9107s_c"]] == 4] <- 3
        resp[["icg9107s_c"]][resp[["icg9107s_c"]] == 5] <- 4

        resp[["icg9125s_c"]][resp[["icg9125s_c"]] == 1] <- 0
        resp[["icg9125s_c"]][resp[["icg9125s_c"]] == 2] <- 1
        resp[["icg9125s_c"]][resp[["icg9125s_c"]] == 3] <- 2
        resp[["icg9125s_c"]][resp[["icg9125s_c"]] == 4] <- 3

        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 1] <- 0
        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 2] <- 1
        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 3] <- 2
        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 4] <- 3
        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 5] <- 4
        resp[["icg9117s_c"]][resp[["icg9117s_c"]] == 6] <- 5

        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 1] <- 0
        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 2] <- 1
        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 3] <- 2
        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 4] <- 3
        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 5] <- 4
        resp[["icg9133s_c"]][resp[["icg9133s_c"]] == 6] <- 5

        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 1] <- 0
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 2] <- 1
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 3] <- 2
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 4] <- 3
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 5] <- 4
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 6] <- 5
        resp[["icg9136s_c"]][resp[["icg9136s_c"]] == 7] <- 6

        resp[["icg9140s_c"]][resp[["icg9140s_c"]] == 1] <- 0
        resp[["icg9140s_c"]][resp[["icg9140s_c"]] == 2] <- 1
        resp[["icg9140s_c"]][resp[["icg9140s_c"]] == 3] <- 2
        resp[["icg9140s_c"]][resp[["icg9140s_c"]] == 4] <- 3
      } else if (wave == "w7") {
        # Already collapsed in SUF!
        # resp[["icg12013s_c"]][resp[["icg12013s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12013s_c"]][resp[["icg12013s_c"]] == 3] <- 1

        # resp[["icg12018s_c"]][resp[["icg12018s_c"]] %in% c(1, 2, 3)] <- 0
        # resp[["icg12018s_c"]][resp[["icg12018s_c"]] == 4] <- 1

        # resp[["icg12060s_c"]][resp[["icg12060s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12060s_c"]][resp[["icg12060s_c"]] == 3] <- 1
        # resp[["icg12060s_c"]][resp[["icg12060s_c"]] == 4] <- 2

        # resp[["icg12016s_c"]][resp[["icg12016s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12016s_c"]][resp[["icg12016s_c"]] == 3] <- 1
        # resp[["icg12016s_c"]][resp[["icg12016s_c"]] == 4] <- 2

        # resp[["icg12054s_c"]][resp[["icg12054s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12054s_c"]][resp[["icg12054s_c"]] == 3] <- 1
        # resp[["icg12054s_c"]][resp[["icg12054s_c"]] == 4] <- 2

        # resp[["icg12138s_c"]][resp[["icg12138s_c"]] == 1] <- 0
        # resp[["icg12138s_c"]][resp[["icg12138s_c"]] == 2] <- 1
        # resp[["icg12138s_c"]][resp[["icg12138s_c"]] == 3] <- 2
        # resp[["icg12138s_c"]][resp[["icg12138s_c"]] == 4] <- 3

        # resp[["icg12028s_c"]][resp[["icg12028s_c"]] %in% c(1, 2, 3)] <- 0
        # resp[["icg12028s_c"]][resp[["icg12028s_c"]] == 4] <- 1
        # resp[["icg12028s_c"]][resp[["icg12028s_c"]] == 5] <- 2

        # resp[["ica5021s_c"]][resp[["ica5021s_c"]] %in% c(1, 2, 3)] <- 0
        # resp[["ica5021s_c"]][resp[["ica5021s_c"]] == 4] <- 1
        # resp[["ica5021s_c"]][resp[["ica5021s_c"]] == 5] <- 2

        # resp[["icg12107s_c"]][resp[["icg12107s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12107s_c"]][resp[["icg12107s_c"]] == 3] <- 1
        # resp[["icg12107s_c"]][resp[["icg12107s_c"]] == 4] <- 2
        # resp[["icg12107s_c"]][resp[["icg12107s_c"]] == 5] <- 3

        # resp[["ica5052s_c"]][resp[["ica5052s_c"]] %in% c(1, 2)] <- 0
        # resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 3] <- 1
        # resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 4] <- 2
        # resp[["ica5052s_c"]][resp[["ica5052s_c"]] == 5] <- 3

        # resp[["icg12048s_c"]][resp[["icg12048s_c"]] == 1] <- 0
        # resp[["icg12048s_c"]][resp[["icg12048s_c"]] == 2] <- 1
        # resp[["icg12048s_c"]][resp[["icg12048s_c"]] == 3] <- 2
        # resp[["icg12048s_c"]][resp[["icg12048s_c"]] == 4] <- 3
        # resp[["icg12048s_c"]][resp[["icg12048s_c"]] == 5] <- 4

        # resp[["icg12119s_c"]][resp[["icg12119s_c"]] == 1] <- 0
        # resp[["icg12119s_c"]][resp[["icg12119s_c"]] == 2] <- 1
        # resp[["icg12119s_c"]][resp[["icg12119s_c"]] == 3] <- 2
        # resp[["icg12119s_c"]][resp[["icg12119s_c"]] == 4] <- 3
        # resp[["icg12119s_c"]][resp[["icg12119s_c"]] == 5] <- 4

        # resp[["icg12050s_c"]][resp[["icg12050s_c"]] %in% c(1, 2, 3)] <- 0
        # resp[["icg12050s_c"]][resp[["icg12050s_c"]] == 4] <- 1
        # resp[["icg12050s_c"]][resp[["icg12050s_c"]] == 5] <- 2
        # resp[["icg12050s_c"]][resp[["icg12050s_c"]] == 6] <- 3

        # resp[["icg12004s_c"]][resp[["icg12004s_c"]] %in% c(1, 2)] <- 0
        # resp[["icg12004s_c"]][resp[["icg12004s_c"]] == 3] <- 1
        # resp[["icg12004s_c"]][resp[["icg12004s_c"]] == 4] <- 2
        # resp[["icg12004s_c"]][resp[["icg12004s_c"]] == 5] <- 3
        # resp[["icg12004s_c"]][resp[["icg12004s_c"]] == 6] <- 4

        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 1] <- 0
        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 2] <- 1
        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 2] <- 2
        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 4] <- 3
        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 5] <- 4
        # resp[["icg12046s_c"]][resp[["icg12046s_c"]] == 6] <- 5
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 1] <- 0
        resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 2] <- 1
        resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 3] <- 2
        resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 4] <- 3

        resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 1] <- 0
        resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 2] <- 1
        resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 3] <- 2
        resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 4] <- 3

        resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 1] <- 0
        resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 2] <- 1
        resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 3] <- 2
        resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 4] <- 3
      } else if (wave == "w5") {
        # collapsed in SUF!
        # CMC-items 14, 16, 17, 18, 19 and 21 were reduced to a 0 and 1
        # scoring since they showed a decrease in one or two of their step
        # parameters instead of an increase.
        # https://www.neps-data.de/Portals/0/Survey%20Papers/SP_VI.pdf
        # --> what categories were collapsed how? --> ask Kiel?
        # resp[["scg11083s_c"]][resp[["scg11083s_c"]] == 1] <- 0
        # resp[["scg11083s_c"]][resp[["scg11083s_c"]] == 2] <- 1
        # resp[["scg11083s_c"]][resp[["scg11083s_c"]] == 3] <- 2
        # resp[["scg11083s_c"]][resp[["scg11083s_c"]] == 4] <- 3

        # resp[["scg11032s_c"]][resp[["scg11032s_c"]] == 1] <- 0
        # resp[["scg11032s_c"]][resp[["scg11032s_c"]] == 2] <- 0
        # resp[["scg11032s_c"]][resp[["scg11032s_c"]] == 3] <- 1
        # resp[["scg11032s_c"]][resp[["scg11032s_c"]] == 4] <- 2

        # resp[["scg11652s_c"]][resp[["scg11652s_c"]] == 1] <- 0
        # resp[["scg11652s_c"]][resp[["scg11652s_c"]] == 2] <- 1
        # resp[["scg11652s_c"]][resp[["scg11652s_c"]] == 3] <- 2
        # resp[["scg11652s_c"]][resp[["scg11652s_c"]] == 4] <- 3

        # resp[["scg11602s_c"]][resp[["scg11602s_c"]] == 1] <- 0
        # resp[["scg11602s_c"]][resp[["scg11602s_c"]] == 2] <- 1
        # resp[["scg11602s_c"]][resp[["scg11602s_c"]] == 3] <- 2
        # resp[["scg11602s_c"]][resp[["scg11602s_c"]] == 4] <- 3

        # resp[["scs5131s_sc4g11_c"]][resp[["scs5131s_sc4g11_c"]] == 1] <- 0
        # resp[["scs5131s_sc4g11_c"]][resp[["scs5131s_sc4g11_c"]] == 2] <- 0
        # resp[["scs5131s_sc4g11_c"]][resp[["scs5131s_sc4g11_c"]] == 3] <- 1
        # resp[["scs5131s_sc4g11_c"]][resp[["scs5131s_sc4g11_c"]] == 4] <- 2
      }
    } else if (domain == "ST") {
      resp[["stg12nhs_c"]][resp[["stg12nhs_c"]] == 1] <- 0
      resp[["stg12nhs_c"]][resp[["stg12nhs_c"]] == 2] <- 1
      resp[["stg12nhs_c"]][resp[["stg12nhs_c"]] == 3] <- 2
      resp[["stg12nhs_c"]][resp[["stg12nhs_c"]] == 4] <- 3
      resp[["stg12nhs_c"]][resp[["stg12nhs_c"]] == 5] <- 4

      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 1] <- 0
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 2] <- 0
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 3] <- 1
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 4] <- 2
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 5] <- 3
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 6] <- 4
      resp[["stg12egs_c"]][resp[["stg12egs_c"]] == 7] <- 5

      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 1] <- 0
      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 2] <- 1
      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 3] <- 2
      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 4] <- 3
      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 5] <- 4
      resp[["stg12mts_c"]][resp[["stg12mts_c"]] == 6] <- 5

      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 1] <- 0
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 2] <- 0
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 3] <- 1
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 4] <- 2
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 5] <- 3
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 6] <- 4
      resp[["stg12cws_c"]][resp[["stg12cws_c"]] == 7] <- 5

      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 1] <- 0
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 2] <- 0
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 3] <- 0
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 4] <- 1
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 5] <- 2
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 6] <- 3
      resp[["stg12pds_c"]][resp[["stg12pds_c"]] == 7] <- 4
    } else if (domain == "EF") {
      if (wave == "w3") {
        # Collapsing according to the imputed values in the response data
        # collapsed if cell frequency < 100
        # no recoding necessary after imputation
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 1] <- 0
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 2] <- 1
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 3] <- 2
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 4] <- 3
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 5] <- 4
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 6] <- 5
        # resp[["efg10022s_c"]][resp[["efg10022s_c"]] == 7] <- 6

        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 1] <- 0
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 2] <- 1
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 3] <- 2
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 4] <- 3
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 5] <- 4
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 6] <- 5
        # resp[["efg10094s_c"]][resp[["efg10094s_c"]] == 7] <- 6

        # resp[["efg10002s_c"]][resp[["efg10002s_c"]] %in% 1:3] <- 0
        # resp[["efg10002s_c"]][resp[["efg10002s_c"]] == 4] <- 1
        # resp[["efg10002s_c"]][resp[["efg10002s_c"]] == 5] <- 2

        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] %in% 1:2] <- 0
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 3] <- 1
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 4] <- 2
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 5] <- 3
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 7] <- 4
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 7] <- 5
        # resp[["efg10098s_c"]][resp[["efg10098s_c"]] == 8] <- 6

        # resp[["efg10075s_c"]][resp[["efg10075s_c"]] == 3] <- 2
        # resp[["efg10075s_c"]][resp[["efg10075s_c"]] == 4] <- 3
      } else if (wave == "w7") {
        # Collapsing according to the imputed values in the response data
        # collapsed if cell frequency < 100
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 1] <- 0
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 2] <- 1
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 3] <- 2
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 4] <- 3
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 5] <- 4
        resp[["efg10022s_sc4g12_c"]][resp[["efg10022s_sc4g12_c"]] == 6] <- 5

        resp[["efg10108s_sc4g12_c"]][resp[["efg10108s_sc4g12_c"]] == 1] <- 0
        resp[["efg10108s_sc4g12_c"]][resp[["efg10108s_sc4g12_c"]] == 2] <- 1
        resp[["efg10108s_sc4g12_c"]][resp[["efg10108s_sc4g12_c"]] == 3] <- 2
        resp[["efg10108s_sc4g12_c"]][resp[["efg10108s_sc4g12_c"]] == 4] <- 3
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 1] <- 0
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 2] <- 1
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 3] <- 2
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 4] <- 3
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 5] <- 4
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 6] <- 5
        # resp[["efgc12a00s_c"]][resp[["efgc12a00s_c"]] == 7] <- 6

        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 1] <- 0
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 2] <- 1
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 3] <- 2
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 4] <- 3
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 5] <- 4
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 6] <- 5
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 7] <- 6
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 8] <- 7
        # resp[["efg12b00s_c"]][resp[["efg12b00s_c"]] == 9] <- 8
      }
    }
  } else if (SC == "SC3") {
    if (domain == "MA") {
      if (wave == "w1") {
        resp[["mag5v01s_c"]][resp[["mag5v01s_c"]] == 1] <- 0
        resp[["mag5v01s_c"]][resp[["mag5v01s_c"]] == 2] <- 1
        resp[["mag5v01s_c"]][resp[["mag5v01s_c"]] == 3] <- 2
        resp[["mag5v01s_c"]][resp[["mag5v01s_c"]] == 4] <- 3
      } else if (wave == "w3") {
        # already collapsed in SUF
      } else if (wave == "w5") {
        # need to be collapsed
        resp[["mag9d05s_c"]][resp[["mag9d05s_c"]] == 1] <- 0
        resp[["mag9d05s_c"]][resp[["mag9d05s_c"]] == 2] <- 1
        resp[["mag9d05s_c"]][resp[["mag9d05s_c"]] == 3] <- 2
        resp[["mag9d05s_c"]][resp[["mag9d05s_c"]] == 4] <- 3

        resp[["mag9d09s_c"]][resp[["mag9d09s_c"]] %in% c(1, 2, 3)] <- 0
        resp[["mag9d09s_c"]][resp[["mag9d09s_c"]] == 4] <- 1
        resp[["mag9d09s_c"]][resp[["mag9d09s_c"]] == 5] <- 2
        resp[["mag9d09s_c"]][resp[["mag9d09s_c"]] == 6] <- 3

        resp[["mag9r10s_c"]][resp[["mag9r10s_c"]] == 1] <- 0
        resp[["mag9r10s_c"]][resp[["mag9r10s_c"]] == 2] <- 1
        resp[["mag9r10s_c"]][resp[["mag9r10s_c"]] == 3] <- 2
        resp[["mag9r10s_c"]][resp[["mag9r10s_c"]] == 4] <- 3

        resp[["mag9r14s_c"]][resp[["mag9r14s_c"]] == 1] <- 0
        resp[["mag9r14s_c"]][resp[["mag9r14s_c"]] == 2] <- 1
        resp[["mag9r14s_c"]][resp[["mag9r14s_c"]] == 3] <- 2
        resp[["mag9r14s_c"]][resp[["mag9r14s_c"]] == 4] <- 3
      } else if (wave == "w9") {
        resp[["mas1q02s_sc3g12_c"]][resp[["mas1q02s_sc3g12_c"]] %in% c(1, 2, 3)] <- 0
        resp[["mas1q02s_sc3g12_c"]][resp[["mas1q02s_sc3g12_c"]] == 4] <- 1
      }
    } else if (domain == "RE"){
      if (wave == "w1") {
        # already collapsed in SUF
      } else if (wave == "w3") {
        # already collapsed in SUF
      } else if (wave == "w6") {
        resp[["reg9063s_c"]][resp[["reg9063s_c"]] == 1] <- 0
        resp[["reg9063s_c"]][resp[["reg9063s_c"]] == 2] <- 1
        resp[["reg9063s_c"]][resp[["reg9063s_c"]] == 3] <- 2

        resp[["reg9083s_c"]][resp[["reg9083s_c"]] == 1] <- 0
        resp[["reg9083s_c"]][resp[["reg9083s_c"]] == 2] <- 1

        resp[["reg9091s_c"]][resp[["reg9091s_c"]] == 1] <- 0
        resp[["reg9091s_c"]][resp[["reg9091s_c"]] == 2] <- 1
        resp[["reg9091s_c"]][resp[["reg9091s_c"]] == 3] <- 2
      } else if (wave == "w9") {
        resp[["reg120504s_sc3g12_c"]][resp[["reg120504s_sc3g12_c"]] == 1] <- 0
        resp[["reg120504s_sc3g12_c"]][resp[["reg120504s_sc3g12_c"]] == 2] <- 1

        resp[["reg121603s_sc3g12_c"]][resp[["reg121603s_sc3g12_c"]] == 1] <- 0
        resp[["reg121603s_sc3g12_c"]][resp[["reg121603s_sc3g12_c"]] == 2] <- 1

        resp[["reg122301s_sc3g12_c"]][resp[["reg122301s_sc3g12_c"]] == 1] <- 0
        resp[["reg122301s_sc3g12_c"]][resp[["reg122301s_sc3g12_c"]] == 2] <- 1

        resp[["reg122305s_sc3g12_c"]][resp[["reg122305s_sc3g12_c"]] == 1] <- 0
        resp[["reg122305s_sc3g12_c"]][resp[["reg122305s_sc3g12_c"]] == 2] <- 1
        resp[["reg122305s_sc3g12_c"]][resp[["reg122305s_sc3g12_c"]] == 3] <- 2
        resp[["reg122305s_sc3g12_c"]][resp[["reg122305s_sc3g12_c"]] == 4] <- 3

        # possibly removed from analysis
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 1] <- 0
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 2] <- 1
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 3] <- 2
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 4] <- 3
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 5] <- 4
        # resp[["reg122307s_sc3g12_c"]][resp[["reg122307s_sc3g12_c"]] == 6] <- 5

        resp[["reg121602s_sc3g12_c"]][resp[["reg121602s_sc3g12_c"]] %in% c(1, 2)] <- 0
        resp[["reg121602s_sc3g12_c"]][resp[["reg121602s_sc3g12_c"]] == 3] <- 1
        resp[["reg121602s_sc3g12_c"]][resp[["reg121602s_sc3g12_c"]] == 4] <- 2

        resp[["reg122005s_sc3g12_c"]][resp[["reg122005s_sc3g12_c"]] == 1] <- 0
        resp[["reg122005s_sc3g12_c"]][resp[["reg122005s_sc3g12_c"]] == 2] <- 1
        resp[["reg122005s_sc3g12_c"]][resp[["reg122005s_sc3g12_c"]] == 3] <- 2

        resp[["reg122907s_sc3g12_c"]][resp[["reg122907s_sc3g12_c"]] == 1] <- 0
        resp[["reg122907s_sc3g12_c"]][resp[["reg122907s_sc3g12_c"]] == 2] <- 1
        resp[["reg122907s_sc3g12_c"]][resp[["reg122907s_sc3g12_c"]] == 3] <- 2

        resp[["reg122504s_sc3g12_c"]][resp[["reg122504s_sc3g12_c"]] == 1] <- 0
        resp[["reg122504s_sc3g12_c"]][resp[["reg122504s_sc3g12_c"]] == 2] <- 1
        resp[["reg122504s_sc3g12_c"]][resp[["reg122504s_sc3g12_c"]] == 3] <- 2

        resp[["reg122501s_sc3g12_c"]][resp[["reg122501s_sc3g12_c"]] %in% c(1, 2)] <- 0
        resp[["reg122501s_sc3g12_c"]][resp[["reg122501s_sc3g12_c"]] == 3] <- 1
      }
    } else if (domain == "SC") {
      if (wave == "w2") { # already collapsed in SUF
        # resp[["scg6103s_c"]][resp[["scg6103s_c"]] == 1] <- 0
        # resp[["scg6103s_c"]][resp[["scg6103s_c"]] == 2] <- 0
        # resp[["scg6103s_c"]][resp[["scg6103s_c"]] == 3] <- 0
        # resp[["scg6103s_c"]][resp[["scg6103s_c"]] == 4] <- 1
        #
        # resp[["scg6142s_c"]][resp[["scg6142s_c"]] == 1] <- 0
        # resp[["scg6142s_c"]][resp[["scg6142s_c"]] == 2] <- 0
        # resp[["scg6142s_c"]][resp[["scg6142s_c"]] == 3] <- 0
        # resp[["scg6142s_c"]][resp[["scg6142s_c"]] == 4] <- 1
        #
        # resp[["scg6144s_c"]][resp[["scg6144s_c"]] == 1] <- 0
        # resp[["scg6144s_c"]][resp[["scg6144s_c"]] == 2] <- 0
        # resp[["scg6144s_c"]][resp[["scg6144s_c"]] == 3] <- 0
        # resp[["scg6144s_c"]][resp[["scg6144s_c"]] == 4] <- 1
        #
        # resp[["scg6661s_c"]][resp[["scg6661s_c"]] == 1] <- 0
        # resp[["scg6661s_c"]][resp[["scg6661s_c"]] == 2] <- 1
        # resp[["scg6661s_c"]][resp[["scg6661s_c"]] == 3] <- 2
        # resp[["scg6661s_c"]][resp[["scg6661s_c"]] == 4] <- 3
        #
        # resp[["scg6664s_c"]][resp[["scg6664s_c"]] == 1] <- 0
        # resp[["scg6664s_c"]][resp[["scg6664s_c"]] == 2] <- 0
        # resp[["scg6664s_c"]][resp[["scg6664s_c"]] == 3] <- 0
        # resp[["scg6664s_c"]][resp[["scg6664s_c"]] == 4] <- 1
        #
        # resp[["scg6111s_c"]][resp[["scg6111s_c"]] == 1] <- 0
        # resp[["scg6111s_c"]][resp[["scg6111s_c"]] == 2] <- 0
        # resp[["scg6111s_c"]][resp[["scg6111s_c"]] == 3] <- 0
        # resp[["scg6111s_c"]][resp[["scg6111s_c"]] == 4] <- 1
        #
        # resp[["scg6113s_c"]][resp[["scg6113s_c"]] == 1] <- 0
        # resp[["scg6113s_c"]][resp[["scg6113s_c"]] == 2] <- 0
        # resp[["scg6113s_c"]][resp[["scg6113s_c"]] == 3] <- 1
        # resp[["scg6113s_c"]][resp[["scg6113s_c"]] == 4] <- 2
        #
        # resp[["scg6061s_c"]][resp[["scg6061s_c"]] == 1] <- 0
        # resp[["scg6061s_c"]][resp[["scg6061s_c"]] == 2] <- 0
        # resp[["scg6061s_c"]][resp[["scg6061s_c"]] == 3] <- 0
        # resp[["scg6061s_c"]][resp[["scg6061s_c"]] == 4] <- 1
      } else if (wave == "w5") {
        resp[["scg9012s_sc3g9_c"]][resp[["scg9012s_sc3g9_c"]] == 1] <- 0
        resp[["scg9012s_sc3g9_c"]][resp[["scg9012s_sc3g9_c"]] == 2] <- 1
        resp[["scg9012s_sc3g9_c"]][resp[["scg9012s_sc3g9_c"]] == 3] <- 2

        # resp[["scg9611s_sc3g9_c"]][resp[["scg9611s_sc3g9_c"]] == 1] <- 0 # not part of the SUF
        # resp[["scg9611s_sc3g9_c"]][resp[["scg9611s_sc3g9_c"]] == 2] <- 1
        # resp[["scg9611s_sc3g9_c"]][resp[["scg9611s_sc3g9_c"]] == 3] <- 2

        resp[["scg9083s_sc3g9_c"]][resp[["scg9083s_sc3g9_c"]] == 1] <- 0
        resp[["scg9083s_sc3g9_c"]][resp[["scg9083s_sc3g9_c"]] == 2] <- 1
        resp[["scg9083s_sc3g9_c"]][resp[["scg9083s_sc3g9_c"]] == 3] <- 2
        resp[["scg9083s_sc3g9_c"]][resp[["scg9083s_sc3g9_c"]] == 4] <- 3

        resp[["scg9042s_sc3g9_c"]][resp[["scg9042s_sc3g9_c"]] == 1] <- 0
        resp[["scg9042s_sc3g9_c"]][resp[["scg9042s_sc3g9_c"]] == 2] <- 1
        resp[["scg9042s_sc3g9_c"]][resp[["scg9042s_sc3g9_c"]] == 3] <- 2

        resp[["scg9043s_sc3g9_c"]][resp[["scg9043s_sc3g9_c"]] == 3] <- 2
      } else if (wave == "w8") {
        # no TR: some items have already been dichotomized, others still need
        # collapsing
        #4: dichotomous
        #5
        resp[["scg11083s_sc3g11_c"]][resp[["scg11083s_sc3g11_c"]] == 1] <- 0
        resp[["scg11083s_sc3g11_c"]][resp[["scg11083s_sc3g11_c"]] == 2] <- 1
        resp[["scg11083s_sc3g11_c"]][resp[["scg11083s_sc3g11_c"]] == 3] <- 2
        #7: not needed
        #10: not needed
        #15
        resp[["scg11123s_sc3g11_c"]][resp[["scg11123s_sc3g11_c"]] == 1] <- 0
        resp[["scg11123s_sc3g11_c"]][resp[["scg11123s_sc3g11_c"]] == 2] <- 1
        resp[["scg11123s_sc3g11_c"]][resp[["scg11123s_sc3g11_c"]] == 3] <- 2
        resp[["scg11123s_sc3g11_c"]][resp[["scg11123s_sc3g11_c"]] == 4] <- 3
        #16: dichotomized
        #17: dichotomized
        #18: dichotomized
        #19: dichotomized
        #21: dichotomized
        #24: not needed
        #25
        resp[["scs5132s_sc3g11_c"]][resp[["scs5132s_sc3g11_c"]] == 1] <- 0
        resp[["scs5132s_sc3g11_c"]][resp[["scs5132s_sc3g11_c"]] == 2] <- 1
        resp[["scs5132s_sc3g11_c"]][resp[["scs5132s_sc3g11_c"]] == 3] <- 2
        resp[["scs5132s_sc3g11_c"]][resp[["scs5132s_sc3g11_c"]] == 4] <- 2
      }
    } else if (domain == "IC") {
      # if (wave == "w2") {
      #   # no polytomous items
      # } else
      if (wave == "w5") {
        # collapsed in SUF, but still cell frequencies < 200
      } else if (wave == "w9") {
        #1
        resp[["icg12018s_sc3g12_c"]][resp[["icg12018s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12018s_sc3g12_c"]][resp[["icg12018s_sc3g12_c"]] == 4] <- 1
        #3
        resp[["icg12107s_sc3g12_c"]][resp[["icg12107s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12107s_sc3g12_c"]][resp[["icg12107s_sc3g12_c"]] == 4] <- 1
        resp[["icg12107s_sc3g12_c"]][resp[["icg12107s_sc3g12_c"]] == 5] <- 2
        # 4
        resp[["icg12004s_sc3g12_c"]][resp[["icg12004s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12004s_sc3g12_c"]][resp[["icg12004s_sc3g12_c"]] == 3] <- 1
        resp[["icg12004s_sc3g12_c"]][resp[["icg12004s_sc3g12_c"]] == 4] <- 2
        resp[["icg12004s_sc3g12_c"]][resp[["icg12004s_sc3g12_c"]] == 5] <- 3
        resp[["icg12004s_sc3g12_c"]][resp[["icg12004s_sc3g12_c"]] == 6] <- 4
        # 8
        resp[["icg12060s_sc3g12_c"]][resp[["icg12060s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12060s_sc3g12_c"]][resp[["icg12060s_sc3g12_c"]] == 4] <- 1
        # 9
        resp[["icg12013s_sc3g12_c"]][resp[["icg12013s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12013s_sc3g12_c"]][resp[["icg12013s_sc3g12_c"]] == 3] <- 1
        #10
        resp[["ica4018s_sc3g12_c"]][resp[["ica4018s_sc3g12_c"]] == 1] <- 0
        resp[["ica4018s_sc3g12_c"]][resp[["ica4018s_sc3g12_c"]] %in% 2:5] <- 1
        resp[["ica4018s_sc3g12_c"]][resp[["ica4018s_sc3g12_c"]] %in% 6:7] <- 2
        resp[["ica4018s_sc3g12_c"]][resp[["ica4018s_sc3g12_c"]] == 8] <- 3
        #11
        resp[["icg12016s_sc3g12_c"]][resp[["icg12016s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12016s_sc3g12_c"]][resp[["icg12016s_sc3g12_c"]] == 3] <- 1
        resp[["icg12016s_sc3g12_c"]][resp[["icg12016s_sc3g12_c"]] == 4] <- 2
        #14
        resp[["icg12028s_sc3g12_c"]][resp[["icg12028s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12028s_sc3g12_c"]][resp[["icg12028s_sc3g12_c"]] == 4] <- 1
        resp[["icg12028s_sc3g12_c"]][resp[["icg12028s_sc3g12_c"]] == 5] <- 2
        #21: no collapsing needed
        #22
        resp[["icg12138s_sc3g12_c"]][resp[["icg12138s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12138s_sc3g12_c"]][resp[["icg12138s_sc3g12_c"]] == 3] <- 1
        resp[["icg12138s_sc3g12_c"]][resp[["icg12138s_sc3g12_c"]] == 4] <- 2
        #23
        resp[["icg12047s_sc3g12_c"]][resp[["icg12047s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12047s_sc3g12_c"]][resp[["icg12047s_sc3g12_c"]] == 3] <- 1
        resp[["icg12047s_sc3g12_c"]][resp[["icg12047s_sc3g12_c"]] == 4] <- 2
        resp[["icg12047s_sc3g12_c"]][resp[["icg12047s_sc3g12_c"]] == 5] <- 3
        resp[["icg12047s_sc3g12_c"]][resp[["icg12047s_sc3g12_c"]] == 6] <- 4
        #25
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 1] <- 0
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 2] <- 1
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 3] <- 2
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 4] <- 3
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 5] <- 4
        resp[["icg12046s_sc3g12_c"]][resp[["icg12046s_sc3g12_c"]] == 6] <- 5
        #26
        resp[["ica4021s_sc3g12_c"]][resp[["ica4021s_sc3g12_c"]] %in% 1:4] <- 0
        resp[["ica4021s_sc3g12_c"]][resp[["ica4021s_sc3g12_c"]] == 5] <- 1
        #27
        resp[["ica4052s_sc3g12_c"]][resp[["ica4052s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["ica4052s_sc3g12_c"]][resp[["ica4052s_sc3g12_c"]] == 3] <- 1
        resp[["ica4052s_sc3g12_c"]][resp[["ica4052s_sc3g12_c"]] == 4] <- 2
        resp[["ica4052s_sc3g12_c"]][resp[["ica4052s_sc3g12_c"]] == 5] <- 3
        #28
        resp[["icg12048s_sc3g12_c"]][resp[["icg12048s_sc3g12_c"]] %in% 1:2] <- 0
        resp[["icg12048s_sc3g12_c"]][resp[["icg12048s_sc3g12_c"]] == 3] <- 1
        resp[["icg12048s_sc3g12_c"]][resp[["icg12048s_sc3g12_c"]] == 4] <- 2
        resp[["icg12048s_sc3g12_c"]][resp[["icg12048s_sc3g12_c"]] == 5] <- 3
        #29
        resp[["icg12050s_sc3g12_c"]][resp[["icg12050s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12050s_sc3g12_c"]][resp[["icg12050s_sc3g12_c"]] == 4] <- 1
        resp[["icg12050s_sc3g12_c"]][resp[["icg12050s_sc3g12_c"]] == 5] <- 2
        resp[["icg12050s_sc3g12_c"]][resp[["icg12050s_sc3g12_c"]] == 6] <- 3
        #30
        resp[["icg12054s_sc3g12_c"]][resp[["icg12054s_sc3g12_c"]] %in% 1:3] <- 0
        resp[["icg12054s_sc3g12_c"]][resp[["icg12054s_sc3g12_c"]] == 3] <- 1
        resp[["icg12054s_sc3g12_c"]][resp[["icg12054s_sc3g12_c"]] == 4] <- 2
        #31
        resp[["icg12109s_sc3g12_c"]][resp[["icg12109s_sc3g12_c"]] == 1] <- 0
        resp[["icg12109s_sc3g12_c"]][resp[["icg12109s_sc3g12_c"]] == 2] <- 1
        resp[["icg12109s_sc3g12_c"]][resp[["icg12109s_sc3g12_c"]] == 3] <- 2
        resp[["icg12109s_sc3g12_c"]][resp[["icg12109s_sc3g12_c"]] == 4] <- 3
        #32
        resp[["icg12119s_sc3g12_c"]][resp[["icg12119s_sc3g12_c"]] == 1] <- 0
        resp[["icg12119s_sc3g12_c"]][resp[["icg12119s_sc3g12_c"]] == 2] <- 1
        resp[["icg12119s_sc3g12_c"]][resp[["icg12119s_sc3g12_c"]] == 3] <- 2
        resp[["icg12119s_sc3g12_c"]][resp[["icg12119s_sc3g12_c"]] == 4] <- 3
        resp[["icg12119s_sc3g12_c"]][resp[["icg12119s_sc3g12_c"]] == 5] <- 4
      }
    } else if (domain == "EF") {
      if (wave == "w7") {
        # Collapsing according to the imputed values in the response data
        # collapsed if cell frequency < 100
        # not necessary
      } else if (wave == "w9") {
        # # >100 is okay: collapsed according to A50/B41 - SC4 EF w7
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 1] <- 0
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 2] <- 1
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 3] <- 2
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 4] <- 3
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 5] <- 4
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 6] <- 5
        #
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 1] <- 0
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 2] <- 1
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 3] <- 2
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 4] <- 3
        # #1 # apparently already collapsed by SUF V.11.0.0
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 1] <- 0
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 2] <- 1#0
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 3] <- 2#1
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 4] <- 3#2
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 5] <- 4#3
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 6] <- 5#4
        # resp[["efg10022s_sc3g12_c"]][resp[["efg10022s_sc3g12_c"]] == 7] <- 6#5
        # #2
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 1] <- 0
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 2] <- 1
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 3] <- 2#1
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 4] <- 3#2
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 5] <- 4#3
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 6] <- 5#4
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 7] <- 6#5
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 8] <- 7#6
        # resp[["efg12b00s_sc3g12_c"]][resp[["efg12b00s_sc3g12_c"]] == 9] <- 8#7
        # #3
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 1] <- 0
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 2] <- 1
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 3] <- 2
        # resp[["efg10108s_sc3g12_c"]][resp[["efg10108s_sc3g12_c"]] == 4] <- 3
      }
    } else if (domain == "ST") {
      resp[["stg12nhs_sc3g12_c"]][resp[["stg12nhs_sc3g12_c"]] == 1] <- 0
      resp[["stg12nhs_sc3g12_c"]][resp[["stg12nhs_sc3g12_c"]] == 2] <- 1
      resp[["stg12nhs_sc3g12_c"]][resp[["stg12nhs_sc3g12_c"]] == 3] <- 2
      resp[["stg12nhs_sc3g12_c"]][resp[["stg12nhs_sc3g12_c"]] == 4] <- 3
      resp[["stg12nhs_sc3g12_c"]][resp[["stg12nhs_sc3g12_c"]] == 5] <- 4

      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 1] <- 0
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 2] <- 0
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 3] <- 1
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 4] <- 2
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 5] <- 3
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 6] <- 4
      resp[["stg12egs_sc3g12_c"]][resp[["stg12egs_sc3g12_c"]] == 7] <- 5

      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 1] <- 0
      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 2] <- 1
      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 3] <- 2
      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 4] <- 3
      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 5] <- 4
      resp[["stg12mts_sc3g12_c"]][resp[["stg12mts_sc3g12_c"]] == 6] <- 5

      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 1] <- 0
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 2] <- 0
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 3] <- 1
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 4] <- 2
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 5] <- 3
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 6] <- 4
      resp[["stg12cws_sc3g12_c"]][resp[["stg12cws_sc3g12_c"]] == 7] <- 5

      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 1] <- 0
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 2] <- 0
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 3] <- 0
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 4] <- 1
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 5] <- 2
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 6] <- 3
      resp[["stg12pds_sc3g12_c"]][resp[["stg12pds_sc3g12_c"]] == 7] <- 4
    # } else if (domain == "NR") {
    #   if (wave == "w3"){
    #     # no polytomous items
    #   } else if (wave == "w6") {
    #     # no polytomous items
    #   }
    # } else if (domain == "NT") {
    #   if (wave == "w3"){
    #     # no polytomous items
    #   } else if (wave == "w6") {
    #     # no polytomous items
    #   }
    } else if (domain == "LI") {
      if (wave == "w6") {
        resp[["lig9011s_c"]][resp[["lig9011s_c"]] %in% c(1, 2)] <- 0
        resp[["lig9011s_c"]][resp[["lig9011s_c"]] == 3] <- 1
        resp[["lig9011s_c"]][resp[["lig9011s_c"]] == 4] <- 2

        resp[["lig9012s_c"]][resp[["lig9012s_c"]] == 1] <- 0
        resp[["lig9012s_c"]][resp[["lig9012s_c"]] == 2] <- 1
        resp[["lig9012s_c"]][resp[["lig9012s_c"]] == 3] <- 2
        resp[["lig9012s_c"]][resp[["lig9012s_c"]] == 4] <- 3

        resp[["lig9014s_c"]][resp[["lig9014s_c"]] == 1] <- 0
        resp[["lig9014s_c"]][resp[["lig9014s_c"]] == 2] <- 1
        resp[["lig9014s_c"]][resp[["lig9014s_c"]] == 3] <- 2

        resp[["lig9015s_c"]][resp[["lig9015s_c"]] == 1] <- 0
        resp[["lig9015s_c"]][resp[["lig9015s_c"]] == 2] <- 1
        resp[["lig9015s_c"]][resp[["lig9015s_c"]] == 3] <- 2
        resp[["lig9015s_c"]][resp[["lig9015s_c"]] == 4] <- 3

        resp[["lig9016s_c"]][resp[["lig9016s_c"]] == 1] <- 0
        resp[["lig9016s_c"]][resp[["lig9016s_c"]] == 2] <- 1
        resp[["lig9016s_c"]][resp[["lig9016s_c"]] %in% c(3, 4)] <- 2

        resp[["lig9017s_c"]][resp[["lig9017s_c"]] == 1] <- 0
        resp[["lig9017s_c"]][resp[["lig9017s_c"]] == 2] <- 1
        resp[["lig9017s_c"]][resp[["lig9017s_c"]] == 3] <- 2
        resp[["lig9017s_c"]][resp[["lig9017s_c"]] == 4] <- 3

        resp[["lig9018s_c"]][resp[["lig9018s_c"]] %in% c(1, 2)] <- 0
        resp[["lig9018s_c"]][resp[["lig9018s_c"]] == 3] <- 1
        resp[["lig9018s_c"]][resp[["lig9018s_c"]] == 4] <- 2

        resp[["lig9021s_c"]][resp[["lig9021s_c"]] == 1] <- 0
        resp[["lig9021s_c"]][resp[["lig9021s_c"]] == 2] <- 1
        resp[["lig9021s_c"]][resp[["lig9021s_c"]] == 3] <- 2
        resp[["lig9021s_c"]][resp[["lig9021s_c"]] == 4] <- 3

        resp[["lig9022s_c"]][resp[["lig9022s_c"]] == 1] <- 0
        resp[["lig9022s_c"]][resp[["lig9022s_c"]] == 2] <- 1
        resp[["lig9022s_c"]][resp[["lig9022s_c"]] == 3] <- 2
        resp[["lig9022s_c"]][resp[["lig9022s_c"]] == 4] <- 3

        resp[["lig9023s_c"]][resp[["lig9023s_c"]] %in% c(1, 2)] <- 0
        resp[["lig9023s_c"]][resp[["lig9023s_c"]] == 3] <- 1
        resp[["lig9023s_c"]][resp[["lig9023s_c"]] == 4] <- 2

        resp[["lig9024s_c"]][resp[["lig9024s_c"]] %in% c(1, 2)] <- 0
        resp[["lig9024s_c"]][resp[["lig9024s_c"]] == 3] <- 1
        resp[["lig9024s_c"]][resp[["lig9024s_c"]] == 4] <- 2

        resp[["lig9025s_c"]][resp[["lig9025s_c"]] == 1] <- 0
        resp[["lig9025s_c"]][resp[["lig9025s_c"]] %in% c(2, 3)] <- 1

        resp[["lig9026s_c"]][resp[["lig9026s_c"]] == 1] <- 0
        resp[["lig9026s_c"]][resp[["lig9026s_c"]] == 2] <- 1
        resp[["lig9026s_c"]][resp[["lig9026s_c"]] == 3] <- 2
        resp[["lig9026s_c"]][resp[["lig9026s_c"]] == 4] <- 3

        resp[["lig9027s_c"]][resp[["lig9027s_c"]] == 1] <- 0
        resp[["lig9027s_c"]][resp[["lig9027s_c"]] == 2] <- 1
        resp[["lig9027s_c"]][resp[["lig9027s_c"]] == 3] <- 2
        resp[["lig9027s_c"]][resp[["lig9027s_c"]] == 4] <- 3

        resp[["lig9028s_c"]][resp[["lig9028s_c"]] == 1] <- 0
        resp[["lig9028s_c"]][resp[["lig9028s_c"]] == 2] <- 1
        resp[["lig9028s_c"]][resp[["lig9028s_c"]] == 3] <- 2
      }
    }
  } else if (SC == "SC2") {
    if (domain == "RE") {
      if (wave == "w6") {
        # collapsed in SUF
        # resp[["reg5012s_sc2g4_c"]][resp[["reg5012s_sc2g4_c"]] %in% c(1, 2, 3, 4, 5)] <- 0
        # resp[["reg5012s_sc2g4_c"]][resp[["reg5012s_sc2g4_c"]] == 6] <- 1
        # resp[["reg5012s_sc2g4_c"]][resp[["reg5012s_sc2g4_c"]] == 7] <- 2

        # resp[["reg5016s_sc2g4_c"]][resp[["reg5016s_sc2g4_c"]] == 6] <- 5

        # resp[["reg5026s_sc2g4_c"]][resp[["reg5026s_sc2g4_c"]] %in% c(1, 2, 3, 4, 5, 6)] <- 0
        # resp[["reg5026s_sc2g4_c"]][resp[["reg5026s_sc2g4_c"]] == 7] <- 1

        # only this one is not collapsed in the SUF
        resp[["reg5052s_sc2g4_c"]][resp[["reg5052s_sc2g4_c"]] == 1] <- 0
        resp[["reg5052s_sc2g4_c"]][resp[["reg5052s_sc2g4_c"]] == 2] <- 1
        resp[["reg5052s_sc2g4_c"]][resp[["reg5052s_sc2g4_c"]] == 3] <- 2
        resp[["reg5052s_sc2g4_c"]][resp[["reg5052s_sc2g4_c"]] == 4] <- 3

        # resp[["reg5055s_sc2g4_c"]][resp[["reg5055s_sc2g4_c"]] == 4] <- 3
      } else if (wave == "w9") {
        #
        resp[["reg7013s_sc2g7_c"]][resp[["reg7013s_sc2g7_c"]] == 1] <- 0
        resp[["reg7013s_sc2g7_c"]][resp[["reg7013s_sc2g7_c"]] == 2] <- 0
        resp[["reg7013s_sc2g7_c"]][resp[["reg7013s_sc2g7_c"]] == 3] <- 1

        resp[["reg7015s_sc2g7_c"]][resp[["reg7015s_sc2g7_c"]] == 1] <- 0
        resp[["reg7015s_sc2g7_c"]][resp[["reg7015s_sc2g7_c"]] == 2] <- 1

        resp[["reg7016s_sc2g7_c"]][resp[["reg7016s_sc2g7_c"]] == 1] <- 0
        resp[["reg7016s_sc2g7_c"]][resp[["reg7016s_sc2g7_c"]] == 2] <- 1
        resp[["reg7016s_sc2g7_c"]][resp[["reg7016s_sc2g7_c"]] == 3] <- 1
        resp[["reg7016s_sc2g7_c"]][resp[["reg7016s_sc2g7_c"]] == 4] <- 2

        resp[["reg7023s_sc2g7_c"]][resp[["reg7023s_sc2g7_c"]] == 1] <- 0
        resp[["reg7023s_sc2g7_c"]][resp[["reg7023s_sc2g7_c"]] == 2] <- 1
        resp[["reg7023s_sc2g7_c"]][resp[["reg7023s_sc2g7_c"]] == 3] <- 2

        resp[["reg7026s_sc2g7_c"]][resp[["reg7026s_sc2g7_c"]] == 1] <- 0
        resp[["reg7026s_sc2g7_c"]][resp[["reg7026s_sc2g7_c"]] == 2] <- 0
        resp[["reg7026s_sc2g7_c"]][resp[["reg7026s_sc2g7_c"]] == 3] <- 0
        resp[["reg7026s_sc2g7_c"]][resp[["reg7026s_sc2g7_c"]] == 4] <- 0
        resp[["reg7026s_sc2g7_c"]][resp[["reg7026s_sc2g7_c"]] == 5] <- 1

        resp[["reg7051s_sc2g7_c"]][resp[["reg7051s_sc2g7_c"]] == 1] <- 0
        resp[["reg7051s_sc2g7_c"]][resp[["reg7051s_sc2g7_c"]] == 2] <- 0
        resp[["reg7051s_sc2g7_c"]][resp[["reg7051s_sc2g7_c"]] == 3] <- 1

        resp[["reg7053s_sc2g7_c"]][resp[["reg7053s_sc2g7_c"]] == 1] <- 0
        resp[["reg7053s_sc2g7_c"]][resp[["reg7053s_sc2g7_c"]] == 2] <- 0
        resp[["reg7053s_sc2g7_c"]][resp[["reg7053s_sc2g7_c"]] == 3] <- 1

        resp[["reg7063s_sc2g7_c"]][resp[["reg7063s_sc2g7_c"]] == 1] <- 0
        resp[["reg7063s_sc2g7_c"]][resp[["reg7063s_sc2g7_c"]] == 2] <- 0
        resp[["reg7063s_sc2g7_c"]][resp[["reg7063s_sc2g7_c"]] == 3] <- 1

        resp[["reg7066s_sc2g7_c"]][resp[["reg7066s_sc2g7_c"]] == 1] <- 0
        resp[["reg7066s_sc2g7_c"]][resp[["reg7066s_sc2g7_c"]] == 2] <- 0
        resp[["reg7066s_sc2g7_c"]][resp[["reg7066s_sc2g7_c"]] == 3] <- 0
        resp[["reg7066s_sc2g7_c"]][resp[["reg7066s_sc2g7_c"]] == 4] <- 1

        resp[["reg7071s_sc2g7_c"]][resp[["reg7071s_sc2g7_c"]] == 1] <- 0
        resp[["reg7071s_sc2g7_c"]][resp[["reg7071s_sc2g7_c"]] == 2] <- 0
        resp[["reg7071s_sc2g7_c"]][resp[["reg7071s_sc2g7_c"]] == 3] <- 1

        resp[["reg7075s_sc2g7_c"]][resp[["reg7075s_sc2g7_c"]] == 1] <- 0
        resp[["reg7075s_sc2g7_c"]][resp[["reg7075s_sc2g7_c"]] == 2] <- 0
        resp[["reg7075s_sc2g7_c"]][resp[["reg7075s_sc2g7_c"]] == 3] <- 1
        resp[["reg7075s_sc2g7_c"]][resp[["reg7075s_sc2g7_c"]] == 4] <- 1

        resp[["reg7045s_sc2g7_c"]][resp[["reg7045s_sc2g7_c"]] == 1] <- 0
        resp[["reg7045s_sc2g7_c"]][resp[["reg7045s_sc2g7_c"]] == 2] <- 1
        resp[["reg7045s_sc2g7_c"]][resp[["reg7045s_sc2g7_c"]] == 3] <- 1

        resp[["reg7045s_sc2g7_c_d"]][resp[["reg7045s_sc2g7_c_d"]] == 1] <- 0
        resp[["reg7045s_sc2g7_c_d"]][resp[["reg7045s_sc2g7_c_d"]] == 2] <- 1
        resp[["reg7045s_sc2g7_c_d"]][resp[["reg7045s_sc2g7_c_d"]] == 3] <- 1

        resp[["reg7024s_sc2g7_c"]][resp[["reg7024s_sc2g7_c"]] == 1] <- 0
        resp[["reg7024s_sc2g7_c"]][resp[["reg7024s_sc2g7_c"]] == 2] <- 1

        resp[["reg7024s_sc2g7_c_d"]][resp[["reg7024s_sc2g7_c_d"]] == 1] <- 0
        resp[["reg7024s_sc2g7_c_d"]][resp[["reg7024s_sc2g7_c_d"]] == 2] <- 1

        resp[["reg7033s_sc2g7_c"]][resp[["reg7033s_sc2g7_c"]] == 1] <- 0
        resp[["reg7033s_sc2g7_c"]][resp[["reg7033s_sc2g7_c"]] == 2] <- 1

        resp[["reg7033s_sc2g7_c_d"]][resp[["reg7033s_sc2g7_c_d"]] == 1] <- 0
        resp[["reg7033s_sc2g7_c_d"]][resp[["reg7033s_sc2g7_c_d"]] == 2] <- 1
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        # data already collapsed in SUF (according to TR)
      } else if (wave == "w3") {
        # data already collapsed in SUF (according to TR)
      } else if (wave == "w5") {
        # data already collapsed in SUF (according to TR)
      } else if (wave == "w9") {
        # data already collapsed in SUF
        # (cf. TR; although some categories are still <200)
      }
    } else if (domain == "MA") {
      # if (wave == "w2") {
      # no polytomous items
      # } else
      if (wave == "w3") {
        # already collapsed in SUF
        # #8
        # resp[["mag1z20s_c"]][resp[["mag1z20s_c"]] %in% c(1, 2)] <- 0
        # resp[["mag1z20s_c"]][resp[["mag1z20s_c"]] == 3] <- 1
        # resp[["mag1z20s_c"]][resp[["mag1z20s_c"]] == 4] <- 2
        # #9
        # resp[["mag1d09s_c"]][resp[["mag1d09s_c"]] == 1] <- 0
        # resp[["mag1d09s_c"]][resp[["mag1d09s_c"]] == 2] <- 1
        # resp[["mag1d09s_c"]][resp[["mag1d09s_c"]] == 3] <- 2
        # resp[["mag1d09s_c"]][resp[["mag1d09s_c"]] == 4] <- 3
        # #21
        # resp[["mag1r19s_c"]][resp[["mag1r19s_c"]] %in% c(1, 2, 3, 4)] <- 0
        # resp[["mag1r19s_c"]][resp[["mag1r19s_c"]] == 5] <- 1
      } else if (wave == "w4") {
        #10
        resp[["mag1d09s_sc2g2_c"]][resp[["mag1d09s_sc2g2_c"]] == 1] <- 0
        resp[["mag1d09s_sc2g2_c"]][resp[["mag1d09s_sc2g2_c"]] == 2] <- 1
        resp[["mag1d09s_sc2g2_c"]][resp[["mag1d09s_sc2g2_c"]] == 3] <- 2
        resp[["mag1d09s_sc2g2_c"]][resp[["mag1d09s_sc2g2_c"]] == 4] <- 3
        #12
        resp[["mag2g12s_c"]][resp[["mag2g12s_c"]] == 1] <- 0
        resp[["mag2g12s_c"]][resp[["mag2g12s_c"]] == 2] <- 1
        resp[["mag2g12s_c"]][resp[["mag2g12s_c"]] == 3] <- 2
        resp[["mag2g12s_c"]][resp[["mag2g12s_c"]] == 4] <- 3
        #22
        resp[["mag1r19s_sc2g2_c"]][resp[["mag1r19s_sc2g2_c"]] %in% c(1, 2, 3, 4)] <- 0
        resp[["mag1r19s_sc2g2_c"]][resp[["mag1r19s_sc2g2_c"]] == 5] <- 1
      } else if (wave == "w6") {
        resp[["mag4d14s_c"]][resp[["mag4d14s_c"]] == 1] <- 0
        resp[["mag4d14s_c"]][resp[["mag4d14s_c"]] %in% c(2, 3)] <- 0
        resp[["mag4d14s_c"]][resp[["mag4d14s_c"]] == 4] <- 0
        resp[["mag4d14s_c"]][resp[["mag4d14s_c"]] == 5] <- 1
      } else if (wave == "w9") {
        resp[["mag7d06s_c"]][resp[["mag7d06s_c"]] %in% c(1, 2, 3)] <- 0
        resp[["mag7d06s_c"]][resp[["mag7d06s_c"]] == 4] <- 1
        resp[["mag7d06s_c"]][resp[["mag7d06s_c"]] == 5] <- 2

        resp[["mag7r02s_sc2g7_c"]][resp[["mag7r02s_sc2g7_c"]] == 1] <- 0
        resp[["mag7r02s_sc2g7_c"]][resp[["mag7r02s_sc2g7_c"]] == 2] <- 1
        resp[["mag7r02s_sc2g7_c"]][resp[["mag7r02s_sc2g7_c"]] == 3] <- 2
      }
    }
  } else if (SC == "SC1") {
    if (domain == "MA") {
      if (wave == "w5") {
        # 1: no need
        # 5: 1 --> 0, 2 --> 1, 3 --> 2
        resp[["man5r14s_c"]][resp[["man5r14s_c"]] == 1] <- 0
        resp[["man5r14s_c"]][resp[["man5r14s_c"]] == 2] <- 1
        resp[["man5r14s_c"]][resp[["man5r14s_c"]] == 3] <- 2
      }
      # wave 7: no polytomous items
    }
    if (domain == "SC") {
      if (wave == "w8") { # collapsed in SUF
        #resp[["scg1652s_sc1n8_c"]][resp[["scg1652s_sc1n8_c"]] %in% c(1, 2)] <- 0
        #resp[["scg1652s_sc1n8_c"]][resp[["scg1652s_sc1n8_c"]] == 3] <- 1
        #resp[["scg1652s_sc1n8_c"]][resp[["scg1652s_sc1n8_c"]] == 4] <- 2

        #resp[["scg1011s_sc1n8_c"]][resp[["scg1011s_sc1n8_c"]] %in% c(1, 2)] <- 0
        #resp[["scg1011s_sc1n8_c"]][resp[["scg1011s_sc1n8_c"]] == 3] <- 1
        #resp[["scg1011s_sc1n8_c"]][resp[["scg1011s_sc1n8_c"]] == 4] <- 2
      }
    }
    # SC, VO, CD: no need
  }
  resp
}
