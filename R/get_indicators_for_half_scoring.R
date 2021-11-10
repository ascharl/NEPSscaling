#' scoring function for PCM items (and occasionally dichotomous items)
#'
#' @param SC String representation of starting cohort
#' @param wave String representation of current wave of test assessment
#' @param domain String representation of current competency domain
#'
#' @return charachter vector of polytomous variable names
#' @noRd

get_indicators_for_half_scoring <- function(SC, domain, wave) {
  if (SC == "SC1") {
    # if (domain == "CD") {} # no polytomous items
    # if (domain == "VO") {} # no polytomous items
    if (domain == "SC") {
      if (wave == "w6") {
        ind <- c("scn6130s_c", "sck1102s_sc1n6_c", "sck1162s_sc1n6_c")
      } else if (wave == "w8") {
        ind <- c("scg1652s_sc1n8_c", "scg1011s_sc1n8_c")
      }
    } else if (domain == "MA") {
      if (wave == "w5") {
        ind <- c("man5z17s_c", "man5r14s_c")
      } else if (wave == "w7") {
        ind <- c()
      } else if (wave == "w9") {
        ind <- c()
      }
    }
  } else if (SC == "SC2") {
    # if (domain == "VO") {} # no polytomous items
    #
    if (domain == "RE") {
      if (wave == "w6") {
        ind <- c("reg5012s_sc2g4_c", "reg5016s_sc2g4_c", "reg5026s_sc2g4_c",
                 "reg5042s_sc2g4_c", "reg5052s_sc2g4_c", "reg5055s_sc2g4_c")
      } else if (wave == "w9") {
        ind <- c("reg7013s_sc2g7_c", "reg7015s_sc2g7_c", "reg7016s_sc2g7_c",
                 "reg7023s_sc2g7_c", "reg7024s_sc2g7_c", "reg7024s_sc2g7_c_d",
                 "reg7026s_sc2g7_c", "reg7033s_sc2g7_c", "reg7033s_sc2g7_c_d",
                 "reg7045s_sc2g7_c", "reg7045s_sc2g7_c_d", "reg7051s_sc2g7_c",
                 "reg7053s_sc2g7_c", "reg7055s_sc2g7_c", "reg7063s_sc2g7_c",
                 "reg7066s_sc2g7_c", "reg7071s_sc2g7_c", "reg7075s_sc2g7_c")
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        ind <- c("sck1102s_c", "sck1033s_c", "sck1023s_c", "sck1162s_c")
      } else if (wave == "w3") {
        ind <- c("scg1652s_c", "scg1091s_c", "scg1011s_c")
      } else if (wave == "w5") {
        ind <- c("scg3181s_c", "scg3131s_c", "scg3641s_c", "scg3091s_c")
      } else if (wave == "w9") {
        ind <- c("scg9611s_c", "scg9042s_c", "scg9043s_c", "scg9012s_c",
                 "scg9052s_c", "scg6142s_c", "scg6144s_c", #"scg9061s_c",
                 "scg9651s_c", "scg9621s_c")
      }
    } else if (domain == "MA") {
      if (wave == "w2") {
        ind <- c("mak2v08s_c")
      } else if (wave == "w3") {
        ind <- c("mag1v01s_c","mag1z20s_c",
                 "mag1d09s_c")
      } else if (wave == "w4") {
        ind <- c("mag1d09s_sc2g2_c", "mag2g12s_c")
      } else if (wave == "w6") {
        ind <- c()
      } else if (wave == "w9") {
        ind <- c("mag7d06s_c", "mag7r02s_sc2g7_c")
      }
    }
  } else if (SC == "SC3") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c("mag5v01s_c")
      } else if (wave == "w3") {
        ind <- c("mag7r02s_c")
      } else if (wave == "w5") {
        ind <- c("mag9d05s_c", "mag9d09s_c", "mag9r10s_c", "mag9r14s_c",
                 "mag9v13s_sc3g9_c")
      } else if (wave == "w9") {
        ind <- c("mas1q021s_sc3g12_c")
      }
    } else if (domain == "RE") {
      if (wave == "w1") {
        ind <- c("reg5012s_c", "reg5016s_c", "reg5026s_c", "reg5042s_c",
                 "reg5052s_c", "reg5055s_c")
      } else if (wave == "w3") {
        ind <- c("reg7013s_c", "reg7016s_c", "reg7063s_c", "reg7066s_c",
                 "reg7023s_c", "reg7024s_c", "reg7026s_c", "reg7033s_c",
                 "reg7045s_c", "reg7051s_c", "reg7053s_c", "reg7055s_c",
                 "reg7071s_c", "reg7075s_c")
      } else if (wave == "w6") {
        # ind <- c(3, 10, 22, 24, 30, 37)
        ind <- c("reg9063s_c", "reg90640_c", "reg90840_c", "reg90860_c",
                 "reg9091s_c", "reg9097s_c", "reg90430_sc3g9_c",
                 "reg9047s_sc3g9_c")
      } else if (wave == "w9") {
        ind <- c("reg120504s_sc3g12_c", "reg122305s_sc3g12_c",
                 "reg121602s_sc3g12_c", "reg121605s_sc3g12_c",
                 "reg122002s_sc3g12_c", "reg122005s_sc3g12_c",
                 "reg122907s_sc3g12_c", "reg122504s_sc3g12_c")
      }
    } else if (domain == "SC") {
      if (wave == "w2") {
        ind <- c("scg6661s_c", "scg6113s_c")
      } else if (wave == "w5") {
        ind <- c("scg9012s_sc3g9_c", "scg9052s_sc3g9_c", "scg9611s_sc3g9_c",
                 "scg9061s_sc3g9_c", "scg9083s_sc3g9_c", "scg9042s_sc3g9_c",
                 "scg9043s_sc3g9_c", "scg9651s_sc3g9_c", "scg9621s_sc3g9_c",
                 "scg9771s_c", "scg9751s_c", "scg9752s_c")
      } else if (wave == "w8") {
        ind <- c("scg11083s_sc3g11_c", "scg11032s_sc3g11_c",
                 "scg11652s_sc3g11_c", "scg11123s_sc3g11_c",
                 "scs5131s_sc3g11_c", "scs5132s_sc3g11_c")
      }
    } else if (domain == "IC") {
      # if (wave == "w2") {
      #   ind <- c() # no polytomous items
      # } else
      if (wave == "w5") {
        ind <- c("icg12060s_sc3g9_c", "icg12018s_sc3g9_c", "icg9140s_sc3g9_c",
                 "icg9102s_sc3g9_c", "icg9117s_sc3g9_c", "ica5021s_sc3g9_c",
                 "icg9133s_sc3g9_c", "icg9136s_sc3g9_c", "icg9107s_sc3g9_c",
                 "icg12138s_sc3g9_c","icg12016s_sc3g9_c", "icg12047s_sc3g9_c",
                 "icg12046s_sc3g9_c","ica5052s_sc3g9_c", "icg9125s_sc3g9_c",
                 "icg12050s_sc3g9_c")
      } else if (wave == "w9") {
        ind <- c("icg12107s_sc3g12_c", "icg12004s_sc3g12_c",
                 "ica4018s_sc3g12_c",  "icg12016s_sc3g12_c",
                 "icg12028s_sc3g12_c", "icg12037s_sc3g12_c",
                 "icg12138s_sc3g12_c", "icg12047s_sc3g12_c",
                 "icg12046s_sc3g12_c", "ica4052s_sc3g12_c",
                 "icg12048s_sc3g12_c", "icg12050s_sc3g12_c",
                 "icg12054s_sc3g12_c", "icg12109s_sc3g12_c",
                 "icg12119s_sc3g12_c")
      }
    } else if (domain == "EF") {
      if (wave == "w7") {
        ind <- c("efg10022s_sc3g10_c", "efg10108s_sc3g10_c",
                 "efg10094s_sc3g10_c", "efg10059s_sc3g10_c",
                 "efg10002s_sc3g10_c", "efg10008s_sc3g10_c",
                 "efg10098s_sc3g10_c", "efg10075s_sc3g10_c")
      } else if (wave == "w9") {
        ind <- c("efg12a00s_sc3g12_c", "efg12b00s_sc3g12_c", "efg12c00s_sc3g12_c")#c("efg10022s_sc3g12_c", "efg12b00s_sc3g12_c", "efg10108s_sc3g12_c")
      }
    } else if (domain == "ST") {
      ind <- c("stg12nhs_sc3g12_c", "stg12egs_sc3g12_c", "stg12mts_sc3g12_c",
               "stg12cws_sc3g12_c",  "stg12pds_sc3g12_c")
    } else if (domain == "LI") {
      ind <- c("lig9011s_c", "lig9012s_c","lig9013s_c", "lig9014s_c",
               "lig9015s_c", "lig9016s_c", "lig9017s_c", "lig9018s_c",
               "lig9021s_c", "lig9022s_c", "lig9023s_c", "lig9024s_c",
               "lig9026s_c", "lig9027s_c", "lig9028s_c")
    }
  } else if (SC == "SC4") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c("mag9v13s_c", "mag9r25s_c")
      } else if (wave == "w7") {
        # items 10 and 15 appear twice for different sample groups!
        ind <- c("mas1q02s_sc4g12_c") # 19 ohne item splits!
      } else if (wave == "w10") {
        ind <- c("mas1v032_sc4a10_c", "maa9d09s_sc4a10_c", "maa9d13s_sc4a10_c",
                 "maa9r03s_sc4a10_c", "mas1q02s_sc4a10_c", "maa9v27s_sc4a10_c",
                 "maa9d20s_sc4a10_c", "maa9r301_sc4a10_c", "maa9r26s_sc4a10_c")
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        ind <- list(c("scg9012s_c", "scg9052s_c"),
                    c("scg9611s_c", "scg9061s_c", "scg9083s_c", "scg9042s_c",
                      "scg9043s_c", "scg9651s_c", "scg9621s_c"))
      } else if (wave == "w5") {
        ind <- c("scg11083s_c", "scg11032s_c", "scg11652s_c", "scg11602s_c",
                 "scg11123s_c", "scs5131s_sc4g11_c")
      }
    } else if (domain == "RE") {
      if (wave == "w2") {
        ind <- c("reg9016s_c", "reg9017s_c", "reg9033s_c", "reg9047s_c")
      } else if (wave == "w7") {
        ind <- c("reg12014s_c", "reg12021s_c", "reg12024s_c", "reg12026s_c",
                 "reg12042s_c", "reg12044s_c", "reg12052s_c", "reg12055s_c",
                 "reg12065s_c", "reg12071s_c", "reg12075s_c")
      } else if (wave == "w10") {
        ind <- c("rea90101s_sc4a10_c", "rea90102s_sc4a10_c",
                 "rea901030_sc4a10_c", "rea90104s_sc4a10_c",
                 "rea90105s_sc4a10_c", "rea90604s_sc4a10_c",
                 "rea90201s_sc4a10_c", "rea90205s_sc4a10_c",
                 "rea90206s_sc4a10_c", "rea90305s_sc4a10_c",
                 "rea90307s_sc4a10_c", "rea90402s_sc4a10_c",
                 "rea90403s_sc4a10_c", "rea90704s_sc4a10_c")
      }
    } else if (domain == "EF") {
      if (wave == "w3") {
        ind <- c("efg10022s_c", "efg10108s_c", "efg10094s_c", "efg10059s_c",
                 "efg10002s_c", "efg10008s_c", "efg10098s_c", "efg10075s_c")
      } else if (wave == "w7") {
        ind <- c("efg10022s_sc4g12_c", "efg12b00s_c", "efg10108s_sc4g12_c",
                 "efg12d001_c")
      }
    } else if (domain == "IC") {
      if (wave == "w1") {
        ind <- c("icg9102s_c", "icg9107s_c", "icg9117s_c", "icg9125s_c",
                 "icg9133s_c", "icg9136s_c", "icg9140s_c")
      } else if (wave == "w7") {
        ind <- c("icg12018s_c", "icg12107s_c", "icg12004s_c", "icg12060s_c",
                 "icg12013s_c", "icg12016s_c", "icg12028s_c", "icg12037s_c",
                 "icg12138s_c", "icg12047s_c", "icg12046s_c", "ica5021s_c",
                 "ica5052s_c", "icg12048s_c", "icg12050s_c", "icg12054s_c",
                 "icg12109s_c", "icg12119s_c")
      }
    } else if (domain == "ST") {
      ind <- c("stg12nhs_c", "stg12egs_c", "stg12mts_c", "stg12cws_c",
               "stg12pds_c")
    }
  } else if (SC == "SC5") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c("mas1q02s_c")
      } else if (wave == "w12") {
        ind <- c("mas1v032_sc5s12_c", "maa9d09s_sc5s12_c", "maa9d13s_sc5s12_c",
                 "maa9r03s_sc5s12_c", "mas1q02s_sc5s12_c", "maa9v27s_sc5s12_c",
                 "maa9d20s_sc5s12_c", "maa9r301_sc5s12_c", "maa9r26s_sc5s12_c")
      }
    } else if (domain == "RE") {
      if (wave == "w1") {
        ind <- c("res1012s_c", "res1021s_c", "res1022s_c", "res1024s_c",
                 "res1032s_c", "res1043s_c")
      } else if (wave == "w12") {
        ind <- c("rea90604s_sc5s12_c", "rea90201s_sc5s12_c",
                 "rea90205s_sc5s12_c", "rea90206s_sc5s12_c",
                 "rea90305s_sc5s12_c", "rea90402s_sc5s12_c",
                 "rea90704s_sc5s12_c")
      }
    } else if (domain == "EF") {
      if (wave == "w12") {
        ind <- c("efs12402s_c")
      }
    } else if (domain == "IC") {
      if (wave == "w5") {
        ind <- c("ics3002s_c", "ics3029s_c", "ics3049s_c", "ics3018s_c",
                 "ics3035s_c")
      }
    } else if (domain == "SC") {
      if (wave == "w5") {
        ind <- c("scs3623s_c", "scs3021s_c", "scs3022s_c", "scs3643s_c",
                 "scs3642s_c", "scs3031s_c", "scs3033s_c", "scs3112s_c",
                 "scs3131s_c", "scs3132s_c", "scs3133s_c", "scs3012s_c",
                 "scs3061s_c")
      }
    }
  } else if (SC == "SC6") {
    if (domain == "MA") {
      if (wave == "w9") {
        ind <- c("mas1v032_sc6a9_c", "maa9d09s_c", "maa9d13s_c", "maa9r03s_c",
                 "mas1q02s_sc6a9_c", "maa9v27s_c", "maa9d20s_c", "maa9r301_c",
                 "maa9r26s_c")
      }
    } else if (domain == "RE") {
      if (wave %in% c("w3", "w5")) {
        ind <- c("rea3012s_c", "rea3015s_c", "rea3028s_c", "rea3038s_c",
                 "rea3042s_c", "rea3052s_c", "rea3054s_c")
      } else if (wave == "w9") {
        ind <- c("rea90101s_c", "rea90102s_c", "rea901030_c", "rea90104s_c",
                 "rea90105s_c", "rea90604s_c", "rea90201s_c", "rea90205s_c",
                 "rea90206s_c", "rea90305s_c", "rea90307s_c", "rea90402s_c",
                 "rea90403s_c", "rea90704s_c")
      }
    } else if (domain == "SC") {
      if (wave == "w5") {
        ind <- c("sca5652s_c", "sca5091s_c")
      }
    } else if (domain == "IC") {
      if (wave == "w5") {
        ind <- c("ica5004s_c", "ica5017s_c", "ica5018s_c", "ica5015s_c",
                 "ica5016s_c", "ica5020s_c", "ica5050s_c", "ica5047s_c",
                 "ica5021s_c", "ica5052s_c")
      }
    }
  }
  ind
}



# #' scoring function for PCM items (and occasionally dichotomous items)
# #'
# #' @param SC String representation of starting cohort
# #' @param wave String representation of current wave of test assessment
# #' @param domain String representation of current competency domain
# #'
# #' @noRd

# get_indicators_for_half_scoring2 <- function(SC, domain, wave) {
  # if (SC == "SC5") {
    # if (domain == "MA") {
      # if (wave == "w1") {
        # ind <- c(13)
      # } else if (wave == "w12") {
        # ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      # }
    # } else if (domain == "RE") {
      # if (wave == "w1") {
        # ind <- c(2, 9, 10, 12, 17, 26)
      # } else if (wave == "w12") {
        # ind <- c(4, 5, 8, 9, 14, 17, 21)
      # }
    # } else if (domain == "EF") {
      # if (wave == "w12") {
        # ind <- c(19)
      # }
    # } else if (domain == "IC") {
      # if (wave == "w5") {
        # ind <- c(2, 15, 17, 18, 26)
      # }
    # } else if (domain == "SC") {
      # if (wave == "w5") {
        # ind <- c(4, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26)
      # }
    # }
  # } else if (SC == "SC6") {
    # if (domain == "MA") {
      # if (wave == "w9") {
        # ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      # }
    # } else if (domain == "RE") {
      # if (wave %in% c("w3", "w5")) {
        # ind <- c(2, 5, 11, 19, 21, 29, 27)
      # } else if (wave == "w9") {
        # ind <- c(1, 2, 3, 4, 5, 9, 10, 14, 15, 20, 22, 24, 25, 36)
      # }
    # } else if (domain == "SC") {
      # if (wave == "w5") {
        # ind <- c(6, 16)
      # }
    # } else if (domain == "IC") {
      # if (wave == "w5") {
        # ind <- c(4, 9, 10, 11, 13, 14, 22, 24, 26, 27)
      # }
    # }
  # } else if (SC == "SC4") {
    # if (domain == "MA") {
      # if (wave == "w1") {
        # ind <- c(3, 16)
      # } else if (wave == "w7") {
        # # items 10 and 15 appear twice for different sample groups!
        # ind <- c(21) # 19 ohne item splits!
      # } else if (wave == "w10") {
        # ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      # }
    # } else if (domain == "SC") {
      # if (wave == "w1") {
        # ind <- list(c(2, 4), c(7, 11, 14, 18, 19, 20, 24))
      # } else if (wave == "w5") {
        # ind <- c(5, 7, 10, 11, 15, 24)
      # }
    # } else if (domain == "RE") {
      # if (wave == "w2") {
        # ind <- c(4, 5, 13, 24)
      # } else if (wave == "w7") {
        # ind <- c(4, 8, 11, 13, 20, 22, 25, 28, 34, 37, 41)
      # } else if (wave == "w10") {
        # ind <- c(1, 2, 3, 4, 5, 9, 10, 14, 15, 20, 22, 24, 25, 36)
      # }
    # } else if (domain == "EF") {
      # if (wave == "w3") {
        # ind <- c(1, 2, 3, 4, 5, 6, 7, 11)
      # } else if (wave == "w7") {
        # ind <- c(1, 2, 3, 4)
      # }
    # } else if (domain == "IC") {
      # if (wave == "w1") {
        # ind <- c(2, 7, 15, 22, 30, 33, 36)
      # } else if (wave == "w7") {
        # ind <- c(1, 3, 4, 8, 9, 10, 13, 20, 21, 22, 24, 25, 26,
                 # 27, 28, 29, 30, 31)
      # }
    # } else if (domain == "ST") {
      # ind <- 1:5
    # }
  # } else if (SC == "SC3") {
    # if (domain == "MA") {
      # if (wave == "w1") {
        # ind <- c(20)
      # } else if (wave == "w3") {
        # ind <- c(23)
      # } else if (wave == "w5") {
        # ind <- c(3, 6, 26, 27, 33)
      # } else if (wave == "w9") {
        # ind <- c(14)
      # }
    # } else if (domain == "RE") {
      # if (wave == "w1") {
        # ind <- c(2, 6, 13, 22, 27, 30)
      # } else if (wave == "w3") {
        # ind <- c(3, 6, 9, 12, 15, 16, 18, 21, 29, 31, 33, 34, 36, 40)
      # } else if (wave == "w6") {
        # # ind <- c(3, 10, 22, 24, 30, 37)
        # ind <- c(3, 4, 11, 13, 33, 24, 30, 37)
      # } else if (wave == "w9") {
        # ind <- c(4, 11, 19, 22, 25, 28, 34, 41)
      # }
    # } else if (domain == "SC") {
      # if (wave == "w2") {
        # ind <- c(15, 20)
      # } else if (wave == "w5") {
        # ind <- c(2, 4, 7, 11, 14, 18, 19, 20, 24, 30, 32, 33)
      # } else if (wave == "w8") {
        # ind <- c(5, 7, 10, 15, 24, 25)
      # }
    # } else if (domain == "IC") {
      # # if (wave == "w2") {
      # #   ind <- c() # no polytomous items
      # # } else
      # if (wave == "w5") {
        # ind <- c(26, 29, 36, 37, 43, 44, 46, 47, 51, 52, 53, 55, 56, 57, 58, 60)
      # } else if (wave == "w9") {
        # ind <- c(3, 4, 10, 11, 14, 21, 22, 23, 25, 27:32)
      # }
    # } else if (domain == "EF") {
      # if (wave == "w7") {
        # ind <- c(1, 2, 3, 4, 5, 6, 7, 11)
      # } else if (wave == "w9") {
        # ind <- 1:3
      # }
    # } else if (domain == "ST") {
      # ind <- 1:5
    # } else if (domain == "LI") {
      # ind <- c(1:12, 14:16)
    # }
  # } else if (SC == "SC2") {
    # # if (domain == "VO") {} # no polytomous items
    # #
    # if (domain == "RE") {
      # if (wave == "w6") {
        # ind <- c(2, 6, 13, 22, 27, 30)
      # } else if (wave == "w9") {
        # ind <- c() # TODO
      # }
    # } else if (domain == "SC") {
      # if (wave == "w1") {
        # ind <- c(5, 7, 9, 17)
      # } else if (wave == "w3") {
        # ind <- c(6, 8, 10)
      # } else if (wave == "w5") {
        # ind <- c(3, 7, 16, 20)
      # }
    # } else if (domain == "MA") {
      # if (wave == "w2") {
        # ind <- c(16)
      # } else if (wave == "w3") {
        # ind <- c(7, 8, 9)
      # } else if (wave == "w4") {
        # ind <- c(10, 12)
      # } else if (wave == "w6") {
        # ind <- c()
      # }
    # }
  # } else if (SC == "SC1") {
    # # if (domain == "CD") {} # no polytomous items
    # # if (domain == "VO") {} # no polytomous items
    # if (domain == "SC") {
      # if (wave == "w6") {
        # ind <- c(2, 4, 12)
      # } else if (wave == "w8") {
        # ind <- c()
      # }
    # } else if (domain == "MA") {
      # if (wave == "w5") {
        # ind <- c(1, 5)
      # } else if (wave == "w7") {
        # ind <- c()
      # } else if (wave == "w9") {
        # ind <- c()
      # }
    # }
  # }
  # ind
# }
