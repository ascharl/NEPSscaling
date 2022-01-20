# Module: Classification and regression trees Header
# Information about classification and regression trees
##########################################################################################################################################
## UI
##########################################################################################################################################
header_CARTUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
    column(12,
           tags$dl(
             tags$dt("Classification and Regression Trees"),
             tags$dd(
               tags$ul(
                 tags$li("Background variables for plausible values cannot contain missingness, but non-response is pervasive in large scale assessments and surveys"),
                 tags$li("Multiple imputation as an approach to fill in randomly missing data without introducing further bias"),
                 tags$li("Decision trees (e.g., classification and regression trees, CART) can be used to identify a set of plausible responses for the missing data"),
                 tags$li("Variable with missingness is recursively split into subsets; each subset has to be more homogenous than the superset"),
                 tags$li("Splits are made according to a value on a predictor variable (e.g., being female, being older than X years) until a node purity criterion is reached"),
                 tags$li("Prediction for missing values are drawn by following the tree's branches to its nodes and choosing a value from the node following an algorithm"),
                 tags$li("CART is a non-parametric approach and automatically incorporates non-linear relationships in the predicted and predictor variables")
               )
             )
           ),
           tags$img(style="max-width: 500px; width: 40%; height: auto;",
                    src = "binary_tree.png", alt = "Binary decision tree"),
           tags$dl(
             tags$dt("Recommended Reading"),
             tags$dd(
               tags$ol(
                 tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                 tags$li("Aßmann, C., Gaasch, C., Pohl, S., & Carstensen, C. H. (2016). Estimation of plausible values considering partially missing background information: A data augmented MCMC approach. In H.-P. Blossfeld, J. Skopek, J. Maurice, & M. Bayer (Eds.), Methodological Issues of Longitudinal Surveys (pp. 503–521). Springer."),
                 tags$li("Loh, W.-Y. (2011). Classification and regression trees. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 14–23. doi:10.1002/widm.8"),
                 tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696")
               )
             )
           )
    ))
  )
}
