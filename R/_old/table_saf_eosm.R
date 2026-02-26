
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

#import data
sheet <- country
  range <- "A4:G6"
  df <- read_range(saf_eosm_file, sheet, range)  

## prepare data
data_for_table <- df %>% 
  filter(is.na(Score) == FALSE) %>% 
  rename(a = 1)

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.8rem",
                          "white-space"= "wrap"
                          ),
                         align = "center",
                         minWidth = 14,
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name='', 
                                    minWidth = 16, 
                                    align = "left", 
                                    style = list(whiteSpace = "pre",
                                                 "font-size" = "0.8rem"
                                      # , "font-weight" = "bold"
                                      )
                         ) # to preserve whitespace,

  )
)

t
