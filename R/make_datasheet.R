#' Create a formatted datasheet based on BRUV/DOV/ROV field metadata
#'
#' @description
#' Builds an `openxlsx2` workbook from standardised field datasheets 
#' (assumes processing using 'Essential-scripts/Datasheet_creator_master.R). The 
#' output workbook includes an "Analysis" sheet (with standardised columns, styling, 
#' filters, data validation, and conditional formatting) and a "Progress" sheet 
#' for tracking analysis progress.
#'
#' @param metadata A data frame containing formatted field metadata. Must include
#'  the minimum required columns for the selected `method`.
#' @param method Character; one of `c("BRUV","DOV")`. Determines the sheet
#'   structure, required columns, and data validations applied.
#' @param campaign.id a character string detailing the campaign id specific to the 
#'  datasheet.
#'
#' @return An `openxlsx2` workbook object that can be written to disk with `openxlsx::wb_save()`.
#'
#' @examples
#' \dontrun{
#' # BRUV
#' wb <- make_datasheet(metadata, method = "BRUV", 
#'   campaign.id = "2023-04_Ningaloo.MP.monitoring_stereoBRUV")
#' wb_save(wb, "BRUV_datasheet.xlsx", overwrite = TRUE)
#' }
#'
#' @import openxlsx2
#' @import dplyr
#' @importFrom tidyr uncount
#' @export

# usethis::use_package("openxlsx2")
# usethis::use_package("dplyr")
# usethis::use_package("tidyr")

# TODO - add named Table for (Analysis and Analysts) to simplify formula creation/data validation? Need to check that this will not ruin old datasheets/metadata creation files


make_datasheet <- function (metadata = NULL, method = c("BRUV","DOV"), campaign.id = NULL) {
  
  
  # Set up ----
  
  warning("Please raise any issues with the technical team or on GitHub (https://github.com/iLab-fish/iLab/issues).")
  
  stopifnot("data.frame expected for input 'metadata'"  = is.data.frame(metadata))
  stopifnot("Method not one of 'BRUV' or 'DOV'" = method %in%c("BRUV","DOV"))
  
  # > Libraries
  # require(openxlsx2)
  # require(dplyr)
  # require(tidyr)
  
  # > openxlsx2 date/date time format
  options("openxlsx2.dateFormat" = "yyyy-mm-dd")
  options("openxlsx2.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
  # options("openxlsx2.maxWidth" = 250) Maximum width allowed in Excel
  
  # Create analysis dataframe ---- 
  if (method == "BRUV") {
    
    BRUV <- TRUE
    
    analysis.data <- metadata%>%
      # Add blank columns, and update values based on footage_useable
      analysis_cols(method = method) # see helper function to update columns and order
      
    
  } else {
    
    BRUV <- FALSE
    
    analysis.data <- metadata%>%
      # Duplicate rows based value in transects
      tidyr::uncount(.data$transects, .remove = TRUE, .id = "transect")%>%
      # Add blank columns, and update values based on footage_useable
      analysis_cols(method = method) # see analysis_cols function to update columns and order
  }
  
  # > remove underscore from column names
  # names(analysis.data) <- names(analysis.data)%>%
    # gsub("_", " ", .)
  names(analysis.data) <- gsub("_", " ", names(analysis.data))
  # Format helpers ----
  # Vectors used to identify columns that have custom formatting/data validation/widths/are hidden etc. 
  
  # > Field/analysis columns and column widths
  if (BRUV) {
    
    # > Field data columns
    field.cols <- names(metadata)[! names(metadata) %in% c("footage_useable","visibility","fov")]
    field.cols <- c("n", gsub("_", " ", field.cols))
    
    # > Analysis columns
    analysis.cols <- names(analysis.data)[!names(analysis.data) %in% field.cols]
    
    # > Column widths
    col.widths <- c(3,	8,	18,	9,	9,	8,	8,	8,	8,	8, 6,
                    5,	5,	7,	7,	8,	8,	8,	8,	7, 7,	10,
                    8,	8,	8,	8,	7,	10,	8,	7,	7,	10,	10)
    names(col.widths) <- names(analysis.data)

    
  } else {
    
    # > Field data columns
    field.cols <- c(names(metadata)[! names(metadata) %in% c("transects","footage_useable","visibility")], "transect")
    field.cols <- c("n", gsub("_", " ", field.cols))
    
    # > Analysis columns
    analysis.cols <- names(analysis.data)[!names(analysis.data) %in% field.cols]
    
    # > Column widths
    col.widths <- c(3,	8,	8, 18,	9,	9,	8,	8,	8,	8,	8, 6, 9,
                    5,	5,	7,	7,	8,	8,	8,	8,	7,	10,
                    8,	8,	8,	7,	7,	10,	10)
    names(col.widths) <- names(analysis.data)
    
  }
  
  # > Dates
  dates <- if (BRUV) {c("maxn complete", "length successful", "habitat successful")
    } else {c("complete","habitat successful")}
  
  # > Text - Not in use
  # text <- c("sample", "lcam", "rcam","field note", "footage useable", "footage note", 
  #           if (BRUV) {
  #             c("fov","visibility", "maxn analyst", "maxn notes", "maxn checker", "checker notes", "length possible", "length analyst", "length notes", "habitat image")
  #             } else {
  #             c("visibility", "analyst", "notes", "checker", "checker notes", "habitat images")
  #             }, 
  #           "habitat analyst", "comments")
  
  
  # > Integers - Not in use
  # integers <- c("n", if (!BRUV) {"transect"},"raw hdd","backup hdd")
  
  # > Conditional formatting (ERROR)
  blank.error <-  c("sample",if (!BRUV) {"transect"},"date time", "latitude","longitude", "depth", "lcam","rcam","raw hdd","backup hdd")
  
  # > Conditional formatting warning
  blank.warning <-  c("site","location","status", if (!BRUV) {"operator"})
  
  # > Hidden Columns 
  hide <- c("site","location","status","dbca zone","dbca sanctuary")
  
  
  # Create workbook ----
  wb <- openxlsx2::wb_workbook(
    creator = Sys.getenv("USERNAME"),
    title = campaign.id,
    company = "DBCA Marine Science Program",
    category = "Monitoring"#,
    # subject = MPID
    # manager = NULL,
    )
  
  # ANALYSIS -----
  
  ## Add/style data ----
  wb <- wb%>%
    
    # > Add analysis worksheet
    openxlsx2::wb_add_worksheet("ANALYSIS", tabColour = "#4969C8") %>%
    
    # > add analysis data
    openxlsx2::wb_add_data(
      x = analysis.data,       # Data to add
      start_row =  1,        # Row start
      start_col = 1,        # column start
      colNames = TRUE,     # Add column names as header
      na.strings = NULL,    # make NAs blank
      with_filter = TRUE,   # adds filter to top row
      name = "Analysis"
    )%>%
    
    # > Header text alignment
    openxlsx2::wb_add_cell_style(
      dims = wb_dims(x = analysis.data, select = "col_names"),
      horizontal = "center", vertical = "center", wrap_text = TRUE
    )%>%
    
    # > Header border
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(x = analysis.data, select = "col_names"),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick") %>%
    
    # > Header font
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(x = analysis.data, select = "col_names"),
      bold = TRUE) %>%
    
    # > Header fill (Field Columns)
    openxlsx2::wb_add_fill( 
      dims = openxlsx2::wb_dims(x = analysis.data, cols = field.cols, select = "col_names"),
      color = openxlsx2::wb_colour("#BE780E")
    )%>%
    
    # > Header fill (Analysis Columns)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = analysis.cols, select = "col_names"),
      color = openxlsx2::wb_colour("#4969C8"))%>%
    
    # > Data border
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(x = analysis.data, select = "data"),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick",
      inner_hgrid = "thin", inner_vgrid = "thin") %>%
    
    # > Table fill (Field Columns)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = field.cols, select = "data"),
      color = openxlsx2::wb_colour("#F7D29B"))%>%
    
    # > Table fill (Field Columns)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = analysis.cols, select = "data"),
      color = openxlsx2::wb_colour("#C1CFF8"))%>%
    
    # > Column widths
    openxlsx2::wb_set_col_widths(cols = 1:ncol(analysis.data), widths = col.widths)%>%
    
    # > Freeze top row
    openxlsx2::wb_freeze_pane(first_row = TRUE)
  
  ## Column formats ----
  # Pre-setting the format of columns in R should assign them to the correct format in excel (https://janmarvin.github.io/openxlsx2/articles/openxlsx2_style_manual.html)
  # This is not perfect with formats ignored when the cell/column is blank, an integer, or text. 
  # This means we need to format date columns
  
  # > Dates
  
  wb <- wb%>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = dates, select = "data"),
      numfmt = "yyyy-mm-dd")
  
  
  # >Text
  # Add if required
  
  # > Integers
  # Add if required
  
  # > Numeric
  # Add if required
  
  # > Latitude and longitude
  wb <- wb%>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = c("latitude", "longitude"), select = "data"),
      numfmt = "0.00")
  
  # > Depth
  wb <- wb%>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = "depth", select = "data"),
      numfmt = "0.0")
  
  
  ## Conditional formatting ----
  
  # > Blank error
  # These columns should not be missing and are vital, a pop up will prevent edditng
  
  # Colour style (error)
  wb$add_dxfs_style(
    name = "error", bg_fill = openxlsx2::wb_color("#EF2222")
  )
  
  # add conditional formatting
  wb <- wb%>%
    openxlsx2::wb_add_conditional_formatting(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = blank.error, select = "data"),
      type = "containsBlanks",
      style = "error")
  
  # > Blank warning
  # These columns should not be missing, but aren't vital. Any attempts to edit will result in a warning requesting confirmation
  
  # Colour style (warning)
  wb$add_dxfs_style(
    name = "warning", bg_fill = openxlsx2::wb_color("#EB6262")
  )
  
  # Add conditional formatting
  wb <- wb%>%
    openxlsx2::wb_add_conditional_formatting(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = blank.warning, select = "data"),
      type = "containsBlanks",
      style = "warning"
    )
  
  # > Footage Un-useable
  # These columns should be grayed out if Footage usable is "No"
  
  # Names of columns from "footage useable" to end
  var <- names(analysis.data)[which(names(analysis.data) %in% "footage useable"):ncol(analysis.data)]
  
  # Colour style (DoNotUse)
  wb$add_dxfs_style(
    name = "DoNotUse", bg_fill = openxlsx2::wb_color("#7f7f7f")
  )
  
  wb <- wb%>%
    openxlsx2::wb_add_conditional_formatting(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = var, select = "data"),
      rule = paste0("$", openxlsx2::wb_dims(x = analysis.data, cols = "footage useable"),' = "No"'),
      style = "DoNotUse"
    )
  
  # > Length Not Possible (BRUV only)
  # These columns should be grayed out if length possible is "No"
  
  if (BRUV) {
    
    # Get column and row numbers
    var <- c("length possible", "length analyst", "length successful","length notes")
    
    wb <- wb%>%
      openxlsx2::wb_add_conditional_formatting(
        dims = openxlsx2::wb_dims(x = analysis.data, cols = var, select = "data"),
        rule = paste0("$",openxlsx2::wb_dims(x = analysis.data, cols = "length possible", rows = 1),' = "No"'),
        style = "DoNotUse"
      )
    }
  
  ## Data validation ----
  # Set up drop downs, format limits, and range limits
  
  # > Sample
  # TODO - add error validation if value changes (loop to assign valuation per cell)
  
  
  # > lat/lon
  # Error if invalid
  
  wb <- wb%>%
    openxlsx2::wb_add_data_validation(
      dims = openxlsx2::wb_dims(x = analysis.data, cols ="latitude", select = "data"),
      type = "decimal",
      operator = "between",
      value = c(-90, 90),    # value limits
      show_input_msg = FALSE,  # No warning pop-up
      show_error_msg = TRUE,    # Error if rules broken
      error_style = "stop",
      error_title = "Invalid Input",
      error = "Latitude must be between -90 and 90"
    )%>%
    openxlsx2::wb_add_data_validation(
      dims = openxlsx2::wb_dims(x = analysis.data, cols ="longitude", select = "data"),
      type = "decimal",
      operator = "between",
      value = c(-180, 180),    # value limits
      show_input_msg = FALSE,  # No warning pop-up
      show_error_msg = TRUE,    # Error if rules broken
      error_style = "stop",
      error_title = "Invalid Input",
      error = "Longitude must be between -180 and 180"
    )
  
  # > HDDs
  # Warn if not an integer
  wb <- wb%>%
    openxlsx2::wb_add_data_validation(
      dims = openxlsx2::wb_dims(x = analysis.data, cols =c("raw hdd", "backup hdd"), select = "data"),
      type = "whole",
      operator = "between",
      value = c(1, 10),    # value limits
      show_input_msg = FALSE,  # No warning pop-up
      show_error_msg = TRUE,    # Error if rules broken
      error_style = "warning",
      error_title = "Unexpected Input",
      error = "Hard drive numbers expected"
    )
  
  # > Analysis complete
  # Warn user if they enter something that isn't a date
  
  if (BRUV) {var <- c("maxn complete", "length successful", "habitat successful")
  } else {var <- c("complete", "habitat successful")}
  
  # Using loop as adding data validation non consecutive ranges not currently working (openxlsx2 version 1.21) 
  for (i in 1:length(var)){
    wb <- wb%>%
      openxlsx2::wb_add_data_validation(
        dims = openxlsx2::wb_dims(x = analysis.data, cols = var[i], select = "data"),
        type = "date",
        operator = "greaterThanOrEqual",
        value = as.Date(Sys.Date()),    # value limits
        show_input_msg = FALSE,  # No warning pop-up
        show_error_msg = TRUE,    # Error if rules broken
        error_style = "warning",
        error_title = "Unexpected Input",
        error = paste("Date after", Sys.Date(), "expected")
      )
  }
  
  # > Analyst
  # error if no intials are selected
  
  if (BRUV) {var <- c("maxn analyst", "maxn checker", "length analyst", "habitat analyst")
  } else {var <- c("analyst", "checker", "habitat analyst")}

  # Using loop as adding data validation for non consecutive ranges not implemented (openxlsx2 version 1.21)
  for (i in 1:length(var)){
    
    wb <- wb%>%
      openxlsx2::wb_add_data_validation(
        dims = openxlsx2::wb_dims(x = analysis.data, cols = var[i], select = "data"),
        type = "list",
        value = "=PROGRESS_RULES!$C$9:$C$19",  # list options (includes not analysed option)
        show_input_msg = FALSE,  # No warning pop-up
        show_error_msg = TRUE,    # Error if rules broken
        error_style = "stop",
        error_title = "Invalid Input",
        error = "Analyst initials expected. Add analyst first/last name to PROGESS_RULES sheet if required."
      )
    
  }
  
  # > visibility
  # Give list of visibility ranges, but allow user to manually enter integers
  wb <- wb%>%
    openxlsx2::wb_add_data_validation(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = "visibility", select = "data"),
      type = "list",
      value = '"0-2,2-4,4+"', # list options
      show_input_msg = FALSE,  # No warning pop-up
      show_error_msg = FALSE    # Error if rules broken
    )
  
  # > Yes/No
  # Allow selection of yes/no and error if alternative value is used
  
  if (BRUV) {var <- c("footage useable", "length possible", "habitat image")} else  {var <- c("footage useable", "habitat images")}
  
  for (i in 1:length(var)) {
    wb <- wb%>%
      openxlsx2::wb_add_data_validation(
        dims = openxlsx2::wb_dims(x = analysis.data, cols = var[i], select = "data"),
        type = "list",
        value = '"Yes,No"',    # list options
        show_input_msg = FALSE,  # No warning pop-up
        show_error_msg = TRUE,    # Error if rules broken
        error_style = "stop",
        error_title = "Invalid Input",
        error = "Select Yes or No from list."
      )
  }
  
  # > FOV (BRUV only)
  # Provide drop down list of possible fov options and error if invalid
  
  if (BRUV){
    
    wb <- wb%>%
      openxlsx2::wb_add_data_validation(
        dims = openxlsx2::wb_dims(x = analysis.data, cols = "fov", select = "data"),
        type = "list",
        value = '"Open, Limited, Face up, Face down"',    # list options
        show_input_msg = FALSE,  # No warning pop-up
        show_error_msg = TRUE,    # Error if rules broken
        error_style = "Warning",
        error_title = "Invalid Input",
        error = "Select option from list."
      )
  }
  
  ## Hide/Protect Sheet ----
  
  ### > hide columns
  
  wb <- wb%>%
    openxlsx2::wb_set_col_widths(
      cols = which(names(analysis.data) %in% hide),
      hidden = TRUE
    )
  
  # > Protect headers and field columns
  # Counter intuitively we unlock all cells, then lock the specific cells we need to secure
  # Excel does not allow sorting with protected cells.
  # See https://learn.microsoft.com/en-us/answers/questions/5393919/protected-excel-sheets-allows-filtering-but-not-so for an awkward workaround
  # For now, we are setting up protection, but do not lock sheet
  
  # exclude protection of raw/backup/field note to allow editing
  var <- field.cols[! field.cols %in% c("raw hdd", "backup hdd", "field note")]

  wb <- wb%>%
    # Unlock all cells
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(x =analysis.data),
      locked = FALSE
    )%>%
    # Lock headers
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(x = analysis.data, select = "col_names"),
      locked = TRUE
    )%>%
    # Lock field data
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(x = analysis.data, cols = var, select = "data"),
      locked = TRUE
    )#%>%
  #   openxlsx2::wb_protect_worksheet(
  #     protect = TRUE,
  #     # TRUE = Restricted; FALSE = allowed 
  #     properties = c(
  #       "selectLockedCells" = FALSE,
  #       "selectUnlockedCells" = FALSE, 
  #       "formatCells" = TRUE,
  #       "formatColumns" = FALSE,
  #       "formatRows" = FALSE,
  #       "insertColumns" = TRUE,
  #       "insertRows" = TRUE, 
  #       "insertHyperlinks" = FALSE,
  #       "deleteColumns" = TRUE,
  #       "deleteRows" = TRUE,
  #       "sort" = FALSE, 
  #       "autoFilter" = FALSE, 
  #       "pivotTables" = TRUE, 
  #       "objects" = FALSE, 
  #       "scenarios" = FALSE)
  #   )
  
  
  # PROGRESS ----
  
  ## Templates ----
  
  # > Campaign ID & n deployments/transects
  campaign <- data.frame("A" = c(campaign.id,
                     paste(nrow(analysis.data),
                           if (BRUV) {"deployments"} else {"transects"}
                           )
                     ))%>%
    dplyr::rename('Campaign ID' = 1)
  
  
  # > Analysis steps blank table 
  progress <- data.frame(
    "A" = if (BRUV) {c("MaxN", "Length", "Checks", "Habitat")
    } else  {c("Analysis", "Checks", "Habitat")
    },
    "Possible" = NA_integer_,
    "Complete" = NA_integer_,
    "Remaining" = NA_integer_,
    "Progress" = NA_integer_)%>%
    dplyr::rename('Anaysis Step' = 1)
  
  class(progress$Progress) <- "percentage"
  
  # > Analyst details blank table
  if (BRUV) {
    analyst.details <- data.frame(
      "A" = rep(NA_character_, 11),
      "Initials" = c(rep(NA_character_, 10),"Not Analysed"),
      "MaxN" = rep(NA_integer_, 11),
      "Length" = rep(NA_integer_, 11),
      "Checks" = rep(NA_integer_, 11),
      "Habitat" = rep(NA_integer_, 11)
    ) %>%
      rename('Analyst (first/last name)' = 1)
  } else {
    analyst.details <- data.frame(
      "A" = rep(NA_character_, 11),
      "Initials" = c(rep(NA_character_, 10),"Not Analysed"),
      "Analysis" = rep(NA_integer_, 11),
      "Check" = rep(NA_integer_, 11),
      "Habitat" = rep(NA_integer_, 11)
    ) %>%
      dplyr::rename('Analyst (first/last name)' = 1)
  }
  
  ## Data/Styles ----
  
  # Create new worksheet
  wb <- wb%>%
    openxlsx2::wb_add_worksheet("PROGRESS_RULES", tabColour = "#06783D")%>%
    
    ### > Data ----
    # > Data (Campaign ID)
    openxlsx2::wb_add_data(
      x = campaign,   # Data to add
      start_row =  1,        # Row start
      start_col = 2,        # column start
      colNames = TRUE,     # Add column names as header
      na.strings = NULL,    # make NAs blank
      name = 'Campaign_id'
    )%>%
    
    # > Data (Progress)
    openxlsx2::wb_add_data(
      x = progress,   # Data to add
      start_row =  1,        # Row start
      start_col = 3,        # column start
      colNames = TRUE,     # Add column names as header
      na.strings = NULL,    # make NAs blank
      name = 'Progress'
    )%>%
    
    # > Data (Project Specific Rules)
    openxlsx2::wb_add_data(
      x = data.frame("A" = seq(1,20), "B" = NA_character_)%>%
        rename('#' = 1, 'Project Specific Rule' = 2),   # Data to add
      start_row =  1,        # Row start
      start_col = 9,        # column start
      colNames = TRUE,     # Add column names as header
      na.strings = NULL,    # make NAs blank
      name = 'Rules'
    )%>%
    
    # > Data (Analysts)
    openxlsx2::wb_add_data(
      x = analyst.details,   # Data to add
      start_row =  8,        # Row start
      start_col = 2,        # column start
      colNames = TRUE,     # Add column names as header
      na.strings = NULL,    # make NAs blank
      name = 'Analysts'
    )
    
    ### > Borders ----
  
  wb <-wb%>%
    # # CampaignID (All)
    # openxlsx2::wb_add_border(
    #   dims = openxlsx2::wb_dims(rows = 1:2, from_col = 2), bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick", inner_hgrid = "thick")%>%
    
    # CampaignID/Progress (Header)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 1, cols = 2:7),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick")%>%
    
    # Campaign ID (body)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 2:3, cols = 2),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick",
      inner_hgrid = "thin", inner_vgrid = "thin")%>%
    
    # Progress (Body)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 2:if (BRUV) {5} else {4}, cols = 3:7),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick",
      inner_hgrid = "thin", inner_vgrid = "thin")%>%
    
    # Rules (Header)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 1, cols = 9:10),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick")%>%
    
    # Rules (Body)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 2:21, cols = 9:10),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick",
      inner_hgrid = "thin", inner_vgrid = "thin")%>%
    
    # Analysts (Header)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 8, cols = 2:if (BRUV) {7} else  {6}),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick")%>%
    
    # Analysts (Body)
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 9:18, cols = 2:if (BRUV) {7} else {6}),
      bottom_border = "thick", top_border = "thick", left_border = "thick", right_border = "thick",
      inner_hgrid = "thin", inner_vgrid = "thin")
    
    
    ### > Fill ----
  
  wb <- wb%>%
    
    # Header (Campaign ID, Progress, Rules)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 1, cols = c(2:7,9:10)),
      color = openxlsx2::wb_colour("#2C9770")
    )%>%
    
    # Header (Analysts)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 8, cols = 2:if (BRUV) {7} else {6}),
      color = openxlsx2::wb_colour("#2C9770")
    )%>%
    
    # CampaignID/Progress (Row 1)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 2, cols = 2:7),
      color = openxlsx2::wb_colour("#C1CFF8")
    )%>%
    
    # CampaignID/Progress (Row 2)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 3, cols = 2:7),
      color = openxlsx2::wb_colour("#819EEF")
    )%>%
    
    # Progress (Row 3)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 4, cols = 3:7),
      color = openxlsx2::wb_colour("#5074DA")
    )%>%
    
    # Analyst (Column 4)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = c(9:18), cols = 4),
      color = openxlsx2::wb_colour("#C1CFF8")
    )%>%
    
    # Analyst (Column 5)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = c(9:18), cols = 5),
      color = openxlsx2::wb_colour("#819EEF")
    )%>%
    
    # Analyst (Column 6)
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = c(9:18), cols = 6),
      color = openxlsx2::wb_colour("#5074DA")
    )
  
  
  # BRUV only
  if (BRUV) {
    wb <- wb%>%
      
      # Progress (Row 4)
      openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 5, cols = 3:7),
      color = openxlsx2::wb_colour("#1F4BC5")
    )%>%
      
      # Analyst (Column 7)
      openxlsx2::wb_add_fill(
        dims = openxlsx2::wb_dims(rows = c(9:18), cols = 7),
        color = openxlsx2::wb_colour("#1F4BC5")
      )
      
  }
  
  
    
    ### > Font ----
  
  wb <- wb%>%  
    
    # > Headers (Campaign ID, Progress, Rules)
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(rows = 1, cols = c(2:7,9:10)),
      bold = TRUE)%>%
    
    # > Header Font (Analysts)
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(rows = 8, cols = 2:if (BRUV) {7} else {6}),
      bold = TRUE)%>%
    
    # > NUll option (Analysts)
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(rows = 19, cols = 3),
      color = openxlsx2::wb_color(hex = "#ffffff")) # set colour as white
  
  ### > Alignment/Widths ----
  
  wb <- wb%>%
    
    # Alignment (All except Project specific rule column) 
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(rows = 1:21, cols = 2:9),
      horizontal = "center", vertical = "center")%>%
    
    # Column widths (All except Project specific rule column)
    openxlsx2::wb_set_col_widths(cols = 2:9,widths = "auto")%>%
    
    # Column widths (All except Project specific rule column)
    openxlsx2::wb_set_col_widths(cols = 10,widths = 55)
  
  ### > Progress Bars ----
  wb <- wb%>%
    openxlsx2::wb_add_conditional_formatting(
      dims = if (BRUV) {openxlsx2::wb_dims(rows = 2:5, cols = 7)
      } else {openxlsx2::wb_dims(rows = 2:4, cols = 7)},
      type = "dataBar",
      rule = c(0, 1),
      style = c("#a6a6a6", "#2C9770"),
      params = list(gradient = FALSE)
    )

  ## Formulas (Progress) ----
  # Formatting the analysis data as a 'data table' may simplify formula creation and allow us to call the specific columns rather than using cell ranges
  
  wb <- wb%>%
    
    # > MaxN/DOV Analysis Possible
    openxlsx2::wb_add_formula(
      x =   paste0(nrow(analysis.data),'-COUNTIF(ANALYSIS!',
                 openxlsx2::wb_dims(x = analysis.data, cols = "footage useable", select = "data"),
                 ', "No")'),
      dims = openxlsx2::wb_dims(rows = 2, cols =4)
    )%>%
    
    # > MaxN/DOV Analysis complete
    openxlsx2::wb_add_formula(
      x = if (BRUV) {
        paste0('COUNTA(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "maxn complete", select = "data"),
               ')')
        } else {
          paste0('COUNTA(ANALYSIS!',
                 openxlsx2::wb_dims(x = analysis.data, cols = "complete", select = "data"),
                 ')') 
        },
      dims = openxlsx2::wb_dims(rows = 2, cols =5)
    )%>%
    
    # > Lengths/Checks Possible
    openxlsx2::wb_add_formula(
      x = if (BRUV) {
        paste0('D2-COUNTIF(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "length possible", select = "data"),
               ', "No")')
      } else {
        "D2" # Matches number analysis possible in DOVs
      },
      dims = openxlsx2::wb_dims(rows = 3, cols =4)
    )%>%
    
    # > Length/DOV Checks complete
    openxlsx2::wb_add_formula(
      x = if (BRUV) {
        paste0('COUNTA(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "length successful", select = "data"),
               ')')
      } else { # Need to remove counts of "Not Analysed"
        paste0('COUNTA(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "checker", select = "data"),
               ') - COUNTIF(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "checker", select = "data"),
               ', "Not Analysed")'
        ) 
      },
      dims = openxlsx2::wb_dims(rows = 3, cols =5)
    )%>%
    
    # > Habitat Possible
    openxlsx2::wb_add_formula(
      x = "D2",
      dims = if (BRUV) { openxlsx2::wb_dims(rows = 5, cols = 4)
        } else {openxlsx2::wb_dims(rows = 4, cols = 4)}
    )%>%
    
    # > Habitat Complete
    openxlsx2::wb_add_formula(
      x = paste0('COUNTA(ANALYSIS!',
               openxlsx2::wb_dims(x = analysis.data, cols = "habitat successful", select = "data"),
               ')'),
      dims = if (BRUV) { openxlsx2::wb_dims(rows = 5, cols = 5)
      } else {openxlsx2::wb_dims(rows = 4, cols = 5)}
    )%>%
    
    # > Remaining (Analysis/Length/Checks/Habitat)
    openxlsx2::wb_add_formula(
      x = "$D2-$E2",
      dims = if (BRUV) {openxlsx2::wb_dims(rows = 2:5, cols = 6)
      } else {openxlsx2::wb_dims(rows = 2:4, cols = 6)},
      shared = TRUE # FALSE should still work
    )%>%
    
    # > Progress (Analysis/Length/Checks/Habitat)
    openxlsx2::wb_add_formula(
      x = "$E2/$D2",
      dims = if (BRUV) {openxlsx2::wb_dims(rows = 2:5, cols = 7)
      } else {openxlsx2::wb_dims(rows = 2:4, cols = 7)},
      shared = TRUE
    )
    
  # > Checks Complete (BRUV only)
  if (BRUV){
    
    wb <- wb%>%
      # Checks Required
      openxlsx2::wb_add_formula(
        x = "D2",
        dims = openxlsx2::wb_dims(rows = 4, cols = 4)
      )%>%
      
      # Checks Complete
      openxlsx2::wb_add_formula(
        # Need to remove counts of "Not Analysed"
        x = paste0('COUNTA(ANALYSIS!',
                   openxlsx2::wb_dims(x = analysis.data, cols = "maxn checker", select = "data"),
                   ') - COUNTIF(ANALYSIS!',
                   openxlsx2::wb_dims(x = analysis.data, cols = "maxn checker", select = "data"),
                   ', "Not Analysed")'),
        dims = openxlsx2::wb_dims(rows = 4, cols = 5)
      )
    
  }
    
  
  ## Formulas (Analyst) ----
  
  # > Analyst initials
  wb <- wb%>%
    openxlsx2::wb_add_formula(
      x = 'IF(ISBLANK($B9),"",LEFT($B9,1) & MID($B9,FIND(" ",$B9)+1,1))',
      dims = openxlsx2::wb_dims(rows = 9:18, cols = 3),
      shared = TRUE
    )
    
  # > Counts per analyst
  # Using loop as adding shared formulas that call ranges in other sheets does not appear to work (openxlsx2 version 1.21)
  
  for (i in 9:18) {
    wb <- wb%>%

      # > MaxN/Analysis per Analyst
      openxlsx2::wb_add_formula(
        x = paste0('IF(ISBLANK($B',i,'),"",COUNTIF(ANALYSIS!',
                   make_absolute( # Makes column and row info absolute (not changing between cells)
                     openxlsx2::wb_dims(x = analysis.data, cols = if (BRUV) {"maxn analyst"
                     } else {"analyst"}, select = "data")
                   ),
                     ",$C",i ,"))"),
        dims = openxlsx2::wb_dims(rows = i, cols = 4)
      )

    if (BRUV) {

      wb <- wb%>%
        # > Lengths per Analyst
        openxlsx2::wb_add_formula(
          x = paste0('IF(ISBLANK($B',i,'),"",COUNTIF(ANALYSIS!',
                     make_absolute( # Makes column and row info absolute (not changing between cells)
                       openxlsx2::wb_dims(x = analysis.data, cols = "length analyst", select = "data")
                     ),
                     ",$C",i ,"))"),
          dims = openxlsx2::wb_dims(rows = i, cols = 5)
        )

    }

    # > Checks per Analyst
    wb <- wb%>%
      openxlsx2::wb_add_formula(
        x = paste0('IF(ISBLANK($B',i,'),"",COUNTIF(ANALYSIS!',
                   make_absolute( # Makes column and row info absolute (not changing between cells)
                     openxlsx2::wb_dims(x = analysis.data, cols = if (BRUV) {"maxn checker"
                     } else {"checker"}, select = "data")
                   ),
                   ",$C",i ,"))"),
        dims = openxlsx2::wb_dims(rows = i, cols = if (BRUV) {6} else {5})
      )%>%

      # > Habitat per analyst
      openxlsx2::wb_add_formula(
        x = paste0('IF(ISBLANK($B',i,'),"",COUNTIF(ANALYSIS!',
                   make_absolute( # Makes column and row info absolute (not changing between cells)
                     openxlsx2::wb_dims(x = analysis.data, cols = "habitat analyst", select = "data")
                   ),
                   ",$C",i ,"))"),
        dims = openxlsx2::wb_dims(rows = i, cols = if (BRUV) {7} else {6})
      )
  }
    
  # Attempted shared formula for MaxN/Analysis per Analyst and simple test formula
  # shared forumlas seem to break when calling another sheet
  # It may be possible by calling specific columns from data tables
  # wb <- wb%>%
  #   openxlsx2::wb_add_formula(
  #     x = 'SUM(ANALYSIS!$A$2:$A$4)',
  #     dims = openxlsx2::wb_dims(rows = 1:5, cols = 1),
  #     shared = TRUE
  #   )
  # 
  # openxlsx2::wb_add_formula(
  #   x = paste0('IF(ISBLANK($B9),"",COUNTIF(ANALYSIS!',
  #              make_absolute( # Makes column and row info absolute (not changing between cells)
  #                openxlsx2::wb_dims(x = analysis.data, cols = if (BRUV) {"maxn analyst"
  #                } else {"analyst"}, select = "data")
  #              ),
  #              ",$C9))"),
  #   dims = openxlsx2::wb_dims(rows = 9:18, cols = 4),
  #   shared = TRUE
  # )

  
  ## Protect Sheet ----
  
  wb <- wb%>%
    
    # > Hide formulas
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(rows = 2:19, cols = 3:7),
      hidden = TRUE)%>%
    
    # > Unlock Campaign ID
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(cols = 2, rows =2),
      locked = FALSE
    )%>%
    
    # > Unlock Analyst Names
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(cols = 2, rows =9:18),
      locked = FALSE
    )%>%
    
    # > Unlock Rules
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(cols = 10, rows =2:21),
      locked = FALSE
    )%>%
    
    # > Protect worksheet
    openxlsx2::wb_protect_worksheet(
      protect = TRUE,
      # TRUE = Restricted; FALSE = allowed 
      properties = c(
        "selectLockedCells" = FALSE,
        "selectUnlockedCells" = FALSE, 
        "formatCells" = TRUE,
        "formatColumns" = TRUE,
        "formatRows" = TRUE,
        "insertColumns" = TRUE,
        "insertRows" = TRUE, 
        "insertHyperlinks" = TRUE,
        "deleteColumns" = TRUE,
        "deleteRows" = TRUE,
        "sort" = TRUE, 
        "autoFilter" = TRUE, 
        "pivotTables" = TRUE, 
        "objects" = FALSE, 
        "scenarios" = FALSE)
    )
  
  # > Reorder sheets
  # Reordering causes issues with cell locking. If we need to re-order we could try running openxlsx2::wb_protect_worksheet() after re-ordering. Or create the progress sheet first.
  # wb <- wb%>%
    # openxlsx2::wb_set_order(c(2,1))
  
  # Return workbook
  return(wb)
}


#' Create analysis dataframe
#' 
#' @description 
#' Formats standardised field metadata to output a data frame containing the columns
#' required for video analysis. Required for `make_datasheet()`.
#' 
#' @param x A data sheet containing formatted field metadata
#' @param method One of "BRUV" or "DOV"

# Add/reorder columns required in data analysis
analysis_cols <- function(x, method = NULL) {
  
  stopifnot("data.frame expected for input 'x'"  = is.data.frame(x))
  stopifnot("Method not one of 'BRUV' or 'DOV'" = method %in%c("BRUV","DOV"))
  
  # Validate method
  valid <- c("BRUV", "DOV")
  if (is.null(method) || !(method %in% valid)) {
    stop(sprintf('method "%s" does not match BRUV, DOV', as.character(method)), call. = FALSE)
  }
  
  # Base columns added for all methods
  base <- x %>%
    dplyr::mutate(
      n = dplyr::row_number(),
      footage_note = NA_character_,
      habitat_analyst = NA_character_,
      habitat_successful = NA_integer_,
      comments = NA_character_
    )
  
  # Method-specific columns to add
  method_cols <- switch(
    method,
    BRUV = list(
      maxn_analyst     = NA_character_,
      maxn_complete    = NA_integer_,
      maxn_notes       = NA_character_,
      maxn_checker     = NA_character_,
      checker_notes    = NA_character_,
      length_possible  = NA_character_,
      length_analyst   = NA_character_,
      length_successful= NA_integer_,
      length_notes     = NA_character_,
      habitat_image    = NA_character_
    ),
    DOV = list(
      analyst        = NA_character_,
      complete       = NA_integer_,
      notes          = NA_character_,
      checker        = NA_character_,
      checker_notes  = NA_character_,
      habitat_images = NA_character_
    )
  )
  
  if (!is.null(method_cols)) {
    # add missing columns
    base <- dplyr::mutate(base, !!!method_cols)
  }
  
  # Dynamically define required column order
  cols <- c(
    "n",
    "sample",
    if (method == "DOV") "transect",
    "date_time",
    "latitude",
    "longitude",
    "site",
    "location",
    "status",
    "dbca_zone",
    "dbca_sanctuary",
    "depth",
    if (method == "DOV") "operator",
    "lcam",
    "rcam",
    "raw_hdd",
    "backup_hdd",
    "field_note",
    "footage_useable",
    "footage_note",
    "visibility",
    if (method == "BRUV") c(
      "fov",
      "maxn_analyst",
      "maxn_complete",
      "maxn_notes",
      "maxn_checker",
      "checker_notes",
      "length_possible",
      "length_analyst",
      "length_successful",
      "length_notes",
      "habitat_image"
    ),
    if (method == "DOV") c(
      "analyst",
      "complete",
      "notes",
      "checker",
      "checker_notes",
      "habitat_images"
    ),
    "habitat_analyst",
    "habitat_successful",
    "comments"
  )
  
  
  # Update columns based on values in footage_useable
  out <- base%>%
    dplyr::mutate(dplyr::across(
      
      # Set column values to No
      if (method == "BRUV") {c("length_possible", "habitat_image")} else {"habitat_images"},
      ~dplyr::case_when(footage_useable == "No" ~ "No", TRUE ~ .)),
      
      # Set column values to "Not Analysed"
      dplyr::across(
        if (method == "BRUV") {c("maxn_analyst", "length_analyst", "habitat_analyst")} else {c("analyst", "habitat_analyst")},
        ~dplyr::case_when(footage_useable == "No" ~ "Not Analysed",TRUE ~ .))
      )
  
  # Reorder and error if columns are missing
  out %>%
    dplyr::select(dplyr::all_of(cols))
    
}

#' Convert excel range to absolute range
#' 
#' @description 
#' Converts an excel range (e.g. A1:A5) into an absolute range (e.g. $A$1:$A$5)
#' This function operates using a simple regular expression and works for typical
#' cell references, including ranges and multi-area references.
#' 
#' @param range A character vector containing Excel-style cell references
#'   or ranges (e.g., `"A1"`, `"A1:B5"`, `"Sheet1!A1:B1"`, `c("A1:A5","C1:C5")`).

make_absolute <- function(range) {
  stopifnot(is.character(range))
  # Add $ before and after column letters
  gsub("([A-Z]+)([0-9]+)", "\\$\\1\\$\\2", range)
}
