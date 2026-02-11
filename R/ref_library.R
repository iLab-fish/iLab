#' Shiny application displaying iLab-Fish reference library images
#'
#' @description
#' Starts a the iLab Fish reference library application, which allows users to
#' filter and display PNG/JPEG images stored in Asset Fish where the ParkID , 
#' genus, and species can be extracted from the filepath.
#' 
#' @return A **Shiny Application**
#' 
#' @import shiny
#' @import stringi
#' @import tools
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select left_join join_by
#' @importFrom htmltools htmlEscape
#' 
#' @export

# Add required packages to DESCRIPTION
# usethis::use_package("shiny")
# usethis::use_package( "tools")
# usethis::use_package("dplyr")
# usethis::use_package("stringi")
# usethis::use_package("janitor")
# usethis::use_package("magrittr")
# usethis::use_package("dplyr")
# usethis::use_package("htmltools")

# TODO - investigate warning "in shiny::removeResourcePath(alias) : Resource imgdir not found
# TODO - add stats on n per transect/deplyment
# TODO - add simplify UI/add dark mode
# TODO - improve pop-up window sizing
# TODO - Create standalone repo?
# TODO - create executable (i.e., can be run outside R)?


ref_library <- function() {
  
  require(shiny)
  require(magrittr)
  
  # UI ----
  ui <- fluidPage(
    
    # tags for pan zoom
    tags$head(
      # CSS first
      tags$style(HTML("
      .grid { display: flex; flex-wrap: wrap; gap: 10px; }
      .card { border: 1px solid #eee; padding: 8px; border-radius: 6px; width: 240px; cursor: pointer; }
      .thumb { width: 100%; height: auto; object-fit: cover; border-radius: 4px; }
      .caption { font-size: 12px; color: #555; margin-top: 6px; }
      .muted { color: #888; }

      .modal-dialog { max-width: 95% !important; }
      #zoom-container { width: 100%; height: 80vh; overflow: hidden; border: 1px solid #ccc; }
      #zoom-img { width: 100%; height: auto; cursor: move; }
    ")),
      
      # JS libraries for zoom/pan
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-mousewheel/3.1.13/jquery.mousewheel.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery.panzoom/3.2.3/jquery.panzoom.min.js")
    ),
    
    # tags for basic UI
  #   tags$head(
  #     tags$style(HTML("
  #   .grid { display: flex; flex-wrap: wrap; gap: 10px; }
  #   .card { border: 1px solid #eee; padding: 8px; border-radius: 6px; width: 240px; cursor: pointer; }
  #   .thumb { width: 100%; height: auto; object-fit: cover; border-radius: 4px; }
  #   .caption { font-size: 12px; color: #555; margin-top: 6px; }
  #   .muted { color: #888; }
  # 
  #   /* Modal image sizing */
  #   .modal-lg-img {
  #     max-width: 90vw;
  #     max-height: 85vh;
  #     width: auto;
  #     height: auto;
  #     display: block;
  #     margin: 0 auto;
  #     border-radius: 6px;
  #   }
  #   .modal-title-small {
  #     font-size: 14px;
  #     color: #666;
  #     margin-top: 8px;
  #     word-break: break-all;
  #   }
  # "))
  #   ),
    titlePanel("iLab Fish Reference Images"),
    sidebarLayout(
      sidebarPanel(
        # No path input and no Scan button — indexing happens automatically.
        # checkboxInput("recursive", "Include subfolders", value = TRUE),
        # tags$hr(),
        selectizeInput(
          "parkid", "Park ID",
          choices = c("All" = ""), selected = "", multiple = FALSE,
          options = list(placeholder = "All")
        ),
        selectizeInput(
          "family", "Family",
          choices = c("All" = ""), selected = "", multiple = FALSE,
          options = list(placeholder = "All")
        ),
        selectizeInput(
          "genus", "Genus",
          choices = c("All" = ""), selected = "", multiple = FALSE,
          options = list(placeholder = "All")
        ),
        selectizeInput(
          "species", "Species",
          choices = c("All" = ""), selected = "", multiple = FALSE,
          options = list(placeholder = "All")
        ),
        tags$hr(),
        div(class = "muted", "Only .png, .jpg, .jpeg files are included (case-insensitive).")
      ),
      mainPanel(
        uiOutput("summary"),
        uiOutput("images")
      )
    )
  )
  
  # Server ----
  server <- function(input, output, session) {
    
    # Set base directory
    get_image_dir <- reactiveVal(NULL)
    
    # Run once on app start
    observeEvent(TRUE, {
      dir <- iLab::get_dir("Asset_Fish", "Science Communication/Cool Clips")
      
      # Normalize and validate
      dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
      if (!nzchar(dir) || !dir.exists(dir)) {
        showNotification(
          "Image folder path is invalid or does not exist. Please update get_dir().",
          type = "error", duration = 8
        )
        get_image_dir(NULL)
      } else {
        get_image_dir(dir)
      }
    }, once = TRUE)
    
    # Reactive state
    rv <- reactiveValues(
      df = data.frame(),
      alias = NULL
    )
    
    # ---------------- Helpers ----------------
    
    # Get ParkID from the folder two levels up from the absolute file path
    get_parkid_from_dirs <- function(file_path) {
      fp <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
      parent1 <- dirname(fp)
      parent2 <- dirname(parent1)
      if (identical(parent2, parent1) || identical(parent2, fp)) {
        return(NA_character_)
      }
      basename(parent2)
    }
    
    # Genus/Species
    # Prepare genus species search pairs from CAAB file 
    prep_caab <- function(CAAB_raw) {
      # 1) Keep only needed columns and clean
      caab <- unique(CAAB_raw[, c("genus", "species")])
      caab$genus   <- trimws(as.character(caab$genus))
      caab$species <- trimws(as.character(caab$species))
      caab <- caab[nzchar(caab$genus) & nzchar(caab$species) &
                     !is.na(caab$genus) & !is.na(caab$species), , drop = FALSE]
      if (!nrow(caab)) stop("CAAB has no valid genus/species rows.")
      
      # 2) Lowercase canonical forms for fast, case-insensitive matching
      caab$gen_lc <- tolower(caab$genus)
      caab$sp_lc  <- tolower(caab$species)
      
      # 3) Precompute binomial tokens for two normalizations:
      #    - sep form: both names separated by ONE space, padded with spaces at ends
      #    - cat form: both names concatenated (no separators)
      #    (We'll normalize stems the same way for fixed-substring detection)
      caab$bin_sep_token <- paste0(" ", caab$gen_lc, " ", caab$sp_lc, " ")
      caab$bin_cat_token <- paste0(caab$gen_lc, caab$sp_lc)
      
      # 4) Precompute unique genus lists and genus tokens for fallback matching
      genus_u <- sort(unique(caab$gen_lc))
      genus_sep_token <- paste0(" ", genus_u, " ")
      genus_cat_token <- genus_u
      
      # 5) Build an index: genus -> row indices (to restrict species search if genus found)
      genus_index <- split(seq_len(nrow(caab)), caab$gen_lc)
      
      # 6) Return a compact object with everything we need at match time
      list(
        caab          = caab,
        genus_u       = genus_u,
        genus_sep_tok = genus_sep_token,
        genus_cat_tok = genus_cat_token,
        genus_index   = genus_index
      )
    }
    
    # Vectorized matcher: stems -> (genus, species)
    # Behavior:
    #   - Matches ONLY "Genus species" pairs (binomials) anywhere in the path
    #     allowing separators (space, underscore, hyphen) OR concatenated form.
    #   - If multiple binomials match => NA/NA (ambiguous).
    #   - If no binomial matches => NA/NA.
    #   - If exactly one binomial matches => return its (genus, species).
    #
    # Arguments:
    #   stems: character vector (file names or full paths; extension doesn't matter)
    #   caabp: result of prep_caab()
    #   require_unique: if TRUE (default), multiple binomial matches => NA/NA
    #
    # Returns: data.frame(genus, species), length = length(stems)
    match_stems_caab <- function(stems, caabp, require_unique = TRUE) {
      stopifnot(is.list(caabp), all(c("caab","genus_u","genus_sep_tok","genus_cat_tok","genus_index") %in% names(caabp)))
      caab  <- caabp$caab
      
      # Normalize stems into two parallel forms (vectorized)
      # 1) Lowercase everything
      stems_lc <- tolower(as.character(stems))
      
      # 2) "Separated" form:
      #    - Replace any non-alphabetic values with a single space
      #    - Trim extra spaces
      #    - Pad with a leading and trailing space so we can detect " tokens " cleanly
      #    Example: "PS2-3_Pseudocoris cooperi" -> " ps pseudocoris cooperi "
      stems_sep <- paste0(
        " ",
        stringi::stri_trim_both(stringi::stri_replace_all_regex(stems_lc, "[^a-z]+", " ")),
        " "
      )
      
      n <- length(stems_lc)
      out_gen <- character(n)
      out_sp  <- character(n)
      
      # Helper: choose a deterministic "best" when require_unique = FALSE
      # (not used if require_unique = TRUE and >1 hits)
      pick_best_idx <- function(idx_vec) {
        if (length(idx_vec) == 1L) return(idx_vec)
        # bin_cat <- caab$bin_cat_token[idx_vec]
        bin_sep <- paste(caab$genus[idx_vec], caab$species[idx_vec])
        idx_vec[order(-nchar(bin_cat), bin_sep)][1]
      }
      
      for (i in seq_len(n)) {
        s_sep <- stems_sep[i]
        
        # 1) Try BINOMIAL matches (Genus + species) only (strict)
        #    We detect across ALL CAAB rows with fixed-string search.
        #    A hit occurs if either sep-token or cat-token is present.
        
        hits_sep <- stringi::stri_detect_fixed(s_sep, caab$bin_sep_token)  # sep-based (e.g., Coris auricualris)
        hits_cat <- stringi::stri_detect_fixed(s_sep, caab$bin_cat_token)  # concatenated (e.g., Corisauricualris)
        hits_idx <- which(hits_sep| hits_cat)
        
        if (!length(hits_idx)) {
          # No binomial found anywhere -> strict rule: NA/NA
          out_gen[i] <- NA_character_
          out_sp[i]  <- NA_character_
          next
        }
        
        if (require_unique && length(hits_idx) != 1L) {
          # More than one binomial found -> ambiguous -> NA/NA
          out_gen[i] <- NA_character_
          out_sp[i]  <- NA_character_
          next
        }
        
        # Unique binomial OR we’re allowed to pick deterministically
        best <- if (length(hits_idx) == 1L) hits_idx else pick_best_idx(hits_idx)
        out_gen[i] <- caab$genus[best]
        out_sp[i]  <- caab$species[best]
      }
      
      data.frame(genus = out_gen, species = out_sp, path = stems, stringsAsFactors = FALSE)
    }
    
    # # Extract from from 4th underscore-separated segment in filename stem.
    # # Inside that segment we expect: "... <Genus> <Species>"
    # get_genus_species <- function(stem) {
    #   parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
    #   if (length(parts) < 4) return(c(NA_character_, NA_character_))
    #   seg <- trimws(parts[4])
    #   if (nchar(seg) == 0) return(c(NA_character_, NA_character_))
    #   toks <- strsplit(seg, "\\s+")[[1]]
    #   toks <- toks[nzchar(toks)]
    #   if (length(toks) >= 3) {
    #     c(toks[2], toks[3])
    #   } else if (length(toks) == 2) {
    #     c(toks[1], toks[2])
    #   } else if (length(toks) > 3) {
    #     n <- length(toks)
    #     c(toks[n-1], toks[n])
    #   } else {
    #     c(NA_character_, NA_character_)
    #   }
    # }
    
    # Build the data.frame from the directory
    build_df <- function(dir) {
      
      # Only png/jpg/jpeg (case-insensitive)
      rel_files <- list.files(
        dir,
        pattern = "\\.(png|jpeg|jpg)$",
        recursive = TRUE,
        full.names = FALSE,
        ignore.case = TRUE
      )
      if (length(rel_files) == 0) {
        return(data.frame())
      }
      
      # CAAB list
      CAAB <- read.delim(file.path(iLab::get_dir("ilab_fish"), "!Essential_Files/CAAB_Species_Files/WA_CAAB.txt"), sep="\t")
      CAAB  <- janitor::clean_names(CAAB)
      names(CAAB)[1] <- "family"
      names(CAAB)[4] <- "caab"
      
      # Absolute paths for ParkID extraction
      abs_files <- file.path(dir, rel_files)
      
      stems   <- file_path_sans_ext(basename(rel_files))
      parkid  <- vapply(abs_files, get_parkid_from_dirs, FUN.VALUE = character(1))
      # gs      <- t(vapply(stems, get_genus_species, caab = prep_gs_caab(), FUN.VALUE = c("g" = "", "s" = "")))
      # genus   <- gs[, 1]
      # species <- gs[, 2]
      gs <- match_stems_caab(stems, prep_caab(CAAB))
      genus <- gs$genus
      species <- gs$species
      
      df <- data.frame(
        rel_path = rel_files,
        web_path = file.path("PLACEHOLDER_ALIAS", rel_files),  # patched after addResourcePath
        filename = basename(rel_files),
        stem     = stems,
        parkid   = parkid,
        genus    = genus,
        species  = species,
        stringsAsFactors = FALSE
      )%>%
        dplyr::filter(genus %in% CAAB$genus,
               species %in% CAAB$species)%>%
        dplyr::left_join(CAAB%>%select(family,genus,species,caab), by = dplyr::join_by(genus, species))
    }
    
    # Index images and update UI
    index_images <- function() {
      dir <- get_image_dir()
      if (is.null(dir)) {
        rv$df <- data.frame()
        return()
      }
      
      # (Re)register static resource alias for serving local files
      alias <- "imgdir"
      try(shiny::removeResourcePath(alias), silent = TRUE)
      shiny::addResourcePath(alias, dir)
      rv$alias <- alias
      
      df <- build_df(dir)
      
      df <- df[complete.cases(df), ]
      
      if (nrow(df) == 0) {
        showNotification("No PNG/JPEG images found in this folder (and subfolders if selected).",
                         type = "warning", duration = 6)
        rv$df <- data.frame()
      } else {
        df$web_path <- file.path(paste0("/", alias), df$rel_path)
        rv$df <- df
      }
      
      # Populate dropdown choices
      park_choices   <- sort(unique(na.omit(rv$df$parkid)))
      family_choices  <- sort(unique(na.omit(rv$df$family)))
      genus_choices  <- sort(unique(na.omit(rv$df$genus)))
      species_choices<- sort(unique(na.omit(rv$df$species)))
      
      updateSelectizeInput(
        session, "parkid",
        choices = c("All" = "", park_choices),
        selected = if (nzchar(isolate(input$parkid)) && isolate(input$parkid) %in% park_choices) isolate(input$parkid) else ""
      )
      updateSelectizeInput(
        session, "family",
        choices = c("All" = "", family_choices),
        selected = if (nzchar(isolate(input$family)) && isolate(input$family) %in% family_choices) isolate(input$family) else ""
      )
      updateSelectizeInput(
        session, "genus",
        choices = c("All" = "", genus_choices),
        selected = if (nzchar(isolate(input$genus)) && isolate(input$genus) %in% genus_choices) isolate(input$genus) else ""
      )
      updateSelectizeInput(
        session, "species",
        choices = c("All" = "", species_choices),
        selected = if (nzchar(isolate(input$species)) && isolate(input$species) %in% species_choices) isolate(input$species) else ""
      )
    }
    
    # Auto-index when the directory is set, and re-index when recursion toggle changes
    observeEvent(get_image_dir(), {
      index_images()
    }, ignoreInit = FALSE)
    
    observeEvent(input$recursive, {
      index_images()
    }, ignoreInit = TRUE)
    
    # ---------------- Reactive filtering & outputs ----------------
    
    # Update drop down filtering depending other options
    observe({
      df <- rv$df
      if (nrow(df) == 0) return()
      
      # --- Helper: safe update that preserves selection if still valid ---
      safe_update <- function(inputId, choices, current) {
        choices <- sort(unique(na.omit(choices)))
        updateSelectizeInput(
          session, inputId,
          choices = c("All" = "", choices),
          selected = if (nzchar(current) && current %in% choices) current else ""
        )
      }
      
      # PARKID choices (depends on family, genus, species)
      d_wo_park <- df
      if (nzchar(input$family))  d_wo_park <- d_wo_park[d_wo_park$family  == input$family,  , drop = FALSE]  # family (optional)
      if (nzchar(input$genus))   d_wo_park <- d_wo_park[d_wo_park$genus   == input$genus,   , drop = FALSE]
      if (nzchar(input$species)) d_wo_park <- d_wo_park[d_wo_park$species == input$species, , drop = FALSE]
      safe_update("parkid", d_wo_park$parkid, input$parkid)
      
      # FAMILY choices (depends on parkid, genus, species)
      d_wo_family <- df
      if (nzchar(input$parkid))  d_wo_family <- d_wo_family[d_wo_family$parkid  == input$parkid,  , drop = FALSE]
      if (nzchar(input$genus))   d_wo_family <- d_wo_family[d_wo_family$genus   == input$genus,   , drop = FALSE]
      if (nzchar(input$species)) d_wo_family <- d_wo_family[d_wo_family$species == input$species, , drop = FALSE]
      if ("family" %in% names(df)) {
        safe_update("family", d_wo_family$family, input$family)
      }
      
      # GENUS choices (depends on parkid, family, species)
      d_wo_genus <- df
      if (nzchar(input$parkid))  d_wo_genus <- d_wo_genus[d_wo_genus$parkid  == input$parkid,  , drop = FALSE]
      if (nzchar(input$family) && "family" %in% names(df))  d_wo_genus <- d_wo_genus[d_wo_genus$family  == input$family,  , drop = FALSE]
      if (nzchar(input$species)) d_wo_genus <- d_wo_genus[d_wo_genus$species == input$species, , drop = FALSE]
      safe_update("genus", d_wo_genus$genus, input$genus)
      
      # SPECIES choices (depends on parkid, family, genus)
      d_wo_species <- df
      if (nzchar(input$parkid))  d_wo_species <- d_wo_species[d_wo_species$parkid  == input$parkid,  , drop = FALSE]
      if (nzchar(input$family) && "family" %in% names(df))  d_wo_species <- d_wo_species[d_wo_species$family  == input$family,  , drop = FALSE]
      if (nzchar(input$genus))   d_wo_species <- d_wo_species[d_wo_species$genus   == input$genus,   , drop = FALSE]
      safe_update("species", d_wo_species$species, input$species)
    })
    
    
    # Filtered list of file paths
    filtered <- reactive({
      df <- rv$df
      if (nrow(df) == 0) return(df)
      
      d <- df
      
      # Apply filters
      if (nzchar(input$parkid))  d <- d[d$parkid  == input$parkid,  , drop = FALSE]
      if ("family" %in% names(d) && nzchar(input$family)) 
        d <- d[d$family == input$family, , drop = FALSE]
      if (nzchar(input$genus))   d <- d[d$genus   == input$genus,   , drop = FALSE]
      if (nzchar(input$species)) d <- d[d$species == input$species, , drop = FALSE]
      
      if (nrow(d) == 0) return(d)
      
      # Ensure 'family' column exists to avoid errors in order()
      if (!"family" %in% names(d)) d$family <- NA_character_
      
      # Order by family, then genus, then species, then filename.
      # Puts NAs at the end for each key.
      o <- order(
        is.na(d$family),  d$family,
        is.na(d$genus),   d$genus,
        is.na(d$species), d$species,
        d$filename,
        na.last = TRUE
      )
      
      d[o, , drop = FALSE]
    })
    # filtered <- reactive({
    #   df <- rv$df
    #   if (nrow(df) == 0) return(df)
    #   
    #   d <- df
    #   if (nzchar(input$parkid))  d <- d[d$parkid  == input$parkid, , drop = FALSE]
    #   if (nzchar(input$family))   d <- d[d$family   == input$family,  , drop = FALSE]
    #   if (nzchar(input$genus))   d <- d[d$genus   == input$genus,  , drop = FALSE]
    #   if (nzchar(input$species)) d <- d[d$species == input$species,, drop = FALSE]
    #   d
    # })
    
    # Output
    output$summary <- renderUI({
      df <- rv$df
      d  <- filtered()
      if (nrow(df) == 0) return(NULL)
      
      div(
        h4("Results"),
        p(sprintf("Showing %d of %d indexed image(s).", nrow(d), nrow(df))),
        if (nzchar(input$parkid) || nzchar(input$family) || nzchar(input$genus) || nzchar(input$species)) {
          tags$ul(
            if (nzchar(input$parkid))  tags$li(HTML(paste0("<b>Park ID:</b> ", input$parkid))),
            if (nzchar(input$family))  tags$li(HTML(paste0("<b>Family:</b> ", input$family))),
            if (nzchar(input$genus))   tags$li(HTML(paste0("<b>Genus:</b> ", input$genus))),
            if (nzchar(input$species)) tags$li(HTML(paste0("<b>Species:</b> ", input$species)))
          )
        }
      )
    })
    
    
    output$images <- renderUI({
      d <- filtered()
      if (nrow(d) == 0) {
        return(div(class = "muted", "No images match the current filters."))
      }
      
      tags$div(
        class = "grid",
        lapply(seq_len(nrow(d)), function(i) {
          # We’ll send both the image URL and a label/caption
          img_src <- d$web_path[i]
          caption <- sprintf(
            "Park ID: %s — %s %s %s — %s",
            # d$filename[i],
            ifelse(is.na(d$parkid[i]),  "—", d$parkid[i]),
            ifelse(is.na(d$genus[i]),   "",  d$family[i]),
            ifelse(is.na(d$genus[i]),   "",  d$genus[i]),
            ifelse(is.na(d$species[i]), "",  d$species[i]),
            ifelse(is.na(d$caab[i]), "",  d$caab[i])
          )
          
          tags$div(
            class = "card",
            # Send a structured payload to input$img_click when clicked
            onclick = sprintf(
              "Shiny.setInputValue('img_click', {src: '%s', caption: '%s', nonce: Math.random()}, {priority: 'event'});",
              htmltools::htmlEscape(img_src),
              htmltools::htmlEscape(caption)
            ),
            tags$img(src = img_src, class = "thumb"),
            tags$div(class = "caption",
                     HTML(sprintf(
                       "<br/>Park ID: %s<br/>Family: %s<br/>Genus: %s<br/>Species: %s<br/>CAAB: %s",
                       # "<b>%s</b><br/>Park ID: %s<br/>Family: %s<br/>Genus: %s<br/>Species: %s<br/>CAAB: %s",
                       # d$filename[i],
                       ifelse(is.na(d$parkid[i]),  "&mdash;", d$parkid[i]),
                       ifelse(is.na(d$family[i]),   "&mdash;", d$family[i]),
                       ifelse(is.na(d$genus[i]),   "&mdash;", d$genus[i]),
                       ifelse(is.na(d$species[i]), "&mdash;", d$species[i]),
                       ifelse(is.na(d$caab[i]), "&mdash;", d$caab[i])
                     ))
            )
          )
        })
      )
    })
    
    # Pop up images
    observeEvent(input$img_click, {
      info <- input$img_click
      if (is.null(info) || is.null(info$src)) return()
      
      # Basic pop-up window
      # showModal(
      #   modalDialog(
      #     easyClose = TRUE,
      #     footer = modalButton("Close"),
      #     size = "l",  # large modal
      #     tags$div(
      #       tags$img(src = info$src, class = "modal-lg-img"),
      #       if (!is.null(info$caption) && nzchar(info$caption)) {
      #         tags$div(class = "modal-title-small", info$caption)
      #       }
      #     )
      #   )
      # )
      
      # Pop-up window with panzoom
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          
          # Container + image
          tags$div(
            id = "zoom-container",
            tags$img(id = "zoom-img", src = info$src)
          ),
          
          # Init Panzoom each time the modal is shown
          tags$script(HTML("
      (function() {
        var $elem = $('#zoom-container');
        // Destroy any prior panzoom to avoid double-binding if user opens multiple times
        try { $elem.panzoom('destroy'); } catch(e) {}
        $elem.panzoom({
          contain: 'invert',
          minScale: 1,
          maxScale: 8,
          increment: 0.1
        }).on('mousewheel.focal', function(e) {
          e.preventDefault();
          var delta = e.delta || e.originalEvent.wheelDelta;
          var zoomOut = delta ? delta < 0 : e.originalEvent.deltaY > 0;
          $elem.panzoom('zoom', zoomOut, { animate: false, focal: e });
        });
      })();
    "))
        )
      )
      
    })
    
    
  }
  
  # RUN ----
  shinyApp(ui, server)
}

