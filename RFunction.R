library('move2')
library("httr2")
library("assertthat")
library("rlang")
library("dplyr")
library("openssl")
library("fs")
library("readr")
library("purrr")
library("sf")
library("move") # "indirect" dependency: required to open/read appended objects in movestack format

# NOTE 1: For App testing purposes, use the more complete script in
# "~/tests/app-testing.r" (instead of 'sdk.r')

# NOTE 2: HTTP requests to the MoveApps API built below based on the example code provided in
# <https://github.com/movestore/movestore.github.io/blob/master/web-partner-api/example.html>.

# NOTE 3: Code relies on the current the structure and attribute names of MoveApps's
# API. If some of its naming conventions change in the future, the code will most likely
# be exposed to errors in HTTP requests.
#
# NOTE 4: Currently only supporting products stored as csv, txt or rds files
#
# TODO: Expand support for Products comprising raster data and shapefiles


rFunction = function(data = NULL, 
                     usr, 
                     pwd, 
                     workflow_title,
                     app_title = NULL,
                     app_pos = NULL, 
                     product_file,
                     track_combine = c("merge", "rename"),
                     return_on_fail = FALSE
                     ){
  
  # input processing -----------------------------------------------------------
  # Required step to deal with current MoveApps behaviour of converting NULL
  # defaults for string inputs, as specified in appspec.json, to "" in the App
  # Settings GUI. NULL is a valid and determining input for app_title
  if(not_null(app_title) && app_title == "") app_title <- NULL
  
  # input validation -----------------------------------------------------------
  if(!is.null(data)) assertthat::assert_that(mt_is_move2(data))
  
  track_combine <- rlang::arg_match(track_combine)
  
  assertthat::assert_that(assertthat::is.string(usr))
  assertthat::assert_that(usr != "", msg = "Input for Workflow ID (`usr`) is missing.")
  assertthat::assert_that(!grepl("\\s", usr), msg = "Invalid Workflow ID (`usr`): string must not contain any whitespaces.")
  assertthat::assert_that(assertthat::is.string(pwd))
  assertthat::assert_that(pwd != "", msg = "Input for Workflow password (`pwd`) is missing.")
  assertthat::assert_that(assertthat::is.string(workflow_title))
  assertthat::assert_that(workflow_title != "", msg = "Input for Workflow title (`workflow_title`) is missing.")
  assertthat::assert_that(assertthat::is.string(product_file))
  assertthat::assert_that(product_file != "", msg = "Input for Product filename (`product_file`) is missing.")
  # app_title and app_pos are interchangeable so null input is acceptable 
  if(not_null(app_title)) assertthat::assert_that( assertthat::is.string(app_title)) 
  if(not_null(app_pos)) assertthat::assert_that(is.numeric(app_pos))
  
  
  if(is.null(app_title) & is.null(app_pos)){
    stop(
      paste0("Either the Title of the App (`app_title`) or its position in the ", 
             "Workflow (`app_pos`) must be specified."), 
      call. = FALSE)
  }
  
  
  # input processing -----------------------------------------------------------
  
  # Get basename and extension of user-defined target file
  product_file_base <- fs::path_ext_remove(product_file)
  product_file_ext <- fs::path_ext(product_file)
  
  
  # fetch links to products from all Apps in the workflow of interest -----------
  logger.info("Fetching API endpoints to Apps and associated Products in target Workflow")
  wf_products_resp <- get_workflow_products(usr, pwd)
  
  # Process response  --------------------------------------------------------
  wf_products_temp <- purrr::map(wf_products_resp$results, as.data.frame) |> 
    purrr::list_rbind() |> 
    # add useful info
    dplyr::mutate(
      workflow_title = workflow_title,
      instance_title = wf_products_resp$workflowInstanceTitle, 
      .before = 1
    ) 
  
  if (nrow(wf_products_temp != 0)) {
    wf_products <- wf_products_temp |> 
      # split basename and extension
      dplyr::mutate(
        # shift app positions one place as API counts them from zero. Starting from 1 more user-friendly
        appPositionInWorkflow = appPositionInWorkflow + 1,
        file_basename = fs::path_ext_remove(fileName),
        file_ext = fs::path_ext(fileName)
      )
  } else {
    logger.warn("This workflow has no results, and may have hit an error")
    if (is.null(data)) {
      logger.fatal("No input data to return. Terminating retrieval")
      stop()
    } else {
      
      if (return_on_fail == TRUE) {
        logger.warn("return_on_fail is set to TRUE. Returning input data with no further data retrieved")
        return(data)
      } else {
        logger.fatal("return_on_fail is set to FALSE. Terminating retrieval")
        stop()
      }
      
    }
  }

  # generate workflow label for error messaging 
  wflw_inst_label <- paste0("'", workflow_title, ": ", wf_products$instance_title[1], "'")
  
  # List products' metadata in target app  -------------------------------------
  logger.info("Listing Products metadata in target App")
  
  if(not_null(app_pos) & is.null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appPositionInWorkflow == app_pos)
    
    # non-existent/invalid user-specified App position
    if(nrow(app_products) == 0){
      
      logger.warn(paste0("There is no App with name matching '", app_title, "' in position #", 
                         app_pos, " of Workflow ", wflw_inst_label, ".  ",
                         "Make sure parameters `app_title` and `app_pos` point coherently to the target App."
      ))
      
      if (is.null(data)) {
        logger.fatal("No input data to return. Terminating retrieval")
        rlang::abort(message = c(
          paste0("There is no App with name matching '", app_title, "' in Workflow ", 
                 wflw_inst_label, "."),
          "i" = paste0("Please check the Workflow page and make sure the",
                       " title of the target App is spelled accurately in",
                       " parameter `app_title` (case-sensitive).")
        ),
        call = NULL
        )
      } else {
        
        if (return_on_fail == TRUE) {
          # If there is data, return it with warning
          logger.warn("return_on_fail is set to TRUE. Returning input data with no further data retrieved")
          return(data)
        } else {
          logger.fatal("return_on_fail is set to FALSE. Terminating retrieval")
          stop()
        }
   
      }
      
      
    }
    
    # set app title as stated in the API
    app_title <- app_products$appTitle[1]
    
  }else if(not_null(app_pos) & not_null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appPositionInWorkflow == app_pos, appTitle == app_title)
    
    # non-existent/invalid user-specified App title in user-specified position
    if(nrow(app_products) == 0){
      
      logger.warn(paste0("There is no App with name matching '", app_title, "' in position #", 
                         app_pos, " of Workflow ", wflw_inst_label, ".  ",
                         "Make sure parameters `app_title` and `app_pos` point coherently to the target App."
      ))
      
      if (is.null(data)) {
        logger.fatal("No input data to return. Terminating retrieval")
        rlang::abort(message = c(
          paste0("There is no App with name matching '", app_title, "' in Workflow ", 
                 wflw_inst_label, "."),
          "i" = paste0("Please check the Workflow page and make sure the",
                       " title of the target App is spelled accurately in",
                       " parameter `app_title` (case-sensitive).")
        ),
        call = NULL
        )
      } else {
        
        if (return_on_fail == TRUE) {
          # If there is data, return it with warning
          logger.warn("return_on_fail is set to TRUE. Returning input data with no further data retrieved")
          return(data)
        } else {
          logger.fatal("return_on_fail is set to FALSE. Terminating retrieval")
          stop()
        }
        
      }
      
      
    }
  } else if(is.null(app_pos) & not_null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appTitle == app_title)
    
    # non-existent/invalid user-specified App title
    if(nrow(app_products) == 0){
      
      logger.warn(paste0("There is no App with name matching '", app_title, "' in position #", 
                         app_pos, " of Workflow ", wflw_inst_label, ".  ",
                         "Make sure parameters `app_title` and `app_pos` point coherently to the target App."
      ))
      
      if (is.null(data)) {
        logger.fatal("No input data to return. Terminating retrieval")
        rlang::abort(message = c(
          paste0("There is no App with name matching '", app_title, "' in Workflow ", 
                 wflw_inst_label, "."),
          "i" = paste0("Please check the Workflow page and make sure the",
                       " title of the target App is spelled accurately in",
                       " parameter `app_title` (case-sensitive).")
        ),
        call = NULL
        )
      } else {
        
        if (return_on_fail == TRUE) {
          # If there is data, return it with warning
          logger.warn("return_on_fail is set to TRUE. Returning input data with no further data retrieved")
          return(data)
        } else {
          logger.fatal("return_on_fail is set to FALSE. Terminating retrieval")
          stop()
        }
        
      }
      
      
    }
    
    # Dealing with multiple copies of same app in a Workflow, when only app_title is specified
    # Assumes Products in any given App have unique filenames
    if(any(duplicated(app_products$fileName))){
      
      if (return_on_fail == TRUE) {
        logger.warn(paste0(
            "Unable to unambiguously identify the specified target App.",
            "There is more than one copy of App '", app_title, 
                         "' in the target Workflow ", wflw_inst_label, ".",
            "Please provide the target App position (`app_pos`)."
        ))
        
        if (!is.null(data) & return_on_fail == TRUE) {
          logger.warn("return_on_fail is TRUE. Returning input data") 
          return(data)
        } else {
          stop("return_on_fail is TRUE but no input data is provided. Terminating retrieval")
        }
        
        
      } else {
        rlang::abort(message = c(
          "Unable to unambiguously identify the specified target App.",
          "x" = paste0("There is more than one copy of App '", app_title, 
                       "' in the target Workflow ", wflw_inst_label, "."),
          "i" = "Please provide the target App position (`app_pos`)."
        ),
        call = NULL
        )
      }
      
    }
  }
  
  
  # Get target Product metadata  --------------------------------------------
  logger.info("Getting details of target Product")
  
  # target product
  prod_meta <- app_products |>
    dplyr::filter(file_basename == product_file_base)
  
  # non-existent/invalid user-specified Product name 
  if(nrow(prod_meta) == 0){
    rlang::abort(message = c(
      paste0("There is no Product named '", product_file, "' in App '", 
             app_title, "' in Workflow ", wflw_inst_label, "."),
      "i" = paste0("Make sure the target product is an output of the specified",
                   " App and its filename is correctly defined (parameter `product_file`;",
                   " case-sensitive).")), 
      call = NULL)
  }
  
  
  # Dealing with multiple files in App with same basename, but different extensions
  if(nrow(prod_meta) > 1){

    # If extension missing, throw error asking user to include it in filename
    if(product_file_ext == ""){

      rlang::abort(message = c(
        "Unable to unambiguously identify the target Product.",
        "x" = paste0("Found more than one Product with basename '", product_file,
               "' in App '", app_title, "' in Workflow ", wflw_inst_label, "."),
        "i" = paste0("Please include the file extension when specifying the",
        " filename of the target Product (parameter `product_file`).")),
        call = NULL)

    } else{
      prod_meta <- prod_meta |>
        dplyr::filter(file_ext == product_file_ext)
    }
  }
  
  
  
  # NOTE: assumes files in a given app have unique filenames (i.e. basename + extension)
  logger.info("Downloading and processing object in target Product")
  
  prod_obj <- get_product_object(usr, pwd, prod_meta$self, prod_meta$file_ext)

  
  
  # Combining retrieved product with input data ----------------------------------
  # 
  # Stack-up product to input if and only if product is of type move2_loc. Otherwise
  # product is annexed as an attribute of the input data
  
  logger.info("Combining retrieved product with input object")
  
  #' Get product currently appended to the input dataset (from upstream copy of 
  #' 'Workflow-Products-Retriever'), if any
  appended_products <- attr(data, "appended_products")
  
  
  if(is_move2_loc(prod_obj)){
    
    if(is.null(data)){
      
      # delete appended data in retrieved product, if any
      attr(prod_obj, "appended_products") <- NULL
      
      # retrieved product becomes the "main data" 
      data <- prod_obj
      
    }else{
      
      logger.info("Retrieved Product is a 'move2_loc' object, so it will be stacked to the input dataset")
      
      # homogenize CRS projections 
      if (sf::st_crs(data) != sf::st_crs(prod_obj)){
        
        prod_obj <- sf::st_transform(prod_obj, sf::st_crs(data))
        
        logger.warn(
          paste0(
            "Input and retrieved datasets are in different CRS projections. ",
            "The retrieved dataset has been re-projected to produce a combined ",
            "dataset with the '", sf::st_crs(data)$input,"' projection.")
        )
      }
      
      logger.info(
        paste0("Product data stacked to the input data using the chosen '", 
               track_combine, "' option.")
      )
      #' NOTE: any appended products in `data` and `prod_obj` are automatically 
      #' dropped when used in `mt_stack()`
      data <- mt_stack(data, prod_obj, .track_combine = track_combine)
      
      # remove duplicate timestamps within a track
      if (!mt_has_unique_location_time_records(data)){
        
        n_dupl <- length(which(duplicated(paste(mt_track_id(data), mt_time(data)))))
        
        logger.warn(
          paste("Stacked data has", n_dupl, "duplicated location-time records. Removing",
                "those with less info and then select the first if still duplicated.")
        )
        data <- mt_drop_duplicates(data)
      }
    }
    
    to_append <- list(
      metadata = prod_meta |> dplyr::select(-self) |> dplyr::mutate(append_type = "stacked")
    )
    
  } else{
    
    logger.info("Retrieved Product is not a 'move2_loc' object, so attaching it as an attribute of the input dataset.")
    
    if(is.null(data)){
      
      # i.e., if it's a starting app AND the retrieved object is not move2_loc, 
      # generate empty move2 object to append retrieved objects to
      data <- data.frame(timestamp = 1, track = "a", x = 0, y = 0) |> 
        mt_as_move2("timestamp", "track", coords = c("x", "y")) |> 
        filter_track_data(.track_id = "b") # make it empty
    }
    
    to_append <- list(
      metadata = prod_meta |> dplyr::select(-self) |> dplyr::mutate(append_type = "attached"),
      object = prod_obj
    )
    
  }
  
  
  # Appending metadata and data of fetched product to input data ----------------
  
  # either append as a newly created list, or add to previous appended list
  if(is.null(appended_products)){
    attr(data, "appended_products") <- list(to_append)
    appended_pos <- 1
  }else{
    attr(data, "appended_products") <- append(appended_products, list(to_append))
    appended_pos <- length(appended_products) + 1
  }
  
  
  # Log out info on retrieved product
  if(to_append$metadata$append_type == "stacked"){
    logging_text <- "The following Product was stacked to the input dataset"    
  }else{
    logging_text <- paste0(
      "The following Product was appended to list element #", 
      appended_pos, 
      " of attribute `appended_products` of the output object"
    )
  }
  
  cat(
    paste0(
      "\n", logging_text, ": \n\n",
      "  Product: '", prod_meta$fileName, "' \n",
      "File Size: '", prod_meta$fileSize, "' \n",
      " Modified: '", prod_meta$modifiedAt, "' \n", 
      "      App: '", prod_meta$appTitle, "' \n",
      " Workflow: '", prod_meta$workflow_title, "' \n",
      " Instance: '", prod_meta$instance_title, "' \n\n"
    )
  )
  
  
  # Export metadata of appended product as artifact --------------------------------
  readr::write_csv(
    to_append$metadata, 
    file = appArtifactPath("appended_product_metadata.csv")
  )
  
  
  logger.info("Job done!")
  
  return(data)
}






#' /////////////////////////////////////////////////////////////////////////////
#' Retrieve metadata of products generated in target workflow
#' 
get_workflow_products <- function(usr, pwd){
  
  # MoveApps API base server url
  base_url <- "https://www.moveapps.org/web-partner/v1/workflowInstances/"
  
  # build api request
  wf_prods_req <- httr2::request(base_url) |> 
    httr2::req_url_path_append(usr) |> 
    httr2::req_url_path_append("artifacts/index") |> 
    httr2::req_headers(
      # encoding to base-64 a bit fiddly. jsonlite::base64_enc() didn't work.
      # openssl::base64_encode() appears to do the trick
      Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd))), 
      Accept = "application/json"
    )
  
  
  # submit request and convert from json format
  rlang::try_fetch(
    wf_prods_req |> httr2::req_perform() |> httr2::resp_body_json(),
    httr2_http_401 = function(cnd){
      rlang::abort(
        message = paste0(
          "API request error: Failed to retrieve Workflow details due to invalid ",
          "Workflow API ID (`usr`) and/or API Password (`pwd`)"),
        parent = NA,
        error = cnd, 
        class = "httr2_http_401"
      )
    },
    httr2_http_400 = function(cnd){
      rlang::abort(
        message = paste0(
          "API request error: Failed to retrieve Workflow details due to invalid ",
          "Workflow API ID (`usr`) and/or API Password (`pwd`)"),
        parent = NA,
        error = cnd,
        class = "httr2_http_400"
      )
    }
  )
}


#' /////////////////////////////////////////////////////////////////////////////
#' Download product from API and convert to tibble
#' 
get_product_object <- function(usr, pwd, product_link, file_ext){
  
  filetypes <- c("rds","csv", "txt")
  
  if(file_ext %notin% filetypes){
    rlang::abort(
      message = c(
        "Unsupported target Product file type",
        "x" = paste0("'.", file_ext, "' files are currently not supported for appending",
                     " products from other Workflows to the input data"),
        "i" = paste0("Presently only supporting Products stored as ", 
                     combine_words(filetypes, before = "'.", after = "'"), " files")
      ),
      class = "unsupported_file_extension",
      call = NULL
    )
  }
  
  # build http request
  prod_req <- httr2::request(product_link) |> 
    httr2::req_headers(
      Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd)))
    )
  
  # Submit request
  prod_resp <- httr2::req_perform(prod_req)
  
  # Parse returned API response as an R object
  if(file_ext %in% c("csv", "txt")){
    
    # convert data in body section to string and convert to tibble. 
    # read_delim() accepts literal data as input
    prod_obj <- prod_resp |> 
      httr2::resp_body_string() |>
      readr::read_delim(show_col_types = FALSE)
    
  }else if(file_ext == "rds"){
    
    # attempt to retrieve compression type from content type
    compression <- strsplit(httr2::resp_content_type(prod_resp), split = "/")[[1]][2]

    # set to "unknown" if unavailable from content type, i.e. leaving it to
    # memDecompress() to detect type of compression
    if(compression %notin% c("gzip", "bzip2", "xz")){
      compression <- "unknown"
    }
    
    prod_obj <- prod_resp |> 
      httr2::resp_body_raw() |> 
      memDecompress(type = compression) |>
      unserialize()
  }
  
  return(prod_obj)
}


#'////////////////////////////////////////////////////////////////////////////////////
#' check if object is of type move2_loc, here defined as a move2 object with
#' *at least one* non-empty location point
is_move2_loc <- function(x){
  mt_is_move2(x) && any(!sf::st_is_empty(x))
}



#'////////////////////////////////////////////////////////////////////////////////////
#' Remove time-location duplicates without user interaction, selecting row with
#' most-info. Code stolen from the MoveApp 'Movebank-Loc-move2'
#' (https://github.com/movestore/Movebank-Loc-move2/blob/master/RFunction.R)
mt_drop_duplicates <- function(x){
  x %>%
    dplyr::mutate(n_na = rowSums(is.na(pick(everything())))) %>%
    dplyr::arrange(mt_track_id(.), mt_time(.), n_na) %>% 
    mt_filter_unique(criterion='first') # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA. 
}


#'////////////////////////////////////////////////////////////////////////////////////
# hacked version of stolen `knitr::combine_words`, to avoid dependency on whole {knitr}
combine_words <- function(words, sep = ", ", and = " and ", before = "", after = before, 
                          oxford_comma = FALSE) 
{
  n = length(words)
  if (n == 0) 
    return(words)
  words = paste0(before, words, after)
  if (n == 1) 
    return(words)
  if (n == 2) 
    return(paste(words, collapse = if (is_blank(and)) sep else and))
  if (oxford_comma && grepl("^ ", and) && grepl(" $", sep)) 
    and = gsub("^ ", "", and)
  words[n] = paste0(and, words[n])
  if (!oxford_comma) {
    words[n - 1] = paste0(words[n - 1:0], collapse = "")
    words = words[-n]
  }
  paste(words, collapse = sep)
}


is_blank <- function(x){
  grepl("^\\s*$", x)
}
  


#' Useful wee helpers ///////////////////////////////////////////////////////
"%notin%" <- Negate("%in%")
not_null <- Negate(is.null)



                    