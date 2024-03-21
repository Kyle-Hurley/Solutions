#' Title find_vars
#' @description Find the variables and their line numbers in a script.
#' 
#' @param vars List of variable names. May contain nested lists.
#' @param script_path Path to the R script that will be checked for variables named in `vars`
#'
#' @return `results` A list where each element represents a variable from the input list (including 
#'                   variables from nested lists, if any). Each element's name corresponds to the 
#'                   full "path" of the variable within the nested list structure, represented as 
#'                   strings concatenated with `$` (e.g., "topLevel$secondLevel$variableName"). 
#'                   Each element's value is a numeric vector containing the line numbers where the 
#'                   variable was found in the R script. If a variable is not found in the script, 
#'                   it will not appear in the return list.
#' @export
#'
#' @examples
#' \dontrun{
#' # Not run
#' # Define a list of variables (and nested variables) to search for
#' vars_to_find <- list(
#'   topLevelVar = NA,
#'   nested = list(
#'     secondLevelVar = NA,
#'     deeper = list(
#'       thirdLevelVar = NA
#'     )
#'   )
#' )
#'
#' # Assuming 'script.R' is an R script you want to search
#' # Note: You should replace 'script.R' with the actual path to your script
#' result <- find_list_vars(vars_to_find, "script.R")
#'
#' # View the result
#' print(result)
#' }


find_vars <- function(vars, script_path, nested_path = NULL) {
  
  # Recursive iterations handled by nested_path (e.g.,nested lists)
  if (is.null(nested_path)) {
    lines <- readLines(script_path)
  } else {
    # script_path is text lines on recursive iterations
    lines <- script_path
  }
  
  results <- list()
  
  # Iterate over each item in the vars
  for (var_name in names(vars)) {
    
    # Track location in vars
    current_path <- if (is.null(nested_path)) var_name else paste(nested_path, var_name, sep = "$")
    
    # Recursively check for lists
    if (is.list(vars[[var_name]])) {
      
      # Recurse into the nested list
      nested_results <- find_vars(vars[[var_name]], lines, current_path)
      results <- c(results, nested_results)
      
    } else {
      
      line_numbers <- which(grepl(paste0("\\b", var_name, "\\b"), lines))
      
      if (length(line_numbers) > 0) {
        
        results[[current_path]] <- line_numbers
        
      }
    }
    
  }
  
  return(results)
  
}

