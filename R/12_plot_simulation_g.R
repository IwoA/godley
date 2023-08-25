#' Plot simulations of multiple variables in multiple scenarios
#' 
#' Returns ggplot2 plot which could be further tweaked.
#' 
#' This functions is created to help preparing publication ready plots. As any normal ggplot2 plot it can be
#' modified using any function from the ggplot2 universe.
#' 
#' #' To remove unnecessary legends use + guides(colour = "none", linetype = "none") after the function call.
#' 
#' `expressions` allows to create own variables like "W/Y". It works for plot_simulation() too.
#' 
#' @seealso [plot_simulation()] for interactive plots best for model testing.
#'
#' @import ggplot2
#' @export
#'
#' @param model SFC model object
#' @param scenario vector of strings or single string name of scenario(s) from which take variables values, defaults to 'baseline'
#' @param take_all logical indicating whether all scenarios containing the given scenario name string(s) should be used, defaults to FALSE
#' @param from numeric period number from which the plot should start, defaults to maximum value
#' @param to numeric period number on which the plot should end, defaults to minimum value
#' @param expressions vector of strings or single string name of variable(s) expression(s) to plot, defaults to 'Y'. Accepts basic math formulas: `+`, `-`, `*`, `/`
#' @param colors vector of strings or single string with desired color names
#' @param max.overlaps sets max.overlaps as in ggrepel::geom_text_repel()
#'
#' @returns ggplot2 plot

plot_simulation_g <- function(model,
                            scenario = "baseline",
                            take_all = FALSE,
                            from = NA,
                            to = NA,
                            expressions = "Y",
                            colors = NULL,
                            max.overlaps = 10) {
  # argument check
  # type
  checkmate::assert_class(model, "SFC")
  checkmate::assert_character(scenario)
  checkmate::assert_flag(take_all)

  checkmate::assert(
    checkmate::check_int(from, na.ok = T, lower = 0),
    checkmate::check_string(from, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(from)
  )
  if (
    checkmate::test_string(from, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(from)
  ) {
    checkmate::assert_date(as.Date(from))
    from <- as.Date(from)
  }

  checkmate::assert(
    checkmate::check_int(to, na.ok = T, lower = 0),
    checkmate::check_string(to, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}"),
    checkmate::check_date(to)
  )
  if (
    checkmate::test_string(to, na.ok = T, pattern = "\\d{4}-\\d{2}-\\d{2}") |
      checkmate::test_date(to)
  ) {
    checkmate::assert_date(as.Date(to))
    to <- as.Date(to)
  }

  checkmate::assert_character(expressions)
  checkmate::assert_character(colors, null.ok = TRUE)
  checkmate::assert_number(max.overlaps)

  # conditions
  if (all(!is.na(c(from, to)))) {
    if (class(from) != class(to)) {
      stop("From and to are not of the same class, choose either numeric or Date")
    }
    if (from > to) {
      stop("From cannot be after to")
    }
  }

  if (take_all == TRUE) {
    scenarios <- c()
    for (i in scenario) {
      scenario_i <- names(model)[grepl(i, names(model))]
      scenarios <- append(scenarios, scenario_i)
    }

    scenario_pass <- scenario
    scenario <- scenarios

    if (is.null(scenario)) {
      stop(paste0("There is/ are no scenario(s) named: ", paste0(scenario_pass, collapse = ", "), " in the model"))
    }
  }

  if (!(all(scenario %in% names(model)))) {
    stop(paste0("There is/ are no scenario(s) named: ", paste0(scenario[!(scenario %in% names(model))], collapse = ", "), " in the model"))
  }

  results <- model$baseline$result %>% dplyr::select(time)

  for (i in scenario) {
    m <- model[[i]]$result

    exprs <- lapply(expressions, function(x) {
      gsub(godley:::.pvar(names(m)), "m\\[, '\\1'\\]", x, perl = T)
    })
    exprs <- purrr::map(exprs, function(x) parse(text = x))

    result_var <- m %>% dplyr::select(time)
    for (n in 1:length(expressions)) {
      result <- eval(exprs[[n]])
      name <- paste0(i, ": ", stringr::str_trim(stringr::str_split(expressions[[n]], "=")[[1]][1]))
      names(result) <- name
      result_var <- result_var %>% tibble::add_column(result)
    }

    results <- dplyr::full_join(results, result_var, by = "time")
  }

  if (is.na(from)) {
    from <- min(results$time)
  }
  if (is.na(to)) {
    to <- max(results$time)
  }

  results <- results %>% dplyr::filter(time >= from, time <= to)

  r <- tidyr::pivot_longer(results, -time, names_to = "name")
  r <- dplyr::mutate(r, scenario = stringr::str_split_i(r$name,":", 1),
              scenario = ifelse(grepl("_", scenario), gsub("sensitivity_", "", scenario), scenario),
              var_name = stringr::str_split_i(r$name,": ", 2))

    fig <- ggplot(r, aes(x = time, y = value, group_by = var_name, col = var_name)) + 
      geom_line(aes(linetype = scenario)) +
      #geom_point(aes(shape = var_name), size = 3) +
      ggrepel::geom_text_repel(data=subset(r, time == max(r$time)), 
                               aes(x = time, 
                                   label = paste(var_name, round(value, 2))
                               ),
                               size = rel(3),
                               hjust = "left",
                               direction = "y",
                               max.overlaps = max.overlaps,
                               show.legend = FALSE) +
      labs(x = element_blank(), y = element_blank()) +
      theme_minimal()+
      theme(legend.title = element_blank())
 
  
  if (!is.null(colors)){
    fig <- fig +
      scale_color_manual(values = colors)
  }

  return(fig)
}
