library("ggplot2")

df <- mtcars
df$gear <- as.factor(df$gear)
mod <- lm(mpg ~ disp + hp + gear * qsec, data = df)

library("party")
library("partykit")
cf1 <- cforest(mpg ~ ., data = df)
cf2 <- partykit::cforest(mpg ~ ., data = df)


library("randomForest")
rf <- randomForest(mpg ~ ., data = df)

library("partykit")


# Function for retrieving pdp data from lm objects.
retrieve_data.lm <- function (mod, ...) {
  classes <- attr(mod$terms, "dataClasses")
  y <- as.character(mod$terms)[[2]]
  x <- setdiff(names(classes), y)
  classes <- list(y = classes[y], x = classes[x])
  data <- mod$model
  pred <- function (newdata) stats::predict.lm(mod, newdata)
  # For resampling function
  fn <- as.character(mod$call[[1]])
  args <- as.list(mod$call)[-1]
  args <- args[-which(names(args) == "data")]
#  resample <- function (k) {
#    indices <- replicate(k, sample(1:nrow(data), replace = TRUE), simplify = FALSE)
#    mods <- lapply(1:k, function (i) do.call(fn, append(args, list(data = data[indices[[i]], ]))))
#    mods
#  }
  return(list(data = data, y = y, x = x, classes = classes, predict = pred)) #, resample = resample
}
# glm: logit -> probability!
# svm
# cforest
# ...

# cforest
retrieve_data.RandomForest <- function (mod, data, ...) {

}
# type = c("response", "prob", "votes")
retrieve_data.randomForest <- function (mod, data, ...) {
  if (missing(data)) {
    stop("For model objects of class 'randomForest' the data must be specified explicitly")
  }
  classes <- as.character(attr(mod$terms, "dataClasses"))
  names(classes) <- as.character(attr(mod$terms, "variables"))[-1]
  y <- as.character(mod$terms[[2]])
  x <- setdiff(names(classes), y)
  classes <- list(y = classes[y], x = classes[x])
  if (!all(names(data) %in% c(y, x))) {
    stop("Did you specify the correct data? The variables '", paste(c(y, x)[!c(y, x) %in% names(data)], collapse = "', "), "' are missing")
  }
  data <- data
  pred <- function (newdata, ...) {
    randomForest:::predict.randomForest(mod, newdata, type = type)
  }
  return(list(data = data, y = y, x = x, classes = classes, predict = pred))
}


retrieve_data <- function (mod, ...) UseMethod("retrieve_data") 

# Internal function to return list of possible combinations
get_x_combns <- function (x) {
  interactions <- unclass(as.data.frame(combn(x, 2), stringsAsFactors = FALSE))
  names(interactions) <- NULL
  attributes(interactions) <- NULL
  return(append(as.list(x), interactions))
}



mean_fns <- list(
  "mean" = base::mean,
  "median" = stats::median,
  "mode" = function (x) {
             tab <- table(x)
             names(tab)[which.max(tab)]
           },
  "reference" = function (x) levels(x)[1]
)
get_mean <- function (data, i, controls) {
  method <- controls$mean_methods[[data$classes$x[i]]]
  mean_fns[[method]](data$data[[data$x[i]]])
}
get_all_means <- function (data, controls) {
  means <- lapply(1:length(data$x), get_mean, data = data, controls = controls)
  names(means) <- data$x
  means
}

## Functions for retrieving unique x values on which basis the plots are generated.
#map_methods <- c("unique", "levels", "quantile", "quantile")# "equidistant"
#names(map_methods) <- c("character", "factor", "integer", "numeric")
#
#get_quantiles <- function (x) {
#  nvals <- length(unique(x))
#  nrequired <- 10
#  if (nvals > nrequired) {
#    probs <- seq(0, 1, length.out = nrequired + 1)
#    breaks <- stats::quantile(x, probs = probs)
#    fn <- function (x) cut(x, breaks = breaks, include.lowest = TRUE)
#  } else {
#    fn <- map_fn[["unique"]](x)
#  }
#  fn
#}
#
#get_uniques <- function (x) {
#  xvals <- sort(unique(x))
#  fn <- function (x) factor(x, levels = xvals)
#  fn
#}
#
#get_levels <- function (x) {
#  fn <- function (x) x
#  fn
#}
#
#map_fn <- list(
#  "unique" = get_uniques,
#  "levels" = get_levels,
#  "quantile" = get_quantiles
#)
#
#get_map <- function (data, x) {
#  method <- map_methods[match(data$classes$x[x],
#                        names(map_methods))]
#  fn <- map_fn[[method]](data$data[[x]])
#  fn
#}
#
#get_all_maps <- function (data, x) {
#  x <- unique(unlist(x))
#  maps <- lapply(x, get_map, data = data)
#  names(maps) <- data$x
#  maps
#}


get_plot <- function (newdata, classes) {
  x <- names(classes)
  if (length(x) == 1) {
    p <- ggplot(newdata, aes_string(x = x, y = "effect")) + xlab(x) + ylab("effect")
    if (classes == "numeric") p <- p + geom_line() + geom_point()
    if (classes == "factor") p <- p + geom_boxplot()
    return(p)
  }
  if (length(x) == 2) {
    if (all(classes == "factor")) {
      p <- ggplot(newdata, aes_string(x = x[1], y = x[2], fill = "effect")) + geom_raster() + xlab(x[1]) + ylab(x[2])
      return(p)
    } else {
      factorx <- which(classes == "factor")
      if (length(factorx) > 0) x <- x[c((factorx == 1) + 1, factorx)]
      p <- ggplot(newdata, aes_string(x = x[1], y = x[2], fill = "effect")) + geom_raster(interpolate = FALSE) + xlab(x[1]) + ylab(x[2])
      if (length(factorx) > 0) p <- p + facet_grid(formula(paste(c(x[2], "~", "."), collapse = "")), scales = "free_y")
      return(p)
    }
  }
}


pdp_control <- function (mean_methods = list(factor = "reference",
                                             numeric = "mean",
                                             integer = "mean"),
                         mean_functions,
                         predict_type,
                         effect_type = "multiplicative",
                         k = 10) {
  #predict_type: c("link", "response", "probabilities")
  if (missing(mean_functions)) {
    mean_functions <- mean_fns
  } else {
    mean_functions <- append(mean_fns, mean_functions[!names(mean_functions) %in% names(mean_fns)])
  }
  if (missing(predict_type)) predict_type <- NULL
  controls <- list(mean_methods = mean_methods, mean_functions = mean_functions, predict_type = predict_type, effect_type = effect_type, k = k)
  return(controls)
}

#' @rdname pdp
#' @title Partial dependence plot
#' @description ...
#' @param mod Model object
#' @param x ...
#' @param predict predict function for model object specified in ´mod´
#' @param ... ...
#' @return ...
pdp <- function (mod, x, retrieve, control) { #bootstrap = FALSE, 
  controls <- if (!missing(control)) pdp_control(controls) else pdp_control()

  # Retrieve data and meta data from model object
  data <- retrieve_data(mod)

  # If no variable names are specified for plotting, all possible min and
  # 2-way interaction effects are considered.
  # If variable names and combinations are specified, these are verified.
  if (missing(x)) {
    x <- get_x_combns(data$x)
  } else {
    if (!all(unlist(x) %in% data$x)) {
      stop("Variables", paste(setdiff(unlist(x), data$x), collapse = ", "), 
           "are contained in the model specified")
    }
    # <TODO> else if structure of x not correct (character and list)
    if (any(unlist(lapply(x, length))) > 2) {
      stop("Currently only two-way interactions are implemented")
    }
  }
  # Add variable means to data object.
  data$means <- get_all_means(data, controls)
  # Calculate baseline prediction (all variables at their mean value).
  baseline <- data$predict(data$means)
  # Generate plots for all x combinations.
  plots <- lapply(1:length(x), function (i) {
    # Span data frame with new data.
    newdata <- lapply(x[[i]], function (x) unique(data$data[[x]]))
    names(newdata) <- x[[i]]
    # For numeric variables, a equideistant grid of 100 values is spanned (for interactions of numeric values a grid of 100 x 100 values).
    if (length(x[[i]]) > 1) {
      numericx <- data$classes$x[x[[i]]]
      numericx <- names(numericx)[numericx == "numeric"]
      if (length(numericx) > 0) {
        ranges <- lapply(numericx, function (z) range(newdata[[z]]))
        names(ranges) <- if (length(ranges) == 1) "y" else c("x", "y")
        newdata[numericx] <- approx(x = ranges[[1]], y = (if (length(ranges) == 1) NULL else ranges[[2]]), n = 100)[names(ranges)]
      }
    }
    newdata <- expand.grid(newdata)
    # Add mean values of all variables, which are currently not varied.
    newdata <- cbind(newdata, data$means[setdiff(data$x, x[[i]])])
    # Add predicted response.
    newdata[[data$y]] <- data$predict(newdata)
    # Standardise effect by baseline (additive of multiplicative).
    #newdata$effect <- newdata[[data$y]] - baseline
    newdata$effect <- newdata[[data$y]] / baseline
    # Generate plot.
    p <- get_plot(newdata, data$classes$x[x[[i]]])
    p
  })
  # Draw all partial dependence plots.
  for (p in plots) {
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

pdp(mod)

pdp(lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings))

