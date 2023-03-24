# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Callback classes
#'
#' These classes are used to define callback behaviors for use with
#' [read_ipums_micro_chunked()]. They are based on the
#' [`callback`][readr::callback] classes from readr, but
#' have been adapted to include handling of implicit decimal values and
#' variable/value labeling for use with IPUMS microdata extracts.
#'
#' \describe{
#'  \item{IpumsSideEffectCallback}{
#'    Callback function that is used only for side effects, no results are
#'    returned.
#'
#'    Initialize with a function that takes 2 arguments. The first argument
#'    (`x`) should correspond to the data chunk and the second (`pos`)
#'    should correspond to the position of the first observation in the chunk.
#'
#'    If the function returns `FALSE`, no more chunks will be read.
#'  }
#'  \item{IpumsDataFrameCallback}{
#'    Callback function that combines the results from each chunk into a
#'    single output `data.frame` (or similar) object.
#'
#'    Initialize the same way as you would `IpumsSideEffectCallback`. The
#'    provided function should return an object that inherits from `data.frame`.
#'
#'    The results from each application of the callback function will be
#'    added to the output `data.frame`.
#'  }
#'  \item{IpumsListCallback}{
#'    Callback function that returns a list, where each element contains the
#'    result from a single chunk.
#'
#'    Initialize the same was as you would `IpumsSideEffectCallback`.
#'  }
#'  \item{IpumsBiglmCallback}{
#'    Callback function that performs a linear regression on a dataset by chunks
#'    using the biglm package.
#'
#'    Initialize with a function that takes 2 arguments: The first argument
#'    should correspond to a [formula][stats::formula] specifying the regression
#'    model. The second should correspond to a function that prepares the data
#'    before running the regression analysis. This function follows the
#'    conventions of the functions used in other callbacks. Any additional
#'    arguments passed to this function are passed to biglm.
#'  }
#'  \item{IpumsChunkCallback}{
#'    (Advanced) Callback interface definition. All
#'    callback functions for IPUMS data should inherit from this class, and
#'    should use private method `ipumsify` on the data to handle implicit
#'    decimals and value labels.
#'   }
#' }
#'
#' @usage NULL
#' @format NULL
#'
#' @name ipums_callback
#'
#' @keywords internal
#'
#' @export
IpumsChunkCallback <- R6::R6Class(
  "IpumsChunkCallback",
  inherit = hipread::HipChunkCallback,
  private = list(
    data_structure = NULL,
    ddi = NULL,
    var_attrs = NULL,
    rt_ddi = NULL,
    ipumsify = function(data) {
      if (is.null(private$data_structure)) {
        return(data)
      }
      ipumsify_data(
        data, private$data_structure, private$ddi,
        private$var_attrs, private$rt_ddi
      )
    }
  ),
  public = list(
    set_ipums_fields = function(data_structure, ddi, var_attrs, rt_ddi = NULL) {
      private$data_structure <- data_structure
      private$ddi <- ddi
      private$var_attrs <- var_attrs
      private$rt_ddi <- rt_ddi
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsSideEffectCallback <- R6::R6Class(
  "IpumsSideEffectCallback",
  inherit = IpumsChunkCallback,
  private = list(
    cancel = FALSE
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$cancel <- identical(result, FALSE)
    },
    continue = function() {
      !private$cancel
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsDataFrameCallback <- R6::R6Class(
  "IpumsDataFrameCallback",
  inherit = IpumsChunkCallback,
  private = list(
    results = list()
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$results <- c(private$results, list(result))
    },
    result = function() {
      ipums_bind_rows(private$results)
    },
    finally = function() {
      private$results <- list()
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsListCallback <- R6::R6Class(
  "IpumsListCallback",
  inherit = IpumsChunkCallback,
  private = list(
    results = list()
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$results <- c(private$results, list(result))
    },
    result = function() {
      private$results
    },
    finally = function() {
      private$results <- list()
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsBiglmCallback <- R6::R6Class(
  "IpumsBiglmCallback",
  inherit = IpumsChunkCallback,
  private = list(
    acc = NULL,
    prep = NULL,
    model = NULL,
    biglm_f = NULL
  ),
  public = list(
    initialize = function(model, prep = function(x, pos) x, ...) {
      if (!requireNamespace("biglm")) {
        rlang::abort(c(
          "The `biglm` package is required for `IpumsBiglmCallback`.",
          "i" = "Install it with `install.packages(\"biglm\")`"
        ))
      }
      private$prep <- prep
      private$model <- model
      # Some hacks to improve printing of biglm sys.call
      private$biglm_f <- function(model, data) {
        biglm <- biglm::biglm
        eval(bquote(biglm(.(model), data, ...)))
      }
    },
    receive = function(data, index) {
      data <- private$prep(private$ipumsify(data), index)
      if (is.null(private$acc)) {
        private$acc <- private$biglm_f(private$model, data)
      } else {
        private$acc <- stats::update(private$acc, data)
      }
    },
    result = function() {
      private$acc
    }
  )
)
