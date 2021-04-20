initialize <- function(users_f) {
    private$data <- utils::read.csv(users_f)
}

#' @export
users <- R6::R6Class('users',
                     public = list(
                         initialize = initialize,
                         translators = function(){
                             tmp <- private$data
                             sort(tmp[tmp$translator, 'gh_user'])
                         },
                         revisors1 = function(){
                             tmp <- private$data
                             sort(tmp[tmp$revisor1, 'gh_user'])
                         },
                         revisors2 = function(){
                             tmp <- private$data
                             sort(tmp[tmp$revisor2, 'gh_user'])
                         },
                         print = function(){
                             print(private$data, row.names = FALSE)
                         },
                         mention = function(role = c('translator', 'revisor1', 'revisor2')){
                             role <- match.arg(role)
                             who <- switch(role,
                                           'translator' = self$translators(),
                                           'revisor1' = self$revisors1(),
                                           'revisor2' = self$revisors2())
                             cat(sprintf("@%s", who), sep = ' ')
                         }
                     ),
                     private = list(
                         data = NULL
                     ))
