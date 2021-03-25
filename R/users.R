initialize <- function(users_f) {
    private$data <- utils::read.csv(users_f)
}

users <- R6::R6Class('users',
                     public = list(
                         initialize = initialize,
                         translators = function(){
                             tmp <- private$data
                             tmp[tmp$translator, 'gh_user']
                         },
                         revisors = function(){
                             tmp <- private$data
                             tmp[tmp$revisor, 'gh_user']
                         },
                         print = function(){
                             print(private$data)
                         },
                         mention_revisors = function(){
                             cat("\n\n",
                                 sprintf("@%s", self$revisors()),
                                 "\n", sep = ' ')
                         }
                     ),
                     private = list(
                         data = NULL
                     ))
