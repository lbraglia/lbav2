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
                             tmp[tmp$revisors, 'gh_user']
                         }
                     ),
                     private = list(
                         data = NULL
                     ))


                     
##                      list-users:
##                              ${RSCRIPT} -e 'db <- read.csv("data/users.csv"); db <- db[order(db[,1])\
## , ]; rownames(db) <- NULL; print(db)' | less

##   summon-revisors:
##           ${RSCRIPT} -e 'db <- read.csv("data/users.csv"); revisors <- db[db[,"re\
## visor"], "gh_user"]; cat("\n\n", sprintf("@%s: ", revisors), "\n", sep = '')'\
## | less

##                                         # lista i traduttori per un determinato progetto
## list-translators:
## ${RSCRIPT} -e "lbav::list_translators(prj = '$(PRJ)')"
