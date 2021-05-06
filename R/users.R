initialize <- function(users_f) {
    pubdata <- utils::read.csv(users_f, comment = '#')
    privdata <- lbprivee::av_yt_users
    data <- merge(pubdata, privdata, by = 'gh_user', all.x = TRUE)
    data$nomec <- paste(data$nome, strtrim(data$cognome, 1), sep = "")
    private$data <- data
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
                         list_users = function(){
                             ## cat("```\n")                            
                             print(private$data, row.names = FALSE)
                             ## cat("```\n")
                         },
                         ## per il gruppo telegram, usare tg_user
                         mention = function(role = c('translator', 'revisor1', 'revisor2')){
                             tmp <- private$data
                             role <- match.arg(role)
                             if (role == 'translator') {
                                 index <- tmp$translator
                             } else if (role == 'revisor1') {
                                 index <- tmp$revisor1
                             } else if (role == 'revisor2') {
                                 index <- tmp$revisor2
                             } else stop('WTF, why are you here?')
                             
                             select <- tmp[index, ]
                             who <- with(select,
                                         ifelse(!is.na(tg_user), tg_user, nome)
                                         )
                             cat(sprintf("@%s", sort(who)), sep = ' ')
                         }
                     ),
                     private = list(
                         data = NULL
                     ))
