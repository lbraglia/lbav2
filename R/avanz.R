#' classe che rappresenta uno stato avanzamento
avanz <-
    R6::R6Class(
            ## class name
            "avanz",
            ## 
            public = list(
                initialize = function(f){
                    private$file <- f
                    private$data <- self$from_disk()
                },
                
                from_disk = function() {
                    if (file.exists(private$file)){
                        private$data <- utils::read.csv(file = private$file)
                    } else {
                        warning(private$file, 'non esistente')
                    }
                },
                
                to_disk = function(newdata = NULL) {
                    if (!is.null(newdata)){
                        utils::write.csv(x = newdata,
                                         file = private$file,
                                         row.names = FALSE)
                    } else if (!is.null(private$data)) {
                        utils::write.csv(x = private$data,
                                         file = private$file,
                                         row.names = FALSE)
                    } else {
                        stop("non so che dati avanzamento esportare, bro")
                    }
                },
                setup = function(trn_filenames){

                    avanz <- data.frame(trn_filename    = trn_filenames,
                                        trn_assignee    = "",
                                        trn_assigned    = FALSE,
                                        trn_completed   = FALSE,
                                        rev1_assigned   = FALSE,
                                        rev1_completed  = FALSE,
                                        rev2_filename   = "",
                                        rev2_assignee   = "",
                                        rev2_assigned   = FALSE,
                                        rev2_completed  = FALSE)
                    self$to_disk(newdata = avanz)
                }
            ),
            private = list(
                file = NULL,
                data = NULL
            ))
