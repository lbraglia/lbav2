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
                
                ## poni in memoria la versione su disco
                from_disk = function() {
                    if (file.exists(private$file)){
                        private$data <- utils::read.csv(file = private$file)
                    } else {
                        warning(private$file, 'non esistente')
                    }
                },

                ## salva su disco i dati in memoria oppure i dati passati
                ## come argomento
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

                ## crea il file di avanzamento vuoto ad inizio progetto
                setup = function(trn_filenames){
                    trn_start <- digits_to_time(
                        gsub(extract_ptrn, '\\1', trn_filenames))
                    avanz <- data.frame(trn_filename    = trn_filenames,
                                        trn_start       = trn_start,
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
                },
                
                ## ritorna i file di traduzione assegnabili
                assignable_trn = function(){
                    tmp <- private$data
                    tmp[!tmp$trn_assigned, "trn_filename"]
                },

                ## ritorna i file di revisione assegnabili
                assignable_rev2 = function(){
                    tmp <- private$data
                    tmp[!tmp$rev2_assigned, "rev2_filename"]
                },
                
                ## lista i traduttori per un determinato progetto
                list_assignee = function(){
                    tmp <- private$data
                    assigned <- tmp[tmp$trn_assigned,
                                    c("trn_assignee", 'trn_start')]
                    times <- assigned$trn_start
                    usr <- lbprivee::av_yt_gh_user_to_id(assigned$trn_assignee)
                    cat(apply(data.frame(times, usr), 1, paste,
                              collapse = ' '), sep = '\n')
                },

                ## assign a translate
                assign_trn = function(old, new, assignee){
                    tmp <- private$data
                    tmp[tmp$trn_filename %in% old, 'trn_assignee'] <- assignee
                    tmp[tmp$trn_filename %in% old, 'trn_assigned'] <- TRUE
                    tmp[tmp$trn_filename %in% old, 'trn_filename'] <- new
                    private$data <- tmp
                },

                ## assign a revision (phase 2)
                assign_rev2 = function(old, new, assignee){
                    tmp <- private$data
                    tmp[tmp$rev2_filename %in% old, 'rev2_assignee']<- assignee
                    tmp[tmp$rev2_filename %in% old, 'rev2_assigned']<- TRUE
                    tmp[tmp$rev2_filename %in% old, 'rev2_filename']<- new
                    private$data <- tmp
                }
            ),
            private = list(
                file = NULL,
                data = NULL
            ))
