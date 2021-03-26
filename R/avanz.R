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
                                        rev1_assignee   = "",
                                        rev1_assigned   = FALSE,
                                        rev1_completed  = FALSE,
                                        rev2_filename   = "",
                                        rev2_assignee   = "",
                                        rev2_assigned   = FALSE,
                                        rev2_completed  = FALSE)
                    self$to_disk(newdata = avanz)
                },
                
                ## obtain assignable by role
                assignable_files = function(role = c('translator', 'revisor1', 'revisor2')){
                    role <- match.arg(role)
                    tmp <- private$data                    
                    if (role == 'translator') {
                        tmp[!tmp$trn_assigned, "trn_filename"]
                    } else if (role == 'revisor1') {
                        row <- tmp$trn_completed & (!tmp$rev1_assigned)
                        tmp[row, "trn_filename"]
                    } else if (role == 'revisor2') {
                        tmp[!tmp$rev2_assigned, "rev2_filename"]
                    }
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

                ## lista i file non finiti per un dato traduttore o revisore
                unfinished_homeworks = function(u, role = c("translator", 'revisor1', 'revisor2')){
                    role <- match.arg(role)
                    tmp <- private$data
                    if (role == 'translator'){
                        row <- with(tmp, trn_assignee %in% u & 
                                         trn_assigned == TRUE & 
                                         trn_completed == FALSE)
                        unfinished <- tmp[row, "trn_filename"]
                    } else if (role == 'revisor1') {
                        row <- with(tmp, rev1_assignee %in% u & 
                                         rev1_assigned == TRUE & 
                                         rev1_completed == FALSE)
                        unfinished <- tmp[row, "rev1_filename"]
                    } else if (role == 'revisor2') {
                        row <- with(tmp, rev2_assignee %in% u & 
                                         rev2_assigned == TRUE & 
                                         rev2_completed == FALSE)
                        unfinished <- tmp[row, "rev2_filename"]
                    }
                    if (length(unfinished) > 0) unfinished else NULL
                },

                assign = function(old_f, assignee, new_f, role = c("translator", "revisor1", "revisor2")){
                    role <- match.arg(role)
                    tmp <- private$data
                    if (role == 'translator'){
                        tmp[tmp$trn_filename %in% old_f, 'trn_assignee'] <- assignee
                        tmp[tmp$trn_filename %in% old_f, 'trn_assigned'] <- TRUE
                        tmp[tmp$trn_filename %in% old_f, 'trn_filename'] <- new_f
                    } else if (role == 'revisor1'){
                        tmp[tmp$rev1_filename %in% old_f, 'rev1_assignee']<- assignee
                        tmp[tmp$rev1_filename %in% old_f, 'rev1_assigned']<- TRUE
                    } else if (role == 'revisor2'){
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_assignee']<- assignee
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_assigned']<- TRUE
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_filename']<- new_f
                    }
                    private$data <- tmp
                }
            ),
            private = list(
                file = NULL,
                data = NULL
            )
        )
