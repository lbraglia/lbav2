monitoring <- function(view = TRUE){
    ## colors
    alpha <- 0.6
    cols <- list(compl    = lbmisc::col2hex("green4",  alpha = alpha),
                 iniz     = lbmisc::col2hex("yellow3", alpha = alpha),
                 non_iniz = lbmisc::col2hex("red2",    alpha = alpha))

    col_gen <- function(assigned, completed){
        ifelse(assigned,
        ifelse(completed, cols$compl, cols$iniz),
        cols$non_iniz)
    }

    tmp <- private$data
    tmp$trn_color            <- with(tmp, col_gen(trn_assigned, trn_completed))
    tmp$rev1_color           <- with(tmp, col_gen(rev1_assigned, rev1_completed))
    tmp$rev2_color           <- with(tmp, col_gen(rev2_assigned, rev2_completed))
    ## db <- db[order(db$trn_filename), ]
    ## rownames(db) <- NULL

    ## graphical parames
    data_y   <- -seq_len(nrow(tmp))
    headers  <- c('Spezzone', 'Traduzione', 'Rev1', 'Rev2')
    headers_y <- 1L
    headers_x <- 0:3
    ## first column: text
    text_x   <- rep(0, nrow(tmp))
    text_y   <- data_y
    adj      <- c(0,0.5)
    ## second column: rectangles regarding translation
    trn_x    <- rep(1, nrow(tmp))
    trn_y    <- data_y
    trn_xleft   <- trn_x - 0.5
    trn_ytop    <- trn_y + 0.5 
    trn_xright  <- trn_x + 0.5
    trn_ybottom <- trn_y - 0.5
    ## third column: rectangles regarding revision 1
    rev1_x    <- rep(2, nrow(tmp))
    rev1_y    <- data_y
    rev1_xleft   <- rev1_x - 0.5
    rev1_ytop    <- rev1_y + 0.5 
    rev1_xright  <- rev1_x + 0.5
    rev1_ybottom <- rev1_y - 0.5
    ## fourth column: rectangles regarding revision 2
    rev2_x    <- rep(3, nrow(tmp))
    rev2_y    <- data_y
    rev2_xleft   <- rev2_x - 0.5
    rev2_ytop    <- rev2_y + 0.5 
    rev2_xright  <- rev2_x + 0.5
    rev2_ybottom <- rev2_y - 0.5
    
    ## base graph
    filename <- sprintf("/tmp/%s_%s.png",
                        private$id,
                        format(Sys.Date(), '%Y_%m_%d'))
    png(filename = filename, width = 380)
    par(mar = c(0,0,3,0), oma = rep(0, 4))
    plot(x = c(-0.5, 5), y = c(0, -nrow(tmp)),
         pch = NA,
         main = sprintf("%s: avanzamento al %s", private$id, Sys.Date()),
         ylab = '',
         xlab = '',
         axes = FALSE,
         frame.plot = FALSE)
    ## headers
    trash <- Map(text,
                 x = as.list(headers_x),
                 y = as.list(headers_y),
                 labels = as.list(headers))
    ## starting times
    trash <- Map(text,
                 x = as.list(text_x),
                 y = as.list(text_y),
                 labels = as.list(tmp$trn_start))
    ## translation rectangles
    trash <- Map(rect,
                 xleft  = as.list(trn_xleft),
                 xright = as.list(trn_xright),
                 ytop = as.list(trn_ytop),
                 ybottom = as.list(trn_ybottom),
                 col    = as.list(tmp$trn_color))
    ## revision1 rectangles
    trash <- Map(rect,
                 xleft  = as.list(rev1_xleft),
                 xright = as.list(rev1_xright),
                 ytop = as.list(rev1_ytop),
                 ybottom = as.list(rev1_ybottom),
                 col    = as.list(tmp$rev1_color))
    ## revision2 rectangles
    trash <- Map(rect,
                 xleft  = as.list(rev2_xleft),
                 xright = as.list(rev2_xright),
                 ytop = as.list(rev2_ytop),
                 ybottom = as.list(rev2_ybottom),
                 col    = as.list(tmp$rev2_color))
    
    ## legenda
    legend(x = 3.6, y = -0.5,
           legend = c('Non iniziata', 'Iniziata', 'Completata'),
           fill = c(cols$non_iniz, cols$iniz, cols$compl))
    trash <- dev.off()
    if (view) system(sprintf("feh %s &", filename))
    
}



## --------------------------------------------
## classe che rappresenta uno stato avanzamento
## --------------------------------------------

#'@export 
avanz <-
    R6::R6Class(
            ## class name
            "avanz",
            ## 
            public = list(
                initialize = function(f, id){
                    private$id <- id
                    private$file <- f
                    private$data <- self$from_disk()
                },
                
                ## poni in memoria la versione su disco
                from_disk = function() {
                    if (file.exists(private$file)){
                        private$data <- utils::read.csv(file = private$file)
                    } else {
                        warning(private$file, ' non esistente.')
                        invisible(NULL)
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
                ## e i nomi file revisione
                setup = function(trn_filenames, trn_to_rev_ratio){
                    trn_filenames <- sort(trn_filenames)
                    start_digit <- gsub(".+([[:digit:]]{6}).+", '\\1', trn_filenames)
                    trn_start <- digits_to_time(start_digit)
                    ## determinazione nome file di revisione in base al trn_to_rev
                    trn_fn_to_rev_fn <- function(std, ratio){
                        n_revs <- floor(length(std) / ratio) + 1
                        groups <- gl(n = n_revs, k = ratio)[seq_along(std)]
                        splitted <- split(std, groups)
                        first <- lapply(splitted, function(x) x[1])
                        last  <- lapply(splitted, function(x) x[length(x)])
                        len   <- unlist(lapply(splitted, length))
                        rev_filenames <- unlist(Map(paste0, "revs_", first, "_", last, ".srt"))
                        rep(rev_filenames, times = len)
                    }
                    rev2_filenames <- trn_fn_to_rev_fn(start_digit, trn_to_rev_ratio)
                    ## creazione dell'avanzamento vuoto
                    ## db$rev2_starting_digits <- gsub(rev2_starting_time_ptrn, '\\1', db$rev2_filename)
                    ## db$rev2_ending_digits   <- gsub(rev2_ending_time_ptrn, '\\1', db$rev2_filename)
                    avanz <- data.frame(trn_filename    = trn_filenames,
                                        trn_start       = trn_start,
                                        trn_assignee    = "",
                                        trn_assigned    = FALSE,
                                        trn_completed   = FALSE,
                                        ## 
                                        rev1_assignee   = "",
                                        rev1_assigned   = FALSE,
                                        rev1_completed  = FALSE,
                                        ## 
                                        rev2_filename   = rev2_filenames,
                                        rev2_ready      = FALSE,
                                        rev2_created    = FALSE,
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
                        unique(tmp[with(tmp, rev2_created & !rev2_assigned), "rev2_filename"])
                    }
                },

                ## return filenames for a phase
                filenames = function(phase = c('trn', 'rev2')){
                    phase <- match.arg(phase)
                    tmp <- private$data
                    files <- if (phase == 'trn') tmp$trn_filename
                             else if (phase == 'rev2') unique(tmp$rev2_filename)
                    files
                },
                
                ## lista i traduttori per un determinato progetto
                list_assignee = function(){
                    tmp <- private$data
                    vars <- c("trn_start",
                              "trn_assignee", "rev1_assignee", "rev2_assignee")
                    tmp <- tmp[, vars]
                    tmp[, 2:4] <-  lapply(tmp[, 2:4],
                                          lbprivee::av_yt_gh_user_to_id)
                    names(tmp) <- c("Time", "Translate", "Rev1", "Rev2")
                    cat("```\n")
                    print(tmp, row.names = FALSE)
                    cat("```\n")
                },

                ## lista i file non finiti per un dato traduttore o revisore
                unfinished_homeworks = function(user, role = c("translator", 'revisor1', 'revisor2')){
                    role <- match.arg(role)
                    tmp <- private$data
                    if (role == 'translator'){
                        row <- with(tmp, trn_assignee %in% user & 
                                         trn_assigned == TRUE & 
                                         trn_completed == FALSE)
                        unfinished <- tmp[row, "trn_filename"]
                    } else if (role == 'revisor1') {
                        row <- with(tmp, rev1_assignee %in% user & 
                                         rev1_assigned == TRUE & 
                                         rev1_completed == FALSE)
                        unfinished <- tmp[row, "rev1_filename"]
                    } else if (role == 'revisor2') {
                        row <- with(tmp, rev2_assignee %in% user & 
                                         rev2_assigned == TRUE & 
                                         rev2_completed == FALSE)
                        unfinished <- tmp[row, "rev2_filename"]
                    }
                    if (length(unfinished) > 0) unfinished else NULL
                },

                assign = function(old_f, assignee, new_f, role = c("translator", "revisor2")){
                    role <- match.arg(role)
                    tmp <- private$data
                    if (role == 'translator'){
                        tmp[tmp$trn_filename %in% old_f, 'trn_assignee'] <- assignee
                        tmp[tmp$trn_filename %in% old_f, 'trn_assigned'] <- TRUE
                        tmp[tmp$trn_filename %in% old_f, 'trn_filename'] <- new_f
                    } else if (role == 'revisor2'){
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_assignee']<- assignee
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_assigned']<- TRUE
                        tmp[tmp$rev2_filename %in% old_f, 'rev2_filename']<- new_f
                    }
                    private$data <- tmp
                },

                mark_as_started = function(f, assignee, role = c("revisor1")){
                    role <- match.arg(role)
                    tmp <- private$data
                    if  (role == 'revisor1'){
                        tmp[tmp$trn_filename %in% f, 'rev1_assignee']<- assignee
                        tmp[tmp$trn_filename %in% f, 'rev1_assigned']<- TRUE
                    } 
                    private$data <- tmp
                },

                ## mark completed files and do some check following that
                mark_as_completed = function(file, phase = c('trn', 'rev1', 'rev2')){
                    phase <- match.arg(phase)
                    tmp <- private$data
                    completed_var <- sprintf("%s_completed", phase)
                    filename_var <- switch(phase,
                                           trn  = "trn_filename",
                                           rev1 = "trn_filename",
                                           rev2 = "rev2_filename")
                    tmp[tmp[, filename_var] %in% file, completed_var] <- TRUE
                    ## checks
                    if (phase == 'rev1'){
                        ## mark rev2 file as ready (creabili, poi che
                        ## siano stati creati altro discorso)
                        tmp$rev2_ready <- {
                            spl<- split(tmp$rev1_completed, f = tmp$rev2_filename)
                            rev_ready <- as.logical(unlist(lapply(spl, prod)))
                            len <- lapply(spl, length)
                            rep(rev_ready, times = len)
                        }
                    } else if (phase == 'rev2') {
                        ## check che tutte le rev2 siano complete e
                        ## nel caso danne messaggio
                        if (all(tmp$rev2_completed)){
                            ascii_header('Tutte le revisioni sono complete')
                            ascii_header('creare il file finale con make final-srt')
                        }
                    }
                    private$data <- tmp
                },

                revs2_todo = function(){
                    ## ritorna i nomi dei file di revisioni che
                    ## sarebbero creabili (pronti ma non creati)
                    tmp <- private$data
                    vars <- c("rev2_filename", "rev2_ready", "rev2_created")
                    tmp <- unique(tmp[, vars])
                    rows <- with(tmp, rev2_ready & !(rev2_created))
                    tmp[rows, 'rev2_filename']
                },

                revs2_created = function(f){
                    ## marka una rev2 come creata
                    tmp <- private$data
                    rows <- tmp$rev2_filename %in% f
                    tmp[rows, 'rev2_created'] <- TRUE
                    private$data <- tmp
                },

                get_trn_fn_for_rev2 = function(f){
                    ## ottiene i nomi file di trn per un dato rev2
                    tmp <- private$data
                    sort(tmp[tmp$rev2_filename %in% f, 'trn_filename'])
                },
                
                monitoring = monitoring
                
            ),
            private = list(
                id = NULL,
                file = NULL,
                data = NULL
            )
        )
