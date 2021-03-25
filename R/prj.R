initialize <- function(id, yt_id){
    ## Inizializza l'oggetto
    
    ## id
    private$id      <- id
    private$yt_id   <- yt_id

    ## utenti
    private$users <- users$new(users_f = users_file)

    ## directory e file del progetto
    private$prj_dir  <- sprintf('%s/%s', subs_dir, id)
    private$avanz_f  <- sprintf('%s/%s', private$prj_dir, 'zz_avanzamento.csv')
    private$avanz    <- avanz$new(private$avanz_f)
    private$source_srt_f <- sprintf('%s/%s.srt', source_dir, id)
}

setup <- function(chunks_len_mins){

    ## import and split source
    private$source_srt <- srt$new(id = 'source',
                                  f = private$source_srt_f)
    chunks <- private$source_srt$split(chunks_len_mins = chunks_len_mins,
                                       yt_id = private$yt_id,
                                       output_dir = private$prj_dir)
    fnames <- unlist(lapply(chunks, function(c) c$fn))
    ## setup monitoring
    private$avanz$setup(trn_filenames = fnames)
}


#' classe che rappresenta un progetto
#' 
#' @export
prj <- R6::R6Class(classname = "prj",
                   public = list(
                       initialize = initialize,
                       setup = setup
                   ),
                   private = list(
                       id = NULL,
                       yt_id = NULL,
                       ## directory e file
                       prj_dir = NULL,
                       ## utenti
                       users = NULL,
                       ## filepath e oggetto avanzamento
                       avanz_f = NULL,
                       avanz   = NULL,
                       ## file e oggetto sorgente
                       source_srt_f = NULL,
                       source_srt   = NULL
                       
                   ))
                       

