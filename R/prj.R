initialize <- function(id, yt_id){
    ## Inizializza l'oggetto
    
    ## id
    private$id      <- id
    private$yt_id   <- yt_id

    ## utenti
    self$users <- users$new(users_f = users_file)

    ## directory e file del progetto
    private$prj_dir  <- sprintf('%s/%s', subs_dir, id)
    private$avanz_f  <- sprintf('%s/%s', private$prj_dir, 'zz_avanzamento.csv')
    private$avanz    <- avanz$new(private$avanz_f)
    private$source_srt_f <- sprintf('%s/%s.srt', source_dir, id)
}


## funzione della main class
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

## --------------
## Create sandbox
## --------------

## importa i login utente github da un file
## f è un path ad un file che include login di github
users_from_file <- function(f){
    users <- if (file.exists(f)) readLines(args$sandbox_file) else NULL
    users %without% ''
}

## genera il nome file (srt) di un sandbox in base a login e tipologia
sandbox_file <- function(user, type = c("translator", "revisor1")){
    type <- match.arg(type)
    postfix <- switch(type, translator = "trn", revisor1 = "rev1")
    sprintf('%s_%s.srt', user, postfix)
}

## funzione che prende in input dei login github e fa il check che
## siano stati abilitati in data/users.csv (a seconda del permesso
## specificato) restituisce gli utenti abilitati, segnala se ve ne
## sono di non abilitati e interrompe se nessuno è abilitato
check_allowed_users <- function(users, type = c('translator', 'revisor')){
    ## obtain allowed users from data/users.csv
    allowed_users <- get_users()
    allowed_translators <- allowed_users$translators
    allowed_revisors  <- allowed_users$revisors
    ## what to be checked
    type <- match.arg(type)
    allowed <- if (type == 'translator') self$users$translators()
               else self$users$revisors()
    ## do the check
    not_allowed_users <- users[users %nin% allowed]
    allowed_users     <- users[users %in% allowed]
    if (length(not_allowed_users) > 0)
        warning('Vi sono richieste per utenti non abilitati (ignorate). ',
                '\n  Gli utenti sono: ',
                paste(not_allowed_users, collapse = ', '),
                '.')
    if (length(allowed_users) == 0L)
        stop("Non vi sono utenti abilitati per questa richiesta.",
             "\n  Processo terminato.")
    allowed_users
}

## funzione della main class
create_sandbox <- function(sandbox_f, rev1_sandbox_f){
    sandbox_users      <- users_from_file(sandbox_f)
    rev1_sandbox_users <- users_from_file(rev1_sandbox_f)
    ## sandbox di translators
    if (length(sandbox_users) > 0L) {
        ## notify: header
        lbmisc::ascii_header('sandbox traduttori')
        ## check permissions
        allowed_users <- check_allowed_users(sandbox_users, 'translator')
        ## files and dirs
        files <- sandbox_file(allowed_users, type = "translator")
        ## copia file
        tmp <- Map(file.copy,
                   from = list(sandbox_template_traduttori),
                   to = files,
                   overwrite = TRUE)
        ## notify: file list
        listing(files)
    }
    ## sandbox di revisori (per revisione 1)
    if (length(rev1_sandbox_users) > 0L) {
        ## notify: header
        lbmisc::ascii_header('sandbox revisori (fase 1)')
        ## check permissions
        allowed_users <- check_allowed_users(rev1_sandbox_users, 'revisor')
        ## files and dirs
        files <- sandbox_file(allowed_users, type = "revisor1")
        ## copia file
        tmp <- Map(file.copy,
                   from = list(sandbox_template_revisori1),
                   to = files,
                   overwrite = TRUE)
        ## notify: file list
        listing(files)
    }
}

## ----------------------------------------------------------------
## Assign
## ----------------------------------------------------------------

check_sandbox <- function(users, type = c('translator', 'revisor1')){
    ## funzione che prende in input dei login github, una tipologia di
    ## utente e verifica che l'utente abbia un sandbox per la
    ## tipologia considerata
    ## restituisce gli utenti che hanno una sandbox o si lamenta e
    ## blocca similmente ad allowed users
    
    type <- match.arg(type)
    users_sandbox <- sprintf('subs/sandbox/%s',
                             sandbox_file(users, type  = type))
                             
    no_sandbox <- users[!file.exists(users_sandbox)]
    if (length(no_sandbox) > 0L){
        msg <- c("Alcuni utenti non hanno ancora il file di sandbox file.\n ",
                 "Sono: ", paste(no_sandbox, collapse = ', '))
        warning(msg)
    }
    users_with_sandbox <- users %without% no_sandbox
    if (length(users_with_sandbox) == 0L)
        stop("No allowed users for this request")
    users_with_sandbox
}
    
check_homework <- function(prj, users, type = c("translator", "revisor2")){
    ## funzione che dato un progetto controlla che un utente di un tipo non
    ## abbia task non finiti (prima di richiedere) nuove assegnazioni
    ## da translator o revisor

    avanz <- leggi_avanzamento_TODOHERE

    type <- match.arg(type)
    ptrn <- switch(type,
                   translator = "^subs_[[:digit:]]{6}_%s.srt$",
                   revisor2 = "^revs_[[:digit:]]{6}_[[:digit:]]{6}_%s\\.srt$")
    
    ok_homework <- function(u, type  = type) {
        ## controlla se un singolo utente ha compiti non finiti in
        ## una determinata tipologia
        non_finiti <- list.files(pattern = sprintf(ptrn, u), path = prj_dir)
        if (length(non_finiti) > 0L) {
            warning(u, ' has unfinished files: ',
                    paste(non_finiti, collapse = ' '),
                    '\n  Ignoring his/her request.')
        }
        all_complete <- length(non_finiti) == 0L
        all_complete
    }
    
    allowed_users <- Filter(f = ok_homework, users)
    if (length(allowed_users) == 0L) stop("No allowed users for this request")
    allowed_users
}


## main class method
assign <- function(translate_f, revise2_f)
{
    ## update aggiornamento: salva su disco
    on.exit(private$avanz$to_disk())

    translate_users <- users_from_file(translate_f)
    revise2_users   <- users_from_file(revise2_f)

    ## translate

    ## revise
    
}    


## ----------------------------------------------------------------
## Main class
## ----------------------------------------------------------------

#' classe che rappresenta un progetto
#' 
#' @export
prj <- R6::R6Class(classname = "prj",
                   public = list(
                       initialize = initialize,
                       setup = setup,
                       ## utenti
                       users = NULL,
                       create_sandbox = create_sandbox,
                       assign = assign
                   ),
                   private = list(
                       id = NULL,
                       yt_id = NULL,
                       ## directory e file
                       prj_dir = NULL,
                       ## filepath e oggetto avanzamento
                       avanz_f = NULL,
                       avanz   = NULL,
                       ## file e oggetto sorgente
                       source_srt_f = NULL,
                       source_srt   = NULL
                   ))
                       









assign_trn <- function(trn_f, assignee){
    ## assegna trn_f = subs_000000.srt a assignee lbraglia

    ## nome nuovo file
    new_f <- paste0(tools::file_path_sans_ext(trn_f), "_", assignee, ".srt")

    ## modifica il nome su disco
    
    ## modifica stato avanzamento e salva su disco
    private$avanz$assign_trn(old = trn_f, new = new_f, assignee = assignee)  
    
}


## ----------
## Translates
## ----------
if (length(translate_users) > 0L){
    ## notify: header
    lbmisc::ascii_header('translate')
    ## check for user permissions
    allowed_users <- check_allowed_users(translate_users, 'translator')
    ## check for unavailable sandboxes
    allowed_users <- check_sandbox(allowed_users, 'translator')
    ## controllo compiti
    allowed_users <- check_homework(allowed_users, 'translator')
    ## determina i file assegnabili sulla base del nome
    assignable_trn <- private$avanz$assignable_trn()
    ## assegnazioni massime sono il minimo tra i file assegnabili e gli
    ## utenti ammessi
    if (length(assignable_trn) > 0L) {
        max_assignments <- min(length(allowed_users), length(assignable_trn))
        assignment_seq <- seq_len(max_assignments)
        ## messaggio per coloro che hanno fatto domanda ma non vi sono più
        ## file da assegnare/tradurre
        if (length(allowed_users) > length(assignable_trn)){
            excluded_users <- allowed_users[- assignment_seq]
            message(
                'per alcuni utenti non vi sono file assegnabili ',
                '(terminati, yee).',
                '\nSono: ',
                paste(excluded_users, collapse = ', '),
                '\n'
            )
        }

        ## TODOHERE sono arrivato qui
        
        ## preparazione path files
        from <- paste(prj_dir, assignable, sep = '/')[assignment_seq]
        new_filenames <- sprintf(
            "%s_%s.srt",
            file_path_sans_ext(assignable[assignment_seq]),
            allowed_users[assignment_seq])
        to <- paste(prj_dir, new_filenames, sep = '/')
        
        Map(file.rename, as.list(from), as.list(to))
        ## notify: file list
        listing(to)
    } else {
        message('Non vi sono file assegnabili: (terminati, yee).')
    }
    
}

## ----------
## Revise
## ----------
if (length(revise_users) > 0L){
    ## notify: header
    ascii_header('revise')
    ## check for user permissions
    allowed_users <- check_allowed_users(revise_users, 'revisor')
    ## check for unavailable sandboxes
    allowed_users <- check_sandbox(allowed_users, 'revisor1')
    ## controllo compiti
    allowed_users <- check_homework(allowed_users, 'revisor')



    ## determina i file assegnabili sulla base del nome
    assignable_ptrn <- "^revs_[[:digit:]]{6}_[[:digit:]]{6}\\.srt$"
    assignable <- list.files(path = prj_dir, pattern = assignable_ptrn)
    ## assegnazioni massime sono il minimo tra i file assegnabili e gli
    ## utenti ammessi
    if (length(assignable) > 0L) {
        max_assignments <- min(length(allowed_users), length(assignable))
        assignment_seq <- seq_len(max_assignments)
        ## messaggio per coloro che hanno fatto domanda ma non vi sono più
        ## file da assegnare/tradurre
        if (length(allowed_users) > length(assignable)){
            excluded_users <- allowed_users[- assignment_seq]
            message(
                'per alcuni utenti non vi sono file assegnabili ',
                '(terminati, yee).',
                '\nSono: ',
                paste(excluded_users, collapse = ', '),
                '\n'
            )
        }
        ## preparazione path files
        from <- paste(prj_dir, assignable, sep = '/')[assignment_seq]
        new_filenames <- sprintf(
            "%s_%s.srt",
            file_path_sans_ext(assignable[assignment_seq]),
            allowed_users[assignment_seq])
        to <- paste(prj_dir, new_filenames, sep = '/')
        Map(file.rename, as.list(from), as.list(to))
        ## notify: file list
        listing(to)
    } else {
        message('Non vi sono file assegnabili: (terminati, yee).')
    }
    
}


