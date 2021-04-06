## ----------------------------------------------------------------------
## Initialize the project in memory (every time) and setup (at beginning)
## ----------------------------------------------------------------------
initialize <- function(id = NULL, yt_id = NULL){
    if (is.null(id) || is.null(yt_id)) stop("must specify id and yt_id")
    ## id
    private$id      <- id
    private$yt_id   <- yt_id
    ## utenti
    self$users <- users$new(users_f = users_file)
    ## directory e file del progetto
    private$prj_dir  <- sprintf('%s/%s', subs_dir, id)
    private$avanz_f  <- sprintf('%s/%s', private$prj_dir, 'zz_avanzamento.csv')
    private$avanz    <- avanz$new(private$avanz_f, id)
    private$source_srt_f <- sprintf('%s/%s.srt', source_dir, id)
}

## funzione della main class
setup <- function(chunks_len_mins = 5, trn_to_rev_ratio = 6){
    ## import and split source
    private$source_srt <- srt$new(id = 'source',
                                  f = private$source_srt_f)
    if (!dir.exists(private$prj_dir)) dir.create(private$prj_dir)
    chunks <- private$source_srt$split(chunks_len_mins = chunks_len_mins,
                                       yt_id = private$yt_id,
                                       output_dir = private$prj_dir)
    fnames <- unlist(lapply(chunks, function(c) c$fn))
    ## setup monitoring
    private$avanz$setup(trn_filenames = fnames, trn_to_rev_ratio = trn_to_rev_ratio)
}

## --------------
## Create sandbox
## --------------
## importa i login utente github da un file
## f è un path ad un file che include login di github
lines_from_file <- function(f = ''){
    lines <- if (file.exists(f)) readLines(f) else NULL
    lines %without% ''
}

## genera il nome file (srt) di un sandbox in base a login e tipologia
sandbox_file <- function(user, role = c("translator", "revisor1", 'revisor2')){
    role <- match.arg(role)
    postfix <- switch(role,
                      translator = "trn",
                      revisor1 = "rev1",
                      revisor1 = "rev2")
    sprintf('subs/sandbox/%s_%s.srt', user, postfix)
}

## funzione che prende in input dei login github e fa il check che
## siano stati abilitati in data/users.csv (a seconda del permesso
## specificato) restituisce gli utenti abilitati, segnala se ve ne
## sono di non abilitati e interrompe se nessuno è abilitato
check_allowed_users <- function(users, role = c('translator', 'revisor1', 'revisor2')){
    role <- match.arg(role)
    allowed <- if      (role == 'translator') self$users$translators()
               else if (role == 'revisor1')   self$users$revisors1()
               else if (role == 'revisor2')   self$users$revisors2()
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
create_sandbox <- function(sandbox_f = '', rev1_sandbox_f = ''){
    sandbox_users      <- lines_from_file(sandbox_f)
    rev1_sandbox_users <- lines_from_file(rev1_sandbox_f)
    ## sandbox di translators
    if (length(sandbox_users) > 0L) {
        ## notify: header
        lbmisc::ascii_header('sandbox traduttori')
        ## check permissions
        allowed_users <- private$check_allowed_users(sandbox_users, 'translator')
        ## files and dirs
        files <- sandbox_file(allowed_users, role = "translator")
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
        allowed_users <- private$check_allowed_users(rev1_sandbox_users, 'revisor1')
        ## files and dirs
        files <- sandbox_file(allowed_users, role = "revisor1")
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

## funzione che prende in input dei login github, una tipologia di
## utente e verifica che l'utente abbia un sandbox per la
## tipologia considerata
## restituisce gli utenti che hanno una sandbox o si lamenta e
## blocca similmente ad allowed users
check_sandbox <- function(users, role = c('translator', 'revisor1', 'revisor2')){
    
    role <- match.arg(role)
    users_sandbox <- sandbox_file(users, role = role)
    no_sandbox <- users[!file.exists(users_sandbox)]
    if (length(no_sandbox) > 0L){
        msg <- c("Alcuni utenti non hanno ancora il file di sandbox da",
                 role, ".\n ",
                 "Sono: ", paste(no_sandbox, collapse = ', '))
        warning(msg)
    }
    users_with_sandbox <- users %without% no_sandbox
    if (length(users_with_sandbox) == 0L)
        stop("No allowed users for this request")
    users_with_sandbox
}
    
## funzione che dato un progetto controlla che gli utenti di un tipo non
## abbia task non finiti (prima di richiedere) nuove assegnazioni
## da translator o revisor
check_homework <- function(users, role = c("translator", "revisor1", "revisor2")){
    role <- match.arg(role)
    homework_done <- function(u) {
        ## controlla se un singolo utente ha fatto i compiti
        file_non_finiti <-
            private$avanz$unfinished_homeworks(user = u, role = role)

        if (length(file_non_finiti) > 0L) {
            warning(u, ' has unfinished files: ',
                    paste(file_non_finiti, collapse = ' '),
                    '\n  Ignoring his/her request.')
        }
        all_complete <- length(file_non_finiti) == 0L
        all_complete
    }
    allowed_users <- Filter(f = homework_done, users)
    if (length(allowed_users) == 0L) stop("No allowed users for this request")
    allowed_users
}


## main class method
assign <- function(translate_f = '', revise2_f = '')
{
    ## update aggiornamento: salva su disco
    on.exit(private$avanz$to_disk())

    translate_users <- lines_from_file(translate_f)
    revise2_users   <- lines_from_file(revise2_f)

    ## -----
    ## Utils
    ## -----
    assign_worker <- function(old_f, assignee, new_f, from_path, to_path, role){
        ## worker di assegnazione: modifica file su disco e aggiorna avanzamento
        ## modifica il nome su disco
        file.rename(from = from_path, to = to_path)
        ## modifica stato avanzamento e salva su disco
        private$avanz$assign(old_f = old_f, assignee = assignee, new_f = new_f, role = role)  
    }

    excluded_users_message <- function(u)                
        message('per alcuni utenti non vi sono file assegnabili ',
                '(terminati, yee).', '\nSono: ',
                paste(u, collapse = ', '), '\n')

    try_assign <- function(users, role = c('translator', 'revisor2')){
        ## only translator and revisor2 file are assigned by me
        role <- match.arg(role)
        lbmisc::ascii_header(role)
        ## check for user permissions
        allowed_users <- private$check_allowed_users(users, role)
        ## check for unavailable sandboxes
        allowed_users <- check_sandbox(allowed_users, role)
        ## controllo compiti
        allowed_users <- private$check_homework(allowed_users, role)
        ## determina i file assegnabili sulla base del nome
        assignable_files <- private$avanz$assignable_files(role)
        ## assegnazioni massime sono il minimo tra i file assegnabili e gli
        ## utenti ammessi
        if (length(assignable_files) > 0L) {
            max_assignments <- min(length(allowed_users), length(assignable_files))
            assignment_seq <- seq_len(max_assignments)
            ## messaggio per coloro che hanno fatto domanda ma non vi sono più
            ## file da assegnare/tradurre
            if (length(allowed_users) > length(assignable_files)){
                excluded_users <- allowed_users[- assignment_seq]
                excluded_users_message(excluded_users)
            }
            ## accoppiamento e assegnazione
            assigned_files <- assignable_files[assignment_seq]
            assigned_users <- allowed_users[assignment_seq]
            from_path <- private$prj_path(assigned_files)
            new_filenames <- sprintf("%s_%s.srt",
                                     file_path_sans_ext(assigned_files),
                                     assigned_users)
            to_path <- private$prj_path(new_filenames)
            Map(assign_worker,
                as.list(assigned_files),
                as.list(assigned_users),
                as.list(new_filenames),
                as.list(from_path),
                as.list(to_path),
                as.list(role))
            ## notify: file list
            listing(to_path)
        } else {
            message('Non vi sono file assegnabili: (terminati, yee).')
        }
    }
    
    ## assign translate
    ## ----------------
    if (length(translate_users) > 0L) {
        try_assign(translate_users, 'translator')
    }

    ## assign translate
    ## ----------------
    if (length(revise2_users) > 0L) {
        try_assign(translate_users, 'revisor2')
    }
}


## -------------------------------------------------------

get_rev1_user <- function(f) {
    title <- sprintf('Who assigned %s', f)
    users <- private$users$revisors1()
    id <- utils::menu(title = title,
                      choices = users,
                      graphics = TRUE)
    users[id]
}

mark_progresses <- function(trn_completed_f  = '',
                            rev1_started_f   = '',
                            rev1_completed_f = '',
                            rev2_completed_f = '')
{
    ## update aggiornamento: salva su disco
    on.exit(private$avanz$to_disk())

    ## rev1 iniziate
    started_rev1 <- lines_from_file(rev1_started_f)
    rev1_assignee <- unlist(lapply(started_rev1, get_rev1_user))
    Map(private$avanz$mark_as_started,
        as.list(started_rev1),
        as.list(rev1_assignee),
        as.list('revisor1'))
    
    ## trn, rev1 e rev2 completate
    compl_trn  <- lines_from_file(trn_completed_f)
    compl_rev1 <- lines_from_file(rev1_completed_f)
    compl_rev2 <- lines_from_file(rev2_completed_f)
    Map(private$avanz$mark_as_completed,
        as.list(compl_trn), as.list('trn'))
    Map(private$avanz$mark_as_completed,
        as.list(compl_rev1), as.list('rev1'))
    Map(private$avanz$mark_as_completed,
        as.list(compl_rev2), as.list('rev2'))
    
    ## controlla se ci sono file revisione da creare, nel caso fallo e
    ## notifica
    revs_todo <- private$avanz$revs2_todo()
    if (length(revs_todo) > 0){
        revs_todo_paths <- prj_path(revs_todo)
        create_rev <- function(f, path){
            ## obtain trn to cat
            trns <- private$avanz$get_trn_fn_for_rev2(f)
            ## create the file
            cmd <- sprintf("cat %s", paste(trns, collapse = ' '))
            input <- pipe(cmd)
            on.exit(close(input))
            ## file per check con subs editor: parsa proprio l'srt e pulisci
            rev <- srt$new(id = 'rev', f = input)
            rev$write(f = path)
            ## update monitoring
            private$avanz$revs2_created(f)
        }
        Map(create_rev,
            as.list(revs_todo),
            as.list(revs_todo_paths))
        ## Notifica
        ascii_header('Ready for revision check: file da assegnare')
        raw_path <- "https://raw.githubusercontent.com/lbraglia/av_it_subs/main"
        rev_url <- sprintf("%s/%s", raw_path, revs_todo)
        cat("\n\n", rev_url, "\n\n", sep = '\n')
        private$user$mention('revisors2')
    }
}

## -----------------------------------------------------------
## create the final srt after revisions
make_final_srt = function(){
    ## full paths to revision files
    revs <- private$prj_path(private$avanz$filenames("rev2"))
    ## remove BOM by Aegisub
    rm_bom <- paste(c('dos2unix', revs), collapse = ' ')
    system(rm_bom)
    cmd <- sprintf("cat %s", paste(revs, collapse = ' '))
    input <- pipe(cmd)
    on.exit(close(input))
    final_srt <- srt$new(id = 'final_srt', f = input)
    ## exporting
    outfile <- private$prj_path(sprintf("%s_final.srt", private$id))
    final_srt$write(f = outfile)
}


list_assignee <- function(){
    private$avanz$list_assignee()
}

monitoring <- function(){
    private$avanz$monitoring()
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
                       assign = assign,
                       mark_progresses = mark_progresses,
                       make_final_srt = make_final_srt,
                       list_assignee =  list_assignee,
                       monitoring = monitoring
                   ),
                   private = list(
                       id = NULL,
                       yt_id = NULL,
                       ## directory e file
                       prj_dir = NULL,
                       prj_path = function(file) sprintf("%s/%s", private$prj_dir, file),
                       ## filepath e oggetto avanzamento
                       avanz_f = NULL,
                       avanz   = NULL,
                       ## file e oggetto sorgente
                       source_srt_f = NULL,
                       source_srt   = NULL,
                       check_allowed_users = check_allowed_users,
                       check_homework = check_homework
                   ))
