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

    if (dir.exists(private$prj_dir))
        stop(private$prj_dir, " already existing. Aborting.")
    else dir.create(private$prj_dir)

    ## import and split source
    private$source_srt <- srt$new(id = 'source',
                                  f = private$source_srt_f)
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
                      revisor1   = "rev1",
                      revisor2   = "rev2")
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
    ## aggiorna dati in memoria e salva su disco alla fine
    ## private$avanz$from_disk()
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
            ## notify: file listing (trn) or github paths (rev2)
            if (role == 'translator') {
                listing(to_path)
            } else if (role == 'revisor2'){
                gh_path <- "https://raw.githubusercontent.com/lbraglia/av_it_subs/main"
                rev2_urls <- sprintf("%s/%s", gh_path, private$prj_path(to_path))
                listing(rev2_urls)
            }
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
        try_assign(revise2_users, 'revisor2')
    }
}


## -------------------------------------------------------

basename2 <- function(x) {
    if (is.null(x)) NULL
    else basename(x)
}

mark_progresses <- function(trn_completed_f  = '',
                            rev1_started_f   = '',
                            rev1_completed_f = '',
                            rev2_completed_f = '')
{
    ## updates dei dati interni e salvataggio a fine lavoro
    ## private$avanz$from_disk()
    on.exit(private$avanz$to_disk())

    ## rev1 iniziate
    started_rev1 <- basename2(lines_from_file(rev1_started_f))
    if (length(started_rev1) > 0L) {
        get_rev1_user <- function(f) {
            title <- sprintf('Who assigned %s', f)
            users <- self$users$revisors1()
            res <- lbmisc::menu2(title = title, choices = users)
            ## users[id]
            res
        }
        rev1_assignee <- unlist(lapply(started_rev1, get_rev1_user))
        Map(private$avanz$mark_as_started,
            as.list(started_rev1),
            as.list(rev1_assignee),
            as.list('revisor1'))
    }
    
    ## trn, rev1 e rev2 completate
    compl_trn  <- basename2(lines_from_file(trn_completed_f))
    if (length(compl_trn) > 0L){
        Map(private$avanz$mark_as_completed,
            as.list(compl_trn), as.list('trn'))
    }
    ## 
    compl_rev1 <- basename2(lines_from_file(rev1_completed_f))
    if (length(compl_rev1) > 0L){
    Map(private$avanz$mark_as_completed,
        as.list(compl_rev1), as.list('rev1'))
    }
    ## 
    compl_rev2 <- basename2(lines_from_file(rev2_completed_f))
    if (length(compl_rev2) > 0L){
    Map(private$avanz$mark_as_completed,
        as.list(compl_rev2), as.list('rev2'))
    }
    
    ## controlla se ci sono file revisione da creare, nel caso fallo e
    ## notifica
    revs_todo <- private$avanz$revs2_todo()
    if (length(revs_todo) > 0){
        create_rev <- function(f){
            ## obtain trn to cat
            trns <- private$avanz$get_trn_fn_for_rev2(f)
            ## create the file
            cmd <- sprintf("cat %s", paste(private$prj_path(trns), collapse = ' '))
            input <- pipe(cmd)
            on.exit(close(input))
            ## file per check con subs editor: parsa proprio l'srt e pulisci
            rev <- srt$new(id = f, f = input#, validate = FALSE
                           )
            rev$write(f = private$prj_path(f))
            ## update monitoring
            private$avanz$revs2_created(f)
        }
        lapply(create_rev, as.list(revs_todo))
        ## Notifica
        ascii_header('Ready for revision check: file da assegnare')        
        cat("\n\n", private$prj_path(revs_todo), "\n\n", sep = '\n')
        self$users$mention('revisor2')
    }
}

## -----------------------------------------------------------
## create the final srt after revisions
make_final_srt <- function(stats = TRUE){
    ## full paths to revision files
    revs <- private$prj_path(private$avanz$filenames("rev2"))
    ## remove BOM by Aegisub
    rm_bom <- paste(c('dos2unix', revs), collapse = ' ')
    system(rm_bom)
    cmd <- sprintf("cat %s", paste(revs, collapse = ' '))
    input <- pipe(cmd)
    on.exit(close(input))
    final_srt <- srt$new(id = private$id, f = input)
    ## exporting
    outfile <- private$prj_path(sprintf("%s_final.srt", private$id))
    final_srt$write(f = outfile)
    if (stats) final_srt$stats()
}

final_srt_stats <- function(){
    file <- private$prj_path(sprintf("%s_final.srt", private$id))
    final_srt <- srt$new(id = private$id, f = file)
    final_srt$stats()
}

list_assignee <- function(){
    private$avanz$list_assignee()
}

monitoring <- function(){
    private$avanz$monitoring()
}

git_log_analysis <- function(){
    projs <- list.files('subs', full.names = TRUE)
    exclude_dirs <- projs %without% private$prj_dir
    exclude_dirs <- paste0(exclude_dirs, '/')
    exclude_dirs <- paste(sprintf("-x file:'%s'", exclude_dirs),
                          collapse = ' ')
    log_file <- sprintf("/tmp/av_it_subs_%s_log_analysis.html", private$id)
    cmd <- paste("gitinspector  -x author:'Luca Braglia'",
                 exclude_dirs,
                 "-f srt -F html -mrT . >",
                 log_file,
                 " && firefox ",
                 log_file,
                 collapse = ' ')
    cat(c("Executing:", "\n", cmd, '\n'), sep = '')
    system(cmd)
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
                       final_srt_stats = final_srt_stats,
                       list_assignee =  list_assignee,
                       monitoring = monitoring,
                       git_log_analysis = git_log_analysis
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



