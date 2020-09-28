## usethis policy re: preparation of user-provided path to a resource on user's
## file system
library(fs)
library(xfun)
library(usethis)


tryCatch(expr = {
  library(jsonlite, quietly = TRUE)
  library(cli, quietly = TRUE)
  library(glue, quietly = TRUE)
  library(readr)
}, error = function(err) {
  stop("You must install jsonlite, cli, glue, and readr\ninstall.packages(c('jsonlite', 'cli', 'glue', 'readr'))")
})


initr_config <- fromJSON('.initr.json')


style_reset()
if (interactive()) {
  COMMAND <- 'docker'
  COMMAND <- tolower(COMMAND)
  if (COMMAND == 'destroy') {
    DESTROY = TRUE
  }
  COMMAND 
} else {
  args <- commandArgs(trailingOnly = TRUE)
  COMMAND <- args[1]
  COMMAND <- tolower(COMMAND)
  if (COMMAND == 'destroy') {
    if(args[2] == 'y') DESTROY = TRUE else DESTROY = FALSE
  }
}
available_commands <- c('init', 'destroy', 'install', 'update', 'docker')
not_acceptable_command <- !COMMAND %in% available_commands
if (not_acceptable_command) {
  stop(cli_alert_warning(glue('{COMMAND} is not an acceptable argument\nTo get started, run Rscript .initr.R init')))
}

style_inverse(glue('.initr is running COMMAND {style_italic(COMMAND) }'))

# Check to see if we can install packages and work with user system.
if (exists('IN_RENV_LOCK')) {
  if (!IN_RENV_LOCK == TRUE) {
      IN_RENV_LOCK = FALSE
  } else {
    IN_RENV_LOCK = FALSE
  }
} else {
  IN_RENV_LOCK = FALSE
}

# If in RENV lock, then install necessary packages.
installed_packages <- installed.packages()[,1]
if (IN_RENV_LOCK & (COMMAND != 'destroy')) {
  library(renv)
  
  resp <- lapply(c('fs', 'jsonlite', 'devtools', 'roxygen2', 'glue', 'stevedore', 'usethis', 'plumber', 'tictoc'), function(pkg) {
    if (!pkg %in% installed_packages) {
      message(paste('.initr is installing', pkg))
      install(pkg, dependencies = TRUE)
      message(paste('initr installed:', pkg))
      pkg
    } else {
      
    }
  })
}

if (COMMAND == "destroy") {

  if (!DESTROY) {
    stop('Termination destruction process')
  }

  if (IN_RENV_LOCK) {
    lapply(c('fs'), function(pkg) {
      if (!pkg %in% installed_packages[,1]) {
        install.packages(pkg, dependencies = TRUE)
      }
    })
  }

  library(fs, quietly = TRUE)

  keep_these_paths <- file.path('.', initr_config$protect)
  current_directory_files <- dir_ls(all = TRUE)
  files_to_delete <- current_directory_files[!current_directory_files %in% initr_config$protect]
  if (length(files_to_delete) > 0) {
    lapply(files_to_delete, function(file_path) {
      if (is_dir(file_path))
        dir_delete(path = file_path) else file_delete(path = file_path)
    })
  }
  as.logical(initr_config$in_renv)
  initr_config$in_renv = 'false'
  write_json(initr_config, '.initr.json', pretty=TRUE)
  message('IN_RENV_LOCK IS FALSE')
  message("Please restart your R session")
  message("Then Run --> Rscript .initr.R build <-- ")
}

if (COMMAND == 'update') {
  library(git2r)
  library(usethis)
  library(renv)
  library(fs)
  settings$snapshot.type("all")
  dependencies()
  snapshot(prompt=FALSE, force=TRUE)
  resp <- lapply(initr_config$add_to_dockerignore, function(x) write_union('.Dockerignore', x))
  resp <- lapply(initr_config$add_to_gitignore, function(x) write_union('.gitignore', x))
}

if (COMMAND == "init") {
  if (!IN_RENV_LOCK) {
    library(fs, quietly = TRUE)
    library(renv, quietly = TRUE)
    write(
      'IN_RENV_LOCK = TRUE; paste("IN_RENV_LOCK", IN_RENV_LOCK);',
      file='.Rprofile',
      append=TRUE
    )
    write(
      "message('beep boop .init initialized - You can delete this message in .Rprofile')",
      file='.Rprofile',
      append=TRUE
    )
    message('')
    consent(provided = TRUE)
    init(bare = TRUE)
    initr_config$in_renv = 'true'
    install(c('fs', 'glue', 'devtools', 'plumber'))
    # write(x = paste(c("apt-get update --allow-releaseinfo-change -qq && apt-get install -y", initr_config$binaries), collapse = ' \\\n\t'), '.binaries.sh')
    write_json(initr_config, '.initr.json', pretty=TRUE)
  } else {
    message('.init already initted')
  }
}

if (COMMAND == "build") {

  library(fs)

    if (!IN_RENV_LOCK) {
      stop('You cannot run this command ourside of IN_RENV_LOCK')
    }

    message(".initr is going to build your package now")
    message("Here is what is going in. \nIf you dont like it, you can run in your terminal\n--> Rscript .initr.R destroy y <--")
    toJSON(initr_config$DESCRIPTION, pretty = TRUE)
    library(devtools, quietly = TRUE)
    library(glue, quietly = TRUE)
    create_package(
      path = getwd(),
      fields = list(
        Package = initr_config$DESCRIPTION$package_name,
        Version = initr_config$DESCRIPTION$version_number,
        Author = initr_config$DESCRIPTION$AUTHORS[[1]]$preferred_name,
        Maintainer = paste(initr_config$DESCRIPTION$AUTHORS[[1]]$preferred_name, glue('<{initr_config$DESCRIPTION$AUTHORS[[1]]$email}}>')),
        Language = "es"
      ),
      rstudio = rstudioapi::isAvailable(),
      roxygen = as.logical(initr_config$roxygen),
      check_name = as.logical(initr_config$check_name),
      open = FALSE
    )

    file_delete('DESCRIPTION')

    use_description(
      fields = list(
        Title = initr_config$DESCRIPTION$project_title,
        Description = initr_config$DESCRIPTION$project_description,
        Language = initr_config$DESCRIPTION$language,
        License = initr_config$DESCRIPTION$license,
        LazyData = initr_config$DESCRIPTION$lazy_data
      )
    )

    make_useful <- as.logical(initr_config$make_useful_function)
    if (make_useful) {
      message('.initr is making you a useful function in R')
      file_contents <- glue::glue("#' glue_message\n#' @importFrom glue glue\n#' @param string A string\n#' @export glue_message\nglue_message <- function(string, ...) message(glue(string, ...))")
      sink(file = "R/useful.R")
      print(file_contents)
      sink()
    }

    library(git2r)
    resp <- lapply(initr_config$add_to_gitignore, use_git_ignore)

    # init(path = getwd())

    if (initr_config$git_protocol == 'ssh') {
      message('.initr setting --> options(usethis.protocol = "ssh")')
      options(usethis.protocol = "ssh")
    } else {
      message(glue('initr_config$git_protocol: {initr_config$git_protocol}\n Defaulting to https'))
      options(usethis.protocol = "https")
    }
    
    message('You must go create a repo and and set "git_page": ["git@github.com:your_thing/project.git"]')

    message("
            AS WELL, YOU NEED TO RUN YOUR GIT UP ON GITHUB

            MY COMMANDS COMMANDS TO DO THIS WERE AS FOLLOWS, BUT YOURS WILL BE DIFFERENT
            --------------------------------------------------------
            git init
            git add README.md
            git commit -m \"first commit\"
            git branch -M master
            git remote add origin git@github.com:fdrennan/initr.git
            git push -u origin master
            --------------------------------------------------------
            ")

}

if (COMMAND == "install") {
    library(devtools)
    library(fs)
    library(renv)
    if(!dir_exists('builds')) {
      dir_create('builds')
    }
    install()
    snapshot(prompt=FALSE, force=TRUE)
    document(pkg = getwd())
    build(
      pkg = ".",
      path = './builds',
      binary = TRUE,
      vignettes = TRUE
    )
    library(initr_config$DESCRIPTION$package_name, character.only = TRUE, quietly = TRUE)
}

if (COMMAND == "docker") {
    library(stevedore)
    library(renv)
    
    if (!docker_available()) {
        stop("Docker is not running")
    } else {
      
      

        {
          library(purrr, quietly = TRUE, warn.conflicts = FALSE)
          library(glue, quietly = TRUE, warn.conflicts = FALSE)
          library(fs, quietly = TRUE, warn.conflicts = FALSE)
        }
      
      # Build Dockerfile
      tryCatch(expr = file.remove('Dockerfile'), error = function(err) {
        message('Starting Fresh')
      })
      write(x = glue("FROM {initr_config$rbase}\n\n"), file = 'Dockerfile', append = TRUE)
      write(x = glue("RUN ls -lah\n\n"), file = 'Dockerfile', append = TRUE)
      write(x = glue("RUN whoami\n\n"), file = 'Dockerfile', append = TRUE)
      # write(x = glue("WORKDIR {initr_config$docker_working_directory}\n\n"), file = 'Dockerfile', append = TRUE)
      write(x = paste(c("RUN apt-get update --allow-releaseinfo-change -qq && apt-get install -y", initr_config$binaries), collapse = '\\\n\t'), 'Dockerfile', append = TRUE)
      walk(
        initr_config$include_in_container,
        ~ write(x = glue("\n\nCOPY {.} {.}"), file = 'Dockerfile', append = TRUE)
      )
      write(x = "\nRUN R -e \"install.packages('renv')\"", 'Dockerfile', append = TRUE)
      write(x = "\nRUN R -e \"library(renv);consent(provided=T);activate();restore(clean=TRUE, prompt=FALSE)\"", 'Dockerfile', append = TRUE)
      write(x = 'ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host=\'0.0.0.0\', port=8000)"]', 'Dockerfile', append = TRUE)
      write(x = 'CMD ["/plumber.R"]', 'Dockerfile', append = TRUE)
      
      message('runing --> docker-compose build <--')
      system('docker-compose build')
      message('you run --> docker-compose up <--')
      # system('docker-compose build')
      
  
      # system('docker build -t initr --file ./Dockerfile .')
      # docker build -t initr --file ./Dockerfile .
      
       
    }
}

if (COMMAND == "tidy") {
  message('tidy Currently Off')
    # # Rscript .initr.R docker
    # library(formatR)
    # tidy_dir("R")
    # tidy_file(".initr.R")
}

if (COMMAND == 'prettify') {
  library(readr)
  library(jsonlite)
  initr_config <- prettify(read_file('.initr.json'))
  write_file(initr_config, '.initr.json')
}


