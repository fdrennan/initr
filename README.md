
git init
git add README.md
git commit -m "first commit"
git branch -M master
git remote add origin git@github.com:fdrennan/rinit.git
git push -u origin master

Rscript .rinit.R destroy y
Rscript .rinit.R init
Rscript .rinit.R build
Rscript .rinit.R update
Rscript .rinit.R install
Rscript .rinit.R docker

docker run -it fdrennan/rinit bash

docker build -t rinit --file ./Dockerfile .

http://localhost/__docs__/#/default/get_mtcars


# initr

This is a program in development. This application is philosophically similar to a package.json file in a web application, but for R. 

```
{
  "DESCRIPTION": {
    "package_name": ["rinit"],
    "project_title": ["Automation Tools for Production"],
    "project_description": ["This is so freaking beautiful..."],
    "language": ["es"],
    "version_number": ["0.0.0.9001"],
    "lazy_data": ["false"],
    "AUTHORS": {
      "Freddy": {
        "first_name": ["Freddy"],
        "last_name": ["Drennan"],
        "preferred_name": ["Freddy Ray Drennan"],
        "email": ["drennanfreddy@gmail.com"]
      }
    },
    "license": ["MIT + file LICENCE"]
  },
  "rbase": ["r-base:4.0.2"],
  "make_useful_function": ["true"],
  "roxygen": ["true"],
  "check_name": ["true"],
  "docker_creds": ["fdrennan", "dockerpassword"],
  "docker_working_directory": ["/home/rinit"],
  "github_pat": ["github_personal_access_key_code"],
  "git_protocol": ["ssh"],
  "git_page": ["git@github.com:fdrennan/initr.git"],
  "aws": {
    "access_key": ["access_key_code"],
    "secret_key": ["secret_key_code"],
    "region": ["us-east-2"]
  },
  "services": {
    "plumber": {
      "ports": ["80"]
    }
  },
  "protect": [".rinit.R", 
              ".Renviron", 
              ".Rproj.user", 
              "rinit.Rproj", 
              "main.R",
              ".initr.json", 
              "README.md", 
              ".initr.store"],
              
  "add_to_gitignore": [".Renviron", 
                       ".Rproj.user", 
                       "renv", 
                       ".rinit.R", 
                       "*.env", 
                       ".initr.json", 
                       "builds"],
                       
  "include_in_container": ["R", 
                           "DESCRIPTION", 
                           "NAMESPACE", 
                           "plumber.R", 
                           "renv.lock", 
                           "man",
                           "README.md", 
                           "builds"],
  "in_renv": ["true"],
  "command": ["build"],
  "Rprofile_lock": ["IN_RENV_LOCK"],
  "binaries": ["sudo", 
               "gdebi-core", 
               "pandoc", 
               "pandoc-citeproc", 
               "libcurl4-gnutls-dev", 
               "libcairo2-dev", 
               "libxt-dev", 
               "xtail", 
               "wget", 
               "libssl-dev",
               "libxml2-dev", 
               "python3-venv",
               "libpq-dev", 
               "libsodium-dev", 
               "libudunits2-dev", 
               "libgdal-dev", 
               "systemctl", 
               "git", 
               "libssh2-1", 
               "libssh2-1-dev", 
               "texlive", 
               "unzip",
               "curl"]
}
```# initr
