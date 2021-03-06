FROM r-base:4.0.2

RUN ls -lah

RUN whoami

RUN apt-get update --allow-releaseinfo-change -qq && apt-get install -y\
	sudo\
	gdebi-core\
	pandoc\
	pandoc-citeproc\
	libcurl4-gnutls-dev\
	libcairo2-dev\
	libxt-dev\
	xtail\
	wget\
	libssl-dev\
	libxml2-dev\
	python3-venv\
	libpq-dev\
	libsodium-dev\
	libudunits2-dev\
	libgdal-dev\
	systemctl\
	git\
	libssh2-1\
	libssh2-1-dev\
	texlive\
	unzip\
	curl

COPY R R

COPY DESCRIPTION DESCRIPTION

COPY NAMESPACE NAMESPACE

COPY plumber.R plumber.R

COPY renv.lock renv.lock

COPY man man

COPY README.md README.md

COPY builds builds

RUN R -e "install.packages('renv')"

RUN R -e "library(renv);consent(provided=T);activate();restore(clean=TRUE, prompt=FALSE)"
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/plumber.R"]
