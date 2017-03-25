Database generator for statistical yearbook
============================================



Setting up the environment in linux ubuntu 16.04/mint 18
-------------------------------------------------

**System libraries**

```
apt install git -y
apt-get install xclip -y
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 -y
gpg -a --export E084DAB9 | apt-key add -
apt-get update
apt-get install r-base r-base-dev -y
apt-get install r-cran-littler
apt-get install libcurl4-openssl-dev libxml2-dev libssl-dev libfreetype6-dev libcairo-dev -y
apt-get install libgdal1-dev libproj-dev -y
apt install libssl-dev
apt install libssh2-1-dev
apt install libcurl4-openssl-dev
apt install libxml2-dev
apt install texlive-full -y
```

**install R-packages in console**

```
R -e 'install.packages("devtools", repos = "https://cloud.r-project.org")' 
R -e 'devtools::install_github("unfaostatistics/gisfao")' 
R -e 'devtools::install_github("rstudio/flexdashboard")' 
R -e 'devtools::install_github("rstudio/rmarkdown")' 
```

**Open R and run the following code**

```
PACKAGES <- scan(url("http://muuankarski.kapsi.fi/luntti/r-paketit.txt"), what="character")
inst <- match(PACKAGES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PACKAGES[need])
```

**Download and install Rstudio**

- <https://www.rstudio.com/products/rstudio/download/>

**Open Rstudio**

1. Create folder `~/faosync/pocketbooks/`
2. start two new projects from version control (git) into directory above with following urls
3. pocketbook_database: `https://github.com/UNFAOstatistics/pocketbook_database´
4. pocketbook: `https://github.com/UNFAOstatistics/pocketbook´
4. edit ´run.R` in pocketbook_database and start executing it row by row
