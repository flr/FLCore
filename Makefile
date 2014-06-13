PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

R_FILES := $(wildcard $(PKG)/R/*.R)
HELP_FILES := $(wildcard $(PKG)/man/*.Rd)

all: news readme staticdocs build

readme: DESCRIPTION
	R --vanilla --silent -e "library(utils);" \
  -e "desc <- read.dcf('DESCRIPTION', fields = c('Version', 'Date'));" \
  -e "str <- readLines('README.md');" \
  -e "ln <- grep('Version:', str);" \
  -e "str[ln[1]] <- sub('Version: .*', paste('Version:', desc[,'Version']), str[ln]);" \
  -e "ln <- grep('Date:', str);" \
  -e "str[ln[1]] <- sub('Date: .*', paste('Date:', desc[,'Date']), str[ln[1]]);" \
  -e "writeLines(str, con = 'README.md')"

staticdocs: $(HELP_FILES)
	R --vanilla --silent -e "library(staticdocs);" \
  -e "build_site('../FLCore/', site_path='gh-pages', launch=FALSE)"; \
	rm Rplots.pdf

news: NEWS.md
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^#/\t\t/' <NEWS.md | fmt -80 >NEWS

roxygen: $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document()"

build:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

clean:
	rm -r gh-pages;\
	cd ..;\
	rm -r $(PKGNAME).Rcheck;\
