PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE := $(shell sed -n "s/Date: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

GITDATE=$(shell (git log -1 --date=short --pretty=format:"%ad"))
GITVERS=$(shell (date -d `git log -1 --date=short --pretty=format:"%ad"` +%Y%m%d))

R_FILES := $(wildcard $(PKGSRC)/R/*.R)
HELP_FILES := $(wildcard $(PKGSRC)/man/*.Rd)

all: NEWS README.md roxygen build

README.md: DESCRIPTION
	sed -i 's/Version: *\([^ ]*\)/Version: $(PKGVERS)/' README.md
	sed -i 's/Date: *\([^ ]*\)/Date: $(PKGDATE)/' README.md

gh-pages: $(HELP_FILES) README.md
	R --vanilla --silent -e "library(staticdocs);" \
  -e "build_site('../$(PKGNAME)/', site_path='gh-pages', launch=FALSE)"; \
	rm Rplots.pdf  
	git subtree push --prefix gh-pages origin gh-pages
	# git push origin `git subtree split --prefix gh-pages master`:gh-pages --force

NEWS: NEWS.md
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^#/\t\t/' <NEWS.md | fmt -80 >NEWS

roxygen: $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

update:
	sed -i 's/Version: \([0-9]\.[0-9]*\.\)\([^ ]*\)/Version: \1$(GITVERS)/' DESCRIPTION
	sed -i 's/Date: *\([^ ]*\)/Date: $(GITDATE)/' DESCRIPTION

build:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: NEWS README.md roxygen build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

clean:
	cd ..;\
	rm -rf $(PKGNAME).Rcheck $(PKGNAME)_$(PKGVERS).tar.gz
