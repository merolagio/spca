## Resubmission
This is a resubmission. In this version I have:

* removed the LICENCE file which was unnecessary

* renamed all the knitr chunks to names without blank spaces. This likely caused a problem when building the package on Linux

* amended some documentation files

The package passed the `R CMD check --as-cran` in Rstudio on Windows 7.

The package passed the `R CMD check --as-cran` also on Ubuntu 12. 
The Travis (https://travis-ci.org/merolagio/spca/builds/44067354) log is below.

Using worker: worker-linux-6-2.bb.travis-ci.org:travis-linux-16
system_info
Build system information
Build language: c
Build image provisioning date and time
Sun Dec  7 06:19:19 UTC 2014
lsb_release -a
Distributor ID:	Ubuntu
Description:	Ubuntu 12.04 LTS
Release:	12.04
Codename:	precise
Cookbooks Version
5736160 https://github.com/travis-ci/travis-cookbooks/tree/5736160
GCC version
gcc (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3
Copyright (C) 2011 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
LLVM version
clang version 3.4 (tags/RELEASE_34/final)
Target: x86_64-unknown-linux-gnu
Thread model: posix
Pre-installed Ruby versions
jruby-1.7.16.1-d18
jruby-1.7.16.1-d19
ree-1.8.7-2012.02
ruby-1.8.7-head
ruby-1.8.7-p374
ruby-1.9.2-p330
ruby-1.9.3-p551
ruby-2.0.0-p598
ruby-2.1.2
ruby-2.1.3
ruby-2.1.4
ruby-2.1.5
ruby-2.2.0-preview2
Pre-installed Node.js versions
v0.10.33
Pre-installed Go versions
go1.3.3
Redis version
redis-server 2.8.18
riak version
1.4.12
MongoDB version
MongoDB 2.4.12
CouchDB version
couchdb 1.6.1
Neo4j version
1.9.4
Cassandra version
2.0.9
RabbitMQ Version
3.4.2
ElasticSearch version
1.4.0
Installed Sphinx versions
2.0.10
2.1.9
2.2.4
Default Sphinx version
2.1.9
Installed Firefox version
firefox 31.0esr
PhantomJS version
1.9.8
ant -version
Apache Ant(TM) version 1.8.2 compiled on December 3 2011
mvn -version
Apache Maven 3.2.3 (33f8c3e1027c3ddde99d3cdebad2656a31e8fdf4; 2014-08-11T20:58:10+00:00)
Maven home: /usr/local/maven
Java version: 1.7.0_72, vendor: Oracle Corporation
Java home: /usr/lib/jvm/java-7-oracle/jre
Default locale: en_US, platform encoding: UTF-8
OS name: "linux", version: "2.6.32-042stab090.5", arch: "amd64", family: "unix"
git.checkout
0.14s$ git clone --depth=50 --branch=master git://github.com/merolagio/spca.git merolagio/spca
Cloning into 'merolagio/spca'...
remote: Counting objects: 164, done.
remote: Compressing objects: 100% (105/105), done.
remote: Total 164 (delta 66), reused 142 (delta 51)
Receiving objects: 100% (164/164), 603.71 KiB | 0 bytes/s, done.
Resolving deltas: 100% (66/66), done.
Checking connectivity... done.
$ cd merolagio/spca
$ git checkout -qf 5047d99a8e809df77590cdf78e781de93bea38fa
Setting environment variables from .travis.yml
$ export CRAN=http://cran.rstudio.com
$ export R_BUILD_ARGS="--no-manual"
$ export R_CHECK_ARGS="--no-manual --as-cran"
$ export CC=gcc
$ gcc --version
gcc (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3
Copyright (C) 2011 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
before_install.1
0.14s$ curl -OL http://raw.github.com/craigcitro/r-travis/master/scripts/travis-tool.sh
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
100 11391  100 11391    0     0  93127      0 --:--:-- --:--:-- --:--:-- 93127
before_install.2
0.01s$ chmod 755 ./travis-tool.sh
before_install.3
101.68s$ ./travis-tool.sh bootstrap
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=bootstrap
+echo 'Running command: bootstrap'
Running command: bootstrap
+shift
+case $COMMAND in
+Bootstrap
+[[ Darwin == \L\i\n\u\x ]]
+[[ Linux == \L\i\n\u\x ]]
+BootstrapLinux
++lsb_release -cs
+sudo add-apt-repository 'deb http://cran.rstudio.com/bin/linux/ubuntu precise/'
+sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
Executing: gpg --ignore-time-conflict --no-options --no-default-keyring --secret-keyring /tmp/tmp.kJ5QBB62x6 --trustdb-name /etc/apt/trustdb.gpg --keyring /etc/apt/trusted.gpg --primary-keyring /etc/apt/trusted.gpg --keyring /etc/apt/trusted.gpg.d//apt.postgresql.org.gpg --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
gpg: requesting key E084DAB9 from hkp server keyserver.ubuntu.com
gpg: key E084DAB9: public key "Michael Rutter <marutter@gmail.com>" imported
gpg: Total number processed: 1
gpg:               imported: 1  (RSA: 1)
+sudo add-apt-repository -y ppa:marutter/rrutter
Executing: gpg --ignore-time-conflict --no-options --no-default-keyring --secret-keyring /tmp/tmp.Dp4pIsUvQ7 --trustdb-name /etc/apt/trustdb.gpg --keyring /etc/apt/trusted.gpg --primary-keyring /etc/apt/trusted.gpg --keyring /etc/apt/trusted.gpg.d//apt.postgresql.org.gpg --keyserver hkp://keyserver.ubuntu.com:80/ --recv C9A7585B49D51698710F3A115E25F516B04C661B
gpg: requesting key B04C661B from hkp server keyserver.ubuntu.com
gpg: key B04C661B: public key "Launchpad PPA for marutter" imported
gpg: Total number processed: 1
gpg:               imported: 1  (RSA: 1)
+sudo add-apt-repository -y ppa:marutter/c2d4u
Executing: gpg --ignore-time-conflict --no-options --no-default-keyring --secret-keyring /tmp/tmp.0hQXE8Kyyr --trustdb-name /etc/apt/trustdb.gpg --keyring /etc/apt/trusted.gpg --primary-keyring /etc/apt/trusted.gpg --keyring /etc/apt/trusted.gpg.d//apt.postgresql.org.gpg --keyserver hkp://keyserver.ubuntu.com:80/ --recv C9A7585B49D51698710F3A115E25F516B04C661B
gpg: requesting key B04C661B from hkp server keyserver.ubuntu.com
gpg: key B04C661B: "Launchpad PPA for marutter" not changed
gpg: Total number processed: 1
gpg:              unchanged: 1
+Retry sudo apt-get update -qq
+sudo apt-get update -qq
+return 0
+Retry sudo apt-get install --no-install-recommends r-base-dev r-recommended qpdf
+sudo apt-get install --no-install-recommends r-base-dev r-recommended qpdf
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following package was automatically installed and is no longer required:
  libgeos-3.2.2
Use 'apt-get autoremove' to remove them.
The following extra packages will be installed:
  cdbs dh-translations gfortran gfortran-4.6 intltool libblas-dev
  libencode-locale-perl libfile-listing-perl libhtml-parser-perl
  libhtml-tagset-perl libhtml-tree-perl libhttp-cookies-perl libhttp-date-perl
  libhttp-message-perl libhttp-negotiate-perl libio-socket-ssl-perl
  liblapack-dev liblwp-mediatypes-perl liblwp-protocol-https-perl liblzma-dev
  libnet-http-perl libnet-ssleay-perl libqpdf3 liburi-perl libwww-perl
  libwww-robotrules-perl libxml-parser-perl python-scour r-base-core
  r-cran-boot r-cran-class r-cran-cluster r-cran-codetools r-cran-foreign
  r-cran-kernsmooth r-cran-lattice r-cran-mass r-cran-matrix r-cran-mgcv
  r-cran-nlme r-cran-nnet r-cran-rpart r-cran-spatial r-cran-survival tcl8.5
  tk8.5
Suggested packages:
  devscripts gfortran-multilib gfortran-doc gfortran-4.6-multilib
  gfortran-4.6-doc libgfortran3-dbg libdata-dump-perl libcrypt-ssleay-perl
  liblzma-doc libauthen-ntlm-perl python-rsvg python-cairo ess r-doc-info
  r-doc-pdf r-mathlib r-base-html texlive-base texlive-latex-base
  texlive-generic-recommended texlive-fonts-recommended texlive-fonts-extra
  texlive-extra-utils texlive-latex-recommended texlive-latex-extra texinfo
  tclreadline
Recommended packages:
  libio-socket-inet6-perl libhtml-form-perl libhtml-format-perl
  libhttp-daemon-perl libmailtools-perl r-doc-html xterm x-terminal-emulator
The following NEW packages will be installed:
  cdbs dh-translations gfortran gfortran-4.6 intltool libblas-dev
  libencode-locale-perl libfile-listing-perl libhtml-parser-perl
  libhtml-tagset-perl libhtml-tree-perl libhttp-cookies-perl libhttp-date-perl
  libhttp-message-perl libhttp-negotiate-perl libio-socket-ssl-perl
  liblapack-dev liblwp-mediatypes-perl liblwp-protocol-https-perl liblzma-dev
  libnet-http-perl libnet-ssleay-perl libqpdf3 liburi-perl libwww-perl
  libwww-robotrules-perl libxml-parser-perl python-scour qpdf r-base-core
  r-base-dev r-cran-boot r-cran-class r-cran-cluster r-cran-codetools
  r-cran-foreign r-cran-kernsmooth r-cran-lattice r-cran-mass r-cran-matrix
  r-cran-mgcv r-cran-nlme r-cran-nnet r-cran-rpart r-cran-spatial
  r-cran-survival r-recommended tcl8.5 tk8.5
0 upgraded, 49 newly installed, 0 to remove and 128 not upgraded.
Need to get 53.2 MB of archives.
After this operation, 97.9 MB of additional disk space will be used.
Get:1 http://us.archive.ubuntu.com/ubuntu/ precise/main liburi-perl all 1.59-1 [90.3 kB]
Get:2 http://us.archive.ubuntu.com/ubuntu/ precise/main libencode-locale-perl all 1.02-2 [12.0 kB]
Get:3 http://us.archive.ubuntu.com/ubuntu/ precise/main libhttp-date-perl all 6.00-1 [10.1 kB]
Get:4 http://us.archive.ubuntu.com/ubuntu/ precise/main libfile-listing-perl all 6.03-1 [9,690 B]
Get:5 http://us.archive.ubuntu.com/ubuntu/ precise/main libhtml-tagset-perl all 3.20-2 [13.5 kB]
Get:6 http://us.archive.ubuntu.com/ubuntu/ precise/main libhtml-parser-perl amd64 3.69-1build1 [97.3 kB]
Get:7 http://us.archive.ubuntu.com/ubuntu/ precise/main libhtml-tree-perl all 4.2-1 [205 kB]
Get:8 http://us.archive.ubuntu.com/ubuntu/ precise/main liblwp-mediatypes-perl all 6.01-1 [17.5 kB]
Get:9 http://us.archive.ubuntu.com/ubuntu/ precise/main libhttp-message-perl all 6.01-1 [77.6 kB]
Get:10 http://us.archive.ubuntu.com/ubuntu/ precise/main libhttp-cookies-perl all 6.00-2 [23.3 kB]
Get:11 http://us.archive.ubuntu.com/ubuntu/ precise/main libhttp-negotiate-perl all 6.00-2 [13.4 kB]
Get:12 http://us.archive.ubuntu.com/ubuntu/ precise/main libnet-http-perl all 6.02-1 [23.5 kB]
Get:13 http://us.archive.ubuntu.com/ubuntu/ precise/main libnet-ssleay-perl amd64 1.42-1build1 [188 kB]
Get:14 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-base-core 3.1.2-1precise0 [22.4 MB]
Get:15 http://us.archive.ubuntu.com/ubuntu/ precise/main libio-socket-ssl-perl all 1.53-1 [47.5 kB]
Get:16 http://us.archive.ubuntu.com/ubuntu/ precise/main liblwp-protocol-https-perl all 6.02-1 [6,746 B]
Get:17 http://us.archive.ubuntu.com/ubuntu/ precise/main libwww-robotrules-perl all 6.01-1 [14.1 kB]
Get:18 http://us.archive.ubuntu.com/ubuntu/ precise/main libwww-perl all 6.03-1 [156 kB]
Get:19 http://us.archive.ubuntu.com/ubuntu/ precise/main libxml-parser-perl amd64 2.41-1build1 [265 kB]
Get:20 http://us.archive.ubuntu.com/ubuntu/ precise/main intltool all 0.50.2-2 [52.0 kB]
Get:21 http://us.archive.ubuntu.com/ubuntu/ precise/main dh-translations all 116 [21.6 kB]
Get:22 http://us.archive.ubuntu.com/ubuntu/ precise/main python-scour all 0.26-3 [46.5 kB]
Get:23 http://us.archive.ubuntu.com/ubuntu/ precise/main cdbs all 0.4.100ubuntu2 [47.6 kB]
Get:24 http://us.archive.ubuntu.com/ubuntu/ precise/main gfortran-4.6 amd64 4.6.3-1ubuntu5 [5,579 kB]
Get:25 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-spatial amd64 7.3-8-1cran1ppa0 [125 kB]
Get:26 http://us.archive.ubuntu.com/ubuntu/ precise/main gfortran amd64 4:4.6.3-1ubuntu5 [1,206 B]
Get:27 http://us.archive.ubuntu.com/ubuntu/ precise/main libblas-dev amd64 1.2.20110419-2ubuntu1 [302 kB]
Get:28 http://us.archive.ubuntu.com/ubuntu/ precise/main liblapack-dev amd64 3.3.1-1 [4,873 kB]
Get:29 http://us.archive.ubuntu.com/ubuntu/ precise/universe libqpdf3 amd64 2.3.1-1 [269 kB]
Get:30 http://us.archive.ubuntu.com/ubuntu/ precise/universe qpdf amd64 2.3.1-1 [147 kB]
Get:31 http://us.archive.ubuntu.com/ubuntu/ precise/main tcl8.5 amd64 8.5.11-1ubuntu1 [1,098 kB]
Get:32 http://us.archive.ubuntu.com/ubuntu/ precise/main tk8.5 amd64 8.5.11-1 [1,003 kB]
Get:33 http://us.archive.ubuntu.com/ubuntu/ precise/main liblzma-dev amd64 5.1.1alpha+20110809-3 [98.4 kB]
Get:34 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-base-dev 3.1.2-1precise0 [3,902 B]
Get:35 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-boot 1.3-13-1precise0 [578 kB]
Get:36 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-mass 7.3-35-1precise0 [1,041 kB]
Get:37 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-class 7.3-11-1precise0 [83.7 kB]
Get:38 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-cluster 1.15.3-1precise0 [481 kB]
Get:39 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-codetools 0.2-9-1precise0 [46.5 kB]
Get:40 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-foreign 0.8.61-1precise0 [230 kB]
Get:41 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-kernsmooth 2.23-13-1precise0 [84.0 kB]
Get:42 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-lattice 0.20-29-1precise0 [725 kB]
Get:43 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-matrix 1.1-4-1precise0 [3,229 kB]
Get:44 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-nlme 3.1.118-1precise0 [2,075 kB]
Get:45 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-mgcv 1.8-4-1precise0 [1,796 kB]
Get:46 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-nnet 7.3-8-1precise0 [99.3 kB]
Get:47 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-survival 2.37-7-1precise0 [4,521 kB]
Get:48 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-cran-rpart 4.1-8-1precise0 [878 kB]
Get:49 http://cran.rstudio.com/bin/linux/ubuntu/ precise/ r-recommended 3.1.2-1precise0 [2,670 B]
Fetched 53.2 MB in 14s (3,606 kB/s)
Extracting templates from packages: 100%
Selecting previously unselected package liburi-perl.
(Reading database ... 70810 files and directories currently installed.)
Unpacking liburi-perl (from .../liburi-perl_1.59-1_all.deb) ...
Selecting previously unselected package libencode-locale-perl.
Unpacking libencode-locale-perl (from .../libencode-locale-perl_1.02-2_all.deb) ...
Selecting previously unselected package libhttp-date-perl.
Unpacking libhttp-date-perl (from .../libhttp-date-perl_6.00-1_all.deb) ...
Selecting previously unselected package libfile-listing-perl.
Unpacking libfile-listing-perl (from .../libfile-listing-perl_6.03-1_all.deb) ...
Selecting previously unselected package libhtml-tagset-perl.
Unpacking libhtml-tagset-perl (from .../libhtml-tagset-perl_3.20-2_all.deb) ...
Selecting previously unselected package libhtml-parser-perl.
Unpacking libhtml-parser-perl (from .../libhtml-parser-perl_3.69-1build1_amd64.deb) ...
Selecting previously unselected package libhtml-tree-perl.
Unpacking libhtml-tree-perl (from .../libhtml-tree-perl_4.2-1_all.deb) ...
Selecting previously unselected package liblwp-mediatypes-perl.
Unpacking liblwp-mediatypes-perl (from .../liblwp-mediatypes-perl_6.01-1_all.deb) ...
Selecting previously unselected package libhttp-message-perl.
Unpacking libhttp-message-perl (from .../libhttp-message-perl_6.01-1_all.deb) ...
Selecting previously unselected package libhttp-cookies-perl.
Unpacking libhttp-cookies-perl (from .../libhttp-cookies-perl_6.00-2_all.deb) ...
Selecting previously unselected package libhttp-negotiate-perl.
Unpacking libhttp-negotiate-perl (from .../libhttp-negotiate-perl_6.00-2_all.deb) ...
Selecting previously unselected package libnet-http-perl.
Unpacking libnet-http-perl (from .../libnet-http-perl_6.02-1_all.deb) ...
Selecting previously unselected package libnet-ssleay-perl.
Unpacking libnet-ssleay-perl (from .../libnet-ssleay-perl_1.42-1build1_amd64.deb) ...
Selecting previously unselected package libio-socket-ssl-perl.
Unpacking libio-socket-ssl-perl (from .../libio-socket-ssl-perl_1.53-1_all.deb) ...
Selecting previously unselected package liblwp-protocol-https-perl.
Unpacking liblwp-protocol-https-perl (from .../liblwp-protocol-https-perl_6.02-1_all.deb) ...
Selecting previously unselected package libwww-robotrules-perl.
Unpacking libwww-robotrules-perl (from .../libwww-robotrules-perl_6.01-1_all.deb) ...
Selecting previously unselected package libwww-perl.
Unpacking libwww-perl (from .../libwww-perl_6.03-1_all.deb) ...
Selecting previously unselected package libxml-parser-perl.
Unpacking libxml-parser-perl (from .../libxml-parser-perl_2.41-1build1_amd64.deb) ...
Selecting previously unselected package intltool.
Unpacking intltool (from .../intltool_0.50.2-2_all.deb) ...
Selecting previously unselected package dh-translations.
Unpacking dh-translations (from .../dh-translations_116_all.deb) ...
Selecting previously unselected package python-scour.
Unpacking python-scour (from .../python-scour_0.26-3_all.deb) ...
Selecting previously unselected package cdbs.
Unpacking cdbs (from .../cdbs_0.4.100ubuntu2_all.deb) ...
Selecting previously unselected package gfortran-4.6.
Unpacking gfortran-4.6 (from .../gfortran-4.6_4.6.3-1ubuntu5_amd64.deb) ...
Selecting previously unselected package gfortran.
Unpacking gfortran (from .../gfortran_4%3a4.6.3-1ubuntu5_amd64.deb) ...
Selecting previously unselected package libblas-dev.
Unpacking libblas-dev (from .../libblas-dev_1.2.20110419-2ubuntu1_amd64.deb) ...
Selecting previously unselected package liblapack-dev.
Unpacking liblapack-dev (from .../liblapack-dev_3.3.1-1_amd64.deb) ...
Selecting previously unselected package libqpdf3.
Unpacking libqpdf3 (from .../libqpdf3_2.3.1-1_amd64.deb) ...
Selecting previously unselected package qpdf.
Unpacking qpdf (from .../qpdf_2.3.1-1_amd64.deb) ...
Selecting previously unselected package tcl8.5.
Unpacking tcl8.5 (from .../tcl8.5_8.5.11-1ubuntu1_amd64.deb) ...
Selecting previously unselected package tk8.5.
Unpacking tk8.5 (from .../tk8.5_8.5.11-1_amd64.deb) ...
Selecting previously unselected package r-base-core.
Unpacking r-base-core (from .../r-base-core_3.1.2-1precise0_amd64.deb) ...
Selecting previously unselected package liblzma-dev.
Unpacking liblzma-dev (from .../liblzma-dev_5.1.1alpha+20110809-3_amd64.deb) ...
Selecting previously unselected package r-base-dev.
Unpacking r-base-dev (from .../r-base-dev_3.1.2-1precise0_all.deb) ...
Selecting previously unselected package r-cran-boot.
Unpacking r-cran-boot (from .../r-cran-boot_1.3-13-1precise0_all.deb) ...
Selecting previously unselected package r-cran-mass.
Unpacking r-cran-mass (from .../r-cran-mass_7.3-35-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-class.
Unpacking r-cran-class (from .../r-cran-class_7.3-11-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-cluster.
Unpacking r-cran-cluster (from .../r-cran-cluster_1.15.3-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-codetools.
Unpacking r-cran-codetools (from .../r-cran-codetools_0.2-9-1precise0_all.deb) ...
Selecting previously unselected package r-cran-foreign.
Unpacking r-cran-foreign (from .../r-cran-foreign_0.8.61-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-kernsmooth.
Unpacking r-cran-kernsmooth (from .../r-cran-kernsmooth_2.23-13-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-lattice.
Unpacking r-cran-lattice (from .../r-cran-lattice_0.20-29-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-matrix.
Unpacking r-cran-matrix (from .../r-cran-matrix_1.1-4-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-nlme.
Unpacking r-cran-nlme (from .../r-cran-nlme_3.1.118-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-mgcv.
Unpacking r-cran-mgcv (from .../r-cran-mgcv_1.8-4-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-nnet.
Unpacking r-cran-nnet (from .../r-cran-nnet_7.3-8-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-survival.
Unpacking r-cran-survival (from .../r-cran-survival_2.37-7-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-rpart.
Unpacking r-cran-rpart (from .../r-cran-rpart_4.1-8-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-spatial.
Unpacking r-cran-spatial (from .../r-cran-spatial_7.3-8-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-recommended.
Unpacking r-recommended (from .../r-recommended_3.1.2-1precise0_all.deb) ...
Processing triggers for man-db ...
Processing triggers for hicolor-icon-theme ...
Setting up liburi-perl (1.59-1) ...
Setting up libencode-locale-perl (1.02-2) ...
Setting up libhttp-date-perl (6.00-1) ...
Setting up libfile-listing-perl (6.03-1) ...
Setting up libhtml-tagset-perl (3.20-2) ...
Setting up libhtml-parser-perl (3.69-1build1) ...
Setting up libhtml-tree-perl (4.2-1) ...
Setting up liblwp-mediatypes-perl (6.01-1) ...
Setting up libhttp-message-perl (6.01-1) ...
Setting up libhttp-cookies-perl (6.00-2) ...
Setting up libhttp-negotiate-perl (6.00-2) ...
Setting up libnet-http-perl (6.02-1) ...
Setting up libnet-ssleay-perl (1.42-1build1) ...
Setting up libio-socket-ssl-perl (1.53-1) ...
Setting up libwww-robotrules-perl (6.01-1) ...
Setting up python-scour (0.26-3) ...
Setting up gfortran-4.6 (4.6.3-1ubuntu5) ...
Setting up gfortran (4:4.6.3-1ubuntu5) ...
update-alternatives: using /usr/bin/gfortran to provide /usr/bin/f95 (f95) in auto mode.
Setting up libblas-dev (1.2.20110419-2ubuntu1) ...
update-alternatives: using /usr/lib/libblas/libblas.so to provide /usr/lib/libblas.so (libblas.so) in auto mode.
Setting up liblapack-dev (3.3.1-1) ...
update-alternatives: using /usr/lib/lapack/liblapack.so to provide /usr/lib/liblapack.so (liblapack.so) in auto mode.
Setting up libqpdf3 (2.3.1-1) ...
Setting up qpdf (2.3.1-1) ...
Setting up tcl8.5 (8.5.11-1ubuntu1) ...
update-alternatives: using /usr/bin/tclsh8.5 to provide /usr/bin/tclsh (tclsh) in auto mode.
Setting up tk8.5 (8.5.11-1) ...
update-alternatives: using /usr/bin/wish8.5 to provide /usr/bin/wish (wish) in auto mode.
Setting up r-base-core (3.1.2-1precise0) ...
Creating config file /etc/R/Renviron with new version
Setting up liblzma-dev (5.1.1alpha+20110809-3) ...
Setting up r-cran-boot (1.3-13-1precise0) ...
Setting up r-cran-mass (7.3-35-1precise0) ...
Setting up r-cran-class (7.3-11-1precise0) ...
Setting up r-cran-cluster (1.15.3-1precise0) ...
Setting up r-cran-codetools (0.2-9-1precise0) ...
Setting up r-cran-foreign (0.8.61-1precise0) ...
Setting up r-cran-kernsmooth (2.23-13-1precise0) ...
Setting up r-cran-lattice (0.20-29-1precise0) ...
Setting up r-cran-matrix (1.1-4-1precise0) ...
Setting up r-cran-nlme (3.1.118-1precise0) ...
Setting up r-cran-mgcv (1.8-4-1precise0) ...
Setting up r-cran-nnet (7.3-8-1precise0) ...
Setting up r-cran-survival (2.37-7-1precise0) ...
Setting up r-cran-rpart (4.1-8-1precise0) ...
Setting up r-cran-spatial (7.3-8-1cran1ppa0) ...
Setting up r-recommended (3.1.2-1precise0) ...
Setting up liblwp-protocol-https-perl (6.02-1) ...
Setting up libwww-perl (6.03-1) ...
Setting up libxml-parser-perl (2.41-1build1) ...
Setting up intltool (0.50.2-2) ...
Setting up dh-translations (116) ...
Setting up cdbs (0.4.100ubuntu2) ...
Setting up r-base-dev (3.1.2-1precise0) ...
Processing triggers for libc-bin ...
ldconfig deferred processing now taking place
+return 0
+sudo chmod 2777 /usr/local/lib/R /usr/local/lib/R/site-library
+BootstrapLinuxOptions
+[[ -n '' ]]
+[[ -n '' ]]
+test -e .Rbuildignore
+grep -q travis-tool .Rbuildignore
+echo '^travis-tool\.sh$'
install.1
28.50s$ ./travis-tool.sh r_binary_install knitr
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=r_binary_install
+echo 'Running command: r_binary_install'
Running command: r_binary_install
+shift
+case $COMMAND in
+RBinaryInstall knitr
+[[ -z 1 ]]
+[[ Linux != \L\i\n\u\x ]]
+[[ -n '' ]]
+echo 'Installing *binary* R packages: knitr'
Installing *binary* R packages: knitr
++echo knitr
++tr '[:upper:]' '[:lower:]'
+r_packages=knitr
++for r_package in '${r_packages}'
++echo -n 'r-cran-knitr '
+r_debs='r-cran-knitr '
+AptGetInstall r-cran-knitr
+[[ Linux != \L\i\n\u\x ]]
+[[ '' == \r\-\c\r\a\n\-\k\n\i\t\r ]]
+echo 'Installing apt package(s) r-cran-knitr'
Installing apt package(s) r-cran-knitr
+Retry sudo apt-get install r-cran-knitr
+sudo apt-get install r-cran-knitr
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following package was automatically installed and is no longer required:
  libgeos-3.2.2
Use 'apt-get autoremove' to remove them.
The following extra packages will be installed:
  r-cran-digest r-cran-evaluate r-cran-formatr r-cran-highr r-cran-markdown
  r-cran-mime r-cran-plyr r-cran-rcpp r-cran-stringr
The following NEW packages will be installed:
  r-cran-digest r-cran-evaluate r-cran-formatr r-cran-highr r-cran-knitr
  r-cran-markdown r-cran-mime r-cran-plyr r-cran-rcpp r-cran-stringr
0 upgraded, 10 newly installed, 0 to remove and 128 not upgraded.
Need to get 4,081 kB of archives.
After this operation, 11.9 MB of additional disk space will be used.
Get:1 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-digest amd64 0.6.6-1cran1ppa0precise0 [100 kB]
Get:2 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-rcpp amd64 0.11.3-1cran1ppa0 [2,191 kB]
Get:3 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-plyr amd64 1.8.1-1cran1ppa0 [738 kB]
Get:4 http://ppa.launchpad.net/marutter/rrutter/ubuntu/ precise/main r-cran-stringr all 0.6.2-1precise0 [73.0 kB]
Get:5 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-evaluate all 0.5.5-1cran1ppa0 [40.7 kB]
Get:6 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-formatr all 1.0-1cran1ppa0 [48.7 kB]
Get:7 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-highr all 0.4-1cran1ppa0precise0 [29.3 kB]
Get:8 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-mime amd64 0.2-1cran1ppa0 [24.1 kB]
Get:9 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-markdown amd64 0.7.4-1cran1ppa0 [113 kB]
Get:10 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-knitr all 1.8-1cran1ppa0precise0 [724 kB]
Fetched 4,081 kB in 4s (838 kB/s)
Selecting previously unselected package r-cran-digest.
(Reading database ... 73818 files and directories currently installed.)
Unpacking r-cran-digest (from .../r-cran-digest_0.6.6-1cran1ppa0precise0_amd64.deb) ...
Selecting previously unselected package r-cran-rcpp.
Unpacking r-cran-rcpp (from .../r-cran-rcpp_0.11.3-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-cran-plyr.
Unpacking r-cran-plyr (from .../r-cran-plyr_1.8.1-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-cran-stringr.
Unpacking r-cran-stringr (from .../r-cran-stringr_0.6.2-1precise0_all.deb) ...
Selecting previously unselected package r-cran-evaluate.
Unpacking r-cran-evaluate (from .../r-cran-evaluate_0.5.5-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-formatr.
Unpacking r-cran-formatr (from .../r-cran-formatr_1.0-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-highr.
Unpacking r-cran-highr (from .../r-cran-highr_0.4-1cran1ppa0precise0_all.deb) ...
Selecting previously unselected package r-cran-mime.
Unpacking r-cran-mime (from .../r-cran-mime_0.2-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-cran-markdown.
Unpacking r-cran-markdown (from .../r-cran-markdown_0.7.4-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-cran-knitr.
Unpacking r-cran-knitr (from .../r-cran-knitr_1.8-1cran1ppa0precise0_all.deb) ...
Setting up r-cran-digest (0.6.6-1cran1ppa0precise0) ...
Setting up r-cran-rcpp (0.11.3-1cran1ppa0) ...
Setting up r-cran-plyr (1.8.1-1cran1ppa0) ...
Setting up r-cran-stringr (0.6.2-1precise0) ...
Setting up r-cran-evaluate (0.5.5-1cran1ppa0) ...
Setting up r-cran-formatr (1.0-1cran1ppa0) ...
Setting up r-cran-highr (0.4-1cran1ppa0precise0) ...
Setting up r-cran-mime (0.2-1cran1ppa0) ...
Setting up r-cran-markdown (0.7.4-1cran1ppa0) ...
Setting up r-cran-knitr (1.8-1cran1ppa0precise0) ...
+return 0
install.2
28.10s$ ./travis-tool.sh install_github yihui/formatR
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=install_github
+echo 'Running command: install_github'
Running command: install_github
+shift
+case $COMMAND in
+InstallGithub yihui/formatR
+EnsureDevtools
+Rscript -e 'if (!("devtools" %in% rownames(installed.packages()))) q(status=1)'
+RBinaryInstall devtools testthat
+[[ -z 2 ]]
+[[ Linux != \L\i\n\u\x ]]
+[[ -n '' ]]
+echo 'Installing *binary* R packages: devtools testthat'
Installing *binary* R packages: devtools testthat
++echo devtools testthat
++tr '[:upper:]' '[:lower:]'
+r_packages='devtools testthat'
++for r_package in '${r_packages}'
++echo -n 'r-cran-devtools '
++for r_package in '${r_packages}'
++echo -n 'r-cran-testthat '
+r_debs='r-cran-devtools r-cran-testthat '
+AptGetInstall r-cran-devtools r-cran-testthat
+[[ Linux != \L\i\n\u\x ]]
+[[ '' == \r\-\c\r\a\n\-\d\e\v\t\o\o\l\s\ \r\-\c\r\a\n\-\t\e\s\t\t\h\a\t ]]
+echo 'Installing apt package(s) r-cran-devtools' r-cran-testthat
Installing apt package(s) r-cran-devtools r-cran-testthat
+Retry sudo apt-get install r-cran-devtools r-cran-testthat
+sudo apt-get install r-cran-devtools r-cran-testthat
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following package was automatically installed and is no longer required:
  libgeos-3.2.2
Use 'apt-get autoremove' to remove them.
The following extra packages will be installed:
  r-cran-bitops r-cran-httr r-cran-jsonlite r-cran-memoise r-cran-r6
  r-cran-rcurl r-cran-rstudioapi r-cran-whisker
The following NEW packages will be installed:
  r-cran-bitops r-cran-devtools r-cran-httr r-cran-jsonlite r-cran-memoise
  r-cran-r6 r-cran-rcurl r-cran-rstudioapi r-cran-testthat r-cran-whisker
0 upgraded, 10 newly installed, 0 to remove and 128 not upgraded.
Need to get 2,638 kB of archives.
After this operation, 5,593 kB of additional disk space will be used.
Get:1 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-bitops amd64 1.0-6-1precise0 [23.6 kB]
Get:2 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-jsonlite amd64 0.9.14-1cran1ppa0precise0 [860 kB]
Get:3 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-rcurl amd64 1.95-4.5-1cran1ppa0precise0 [690 kB]
Get:4 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-r6 all 2.0.1-1cran1ppa0precise0 [111 kB]
Get:5 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-httr amd64 0.6.0-1cran1ppa0precise0 [366 kB]
Get:6 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-memoise all 0.2.1-1cran1ppa0 [16.3 kB]
Get:7 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-whisker all 0.3-2-1cran1precise0 [50.5 kB]
Get:8 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-rstudioapi all 0.1-1cran1ppa0 [15.0 kB]
Get:9 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-devtools amd64 1.6.1-1cran1ppa0 [268 kB]
Get:10 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-testthat amd64 0.9.1-1cran1ppa0 [237 kB]
Fetched 2,638 kB in 2s (1,207 kB/s)
Selecting previously unselected package r-cran-bitops.
(Reading database ... 75014 files and directories currently installed.)
Unpacking r-cran-bitops (from .../r-cran-bitops_1.0-6-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-jsonlite.
Unpacking r-cran-jsonlite (from .../r-cran-jsonlite_0.9.14-1cran1ppa0precise0_amd64.deb) ...
Selecting previously unselected package r-cran-rcurl.
Unpacking r-cran-rcurl (from .../r-cran-rcurl_1.95-4.5-1cran1ppa0precise0_amd64.deb) ...
Selecting previously unselected package r-cran-r6.
Unpacking r-cran-r6 (from .../r-cran-r6_2.0.1-1cran1ppa0precise0_all.deb) ...
Selecting previously unselected package r-cran-httr.
Unpacking r-cran-httr (from .../r-cran-httr_0.6.0-1cran1ppa0precise0_amd64.deb) ...
Selecting previously unselected package r-cran-memoise.
Unpacking r-cran-memoise (from .../r-cran-memoise_0.2.1-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-whisker.
Unpacking r-cran-whisker (from .../r-cran-whisker_0.3-2-1cran1precise0_all.deb) ...
Selecting previously unselected package r-cran-rstudioapi.
Unpacking r-cran-rstudioapi (from .../r-cran-rstudioapi_0.1-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-devtools.
Unpacking r-cran-devtools (from .../r-cran-devtools_1.6.1-1cran1ppa0_amd64.deb) ...
Selecting previously unselected package r-cran-testthat.
Unpacking r-cran-testthat (from .../r-cran-testthat_0.9.1-1cran1ppa0_amd64.deb) ...
Setting up r-cran-bitops (1.0-6-1precise0) ...
Setting up r-cran-jsonlite (0.9.14-1cran1ppa0precise0) ...
Setting up r-cran-rcurl (1.95-4.5-1cran1ppa0precise0) ...
Setting up r-cran-r6 (2.0.1-1cran1ppa0precise0) ...
Setting up r-cran-httr (0.6.0-1cran1ppa0precise0) ...
Setting up r-cran-memoise (0.2.1-1cran1ppa0) ...
Setting up r-cran-whisker (0.3-2-1cran1precise0) ...
Setting up r-cran-rstudioapi (0.1-1cran1ppa0) ...
Setting up r-cran-devtools (1.6.1-1cran1ppa0) ...
Setting up r-cran-testthat (0.9.1-1cran1ppa0) ...
+return 0
+echo 'Installing GitHub packages: yihui/formatR'
Installing GitHub packages: yihui/formatR
+Rscript -e 'library(devtools); library(methods); options(repos=c(CRAN="http://cran.rstudio.com")); install_github(commandArgs(TRUE), build_vignettes = FALSE)' yihui/formatR
Downloading github repo yihui/formatR@master
Installing formatR
'/usr/lib/R/bin/R' --vanilla CMD INSTALL  \
  '/tmp/Rtmp0HGJ6G/devtools1b7a376f2f27/yihui-formatR-867ea54'  \
  --library='/usr/local/lib/R/site-library' --install-tests 
* installing *source* package ‘formatR’ ...
** R
** inst
** tests
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
* DONE (formatR)
install.3
21.52s$ ./travis-tool.sh r_binary_install ggplot2
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=r_binary_install
+echo 'Running command: r_binary_install'
Running command: r_binary_install
+shift
+case $COMMAND in
+RBinaryInstall ggplot2
+[[ -z 1 ]]
+[[ Linux != \L\i\n\u\x ]]
+[[ -n '' ]]
+echo 'Installing *binary* R packages: ggplot2'
Installing *binary* R packages: ggplot2
++echo ggplot2
++tr '[:upper:]' '[:lower:]'
+r_packages=ggplot2
++for r_package in '${r_packages}'
++echo -n 'r-cran-ggplot2 '
+r_debs='r-cran-ggplot2 '
+AptGetInstall r-cran-ggplot2
+[[ Linux != \L\i\n\u\x ]]
+[[ '' == \r\-\c\r\a\n\-\g\g\p\l\o\t\2 ]]
+echo 'Installing apt package(s) r-cran-ggplot2'
Installing apt package(s) r-cran-ggplot2
+Retry sudo apt-get install r-cran-ggplot2
+sudo apt-get install r-cran-ggplot2
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following package was automatically installed and is no longer required:
  libgeos-3.2.2
Use 'apt-get autoremove' to remove them.
The following extra packages will be installed:
  r-cran-colorspace r-cran-dichromat r-cran-gtable r-cran-labeling
  r-cran-munsell r-cran-proto r-cran-rcolorbrewer r-cran-reshape2
  r-cran-scales
The following NEW packages will be installed:
  r-cran-colorspace r-cran-dichromat r-cran-ggplot2 r-cran-gtable
  r-cran-labeling r-cran-munsell r-cran-proto r-cran-rcolorbrewer
  r-cran-reshape2 r-cran-scales
0 upgraded, 10 newly installed, 0 to remove and 128 not upgraded.
Need to get 4,144 kB of archives.
After this operation, 5,538 kB of additional disk space will be used.
Get:1 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-colorspace amd64 1.2-4-1cran1precise0 [358 kB]
Get:2 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-dichromat all 2.0.0-2precise3 [147 kB]
Get:3 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-gtable all 0.1.2-2cran1precise3 [62.7 kB]
Get:4 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-reshape2 amd64 1.4.1-1cran1ppa0precise0 [83.0 kB]
Get:5 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-rcolorbrewer all 1.1-2-1ppa0precise0 [26.8 kB]
Get:6 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-munsell all 0.4.2-1cran1precise0 [128 kB]
Get:7 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-labeling all 0.3-1cran1ppa0 [40.1 kB]
Get:8 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-scales all 0.2.4-1cran1ppa0 [151 kB]
Get:9 http://ppa.launchpad.net/marutter/rrutter/ubuntu/ precise/main r-cran-proto amd64 0.3-10-1precise0 [462 kB]
Get:10 http://ppa.launchpad.net/marutter/c2d4u/ubuntu/ precise/main r-cran-ggplot2 all 1.0.0-1cran1ppa0 [2,685 kB]
Fetched 4,144 kB in 3s (1,084 kB/s)
Selecting previously unselected package r-cran-colorspace.
(Reading database ... 75486 files and directories currently installed.)
Unpacking r-cran-colorspace (from .../r-cran-colorspace_1.2-4-1cran1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-dichromat.
Unpacking r-cran-dichromat (from .../r-cran-dichromat_2.0.0-2precise3_all.deb) ...
Selecting previously unselected package r-cran-gtable.
Unpacking r-cran-gtable (from .../r-cran-gtable_0.1.2-2cran1precise3_all.deb) ...
Selecting previously unselected package r-cran-reshape2.
Unpacking r-cran-reshape2 (from .../r-cran-reshape2_1.4.1-1cran1ppa0precise0_amd64.deb) ...
Selecting previously unselected package r-cran-rcolorbrewer.
Unpacking r-cran-rcolorbrewer (from .../r-cran-rcolorbrewer_1.1-2-1ppa0precise0_all.deb) ...
Selecting previously unselected package r-cran-munsell.
Unpacking r-cran-munsell (from .../r-cran-munsell_0.4.2-1cran1precise0_all.deb) ...
Selecting previously unselected package r-cran-labeling.
Unpacking r-cran-labeling (from .../r-cran-labeling_0.3-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-scales.
Unpacking r-cran-scales (from .../r-cran-scales_0.2.4-1cran1ppa0_all.deb) ...
Selecting previously unselected package r-cran-proto.
Unpacking r-cran-proto (from .../r-cran-proto_0.3-10-1precise0_amd64.deb) ...
Selecting previously unselected package r-cran-ggplot2.
Unpacking r-cran-ggplot2 (from .../r-cran-ggplot2_1.0.0-1cran1ppa0_all.deb) ...
Setting up r-cran-colorspace (1.2-4-1cran1precise0) ...
Setting up r-cran-dichromat (2.0.0-2precise3) ...
Setting up r-cran-gtable (0.1.2-2cran1precise3) ...
Setting up r-cran-reshape2 (1.4.1-1cran1ppa0precise0) ...
Setting up r-cran-rcolorbrewer (1.1-2-1ppa0precise0) ...
Setting up r-cran-munsell (0.4.2-1cran1precise0) ...
Setting up r-cran-labeling (0.3-1cran1ppa0) ...
Setting up r-cran-scales (0.2.4-1cran1ppa0) ...
Setting up r-cran-proto (0.3-10-1precise0) ...
Setting up r-cran-ggplot2 (1.0.0-1cran1ppa0) ...
+return 0
install.4
1.18s$ ./travis-tool.sh r_binary_install reshape2
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=r_binary_install
+echo 'Running command: r_binary_install'
Running command: r_binary_install
+shift
+case $COMMAND in
+RBinaryInstall reshape2
+[[ -z 1 ]]
+[[ Linux != \L\i\n\u\x ]]
+[[ -n '' ]]
+echo 'Installing *binary* R packages: reshape2'
Installing *binary* R packages: reshape2
++echo reshape2
++tr '[:upper:]' '[:lower:]'
+r_packages=reshape2
++for r_package in '${r_packages}'
++echo -n 'r-cran-reshape2 '
+r_debs='r-cran-reshape2 '
+AptGetInstall r-cran-reshape2
+[[ Linux != \L\i\n\u\x ]]
+[[ '' == \r\-\c\r\a\n\-\r\e\s\h\a\p\e\2 ]]
+echo 'Installing apt package(s) r-cran-reshape2'
Installing apt package(s) r-cran-reshape2
+Retry sudo apt-get install r-cran-reshape2
+sudo apt-get install r-cran-reshape2
Reading package lists... Done
Building dependency tree       
Reading state information... Done
r-cran-reshape2 is already the newest version.
r-cran-reshape2 set to manually installed.
The following package was automatically installed and is no longer required:
  libgeos-3.2.2
Use 'apt-get autoremove' to remove them.
0 upgraded, 0 newly installed, 0 to remove and 128 not upgraded.
+return 0
install.5
0.80s$ ./travis-tool.sh install_deps
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=install_deps
+echo 'Running command: install_deps'
Running command: install_deps
+shift
+case $COMMAND in
+InstallDeps
+EnsureDevtools
+Rscript -e 'if (!("devtools" %in% rownames(installed.packages()))) q(status=1)'
+Rscript -e 'library(devtools); library(methods); options(repos=c(CRAN="http://cran.rstudio.com")); install_deps(dependencies = TRUE)'
45.45s$ ./travis-tool.sh run_tests
+CRAN=http://cran.rstudio.com
+BIOC=http://bioconductor.org/biocLite.R
+BIOC_USE_DEVEL=TRUE
++uname -s
+OS=Linux
+PANDOC_VERSION=1.12.4.2
+PANDOC_DIR=/home/travis/opt
+PANDOC_URL=https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.12.4.2.zip
+PATH=/usr/local/phantomjs/bin:/home/travis/.nvm/v0.10.33/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551/bin:/home/travis/.rvm/gems/ruby-1.9.3-p551@global/bin:/home/travis/.rvm/rubies/ruby-1.9.3-p551/bin:/usr/local/phantomjs/bin:./node_modules/.bin:/usr/local/maven-3.2.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/bin:/home/travis/.gvm/gos/go1.3.3/bin:/home/travis/.gvm/pkgsets/go1.3.3/global/overlay/bin:/home/travis/.gvm/bin:/home/travis/.gvm/bin:/usr/local/clang-3.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/travis/.rvm/bin:/home/travis/.rvm/bin:/usr/texbin
+R_BUILD_ARGS=--no-manual
+R_CHECK_ARGS='--no-manual --as-cran'
+R_USE_BIOC_CMDS='source('\''http://bioconductor.org/biocLite.R'\''); tryCatch(useDevel(TRUE), error=function(e) {if (!grepl('\''already in use'\'', e)) {e}}); options(repos=biocinstallRepos());'
+COMMAND=run_tests
+echo 'Running command: run_tests'
Running command: run_tests
+shift
+case $COMMAND in
+RunTests
+echo 'Building with: R CMD build --no-manual'
Building with: R CMD build --no-manual
+R CMD build --no-manual .
* checking for file ‘./DESCRIPTION’ ... OK
* preparing ‘spca’:
* checking DESCRIPTION meta-information ... OK
* installing the package to build vignettes
* creating vignettes ... OK
* checking for LF line-endings in source and make files
* checking for empty or unneeded directories
* looking to see if a ‘data/datalist’ file should be added
* building ‘spca_0.6.0.tar.gz’
++ls -1t spca_0.6.0.tar.gz
++head -n 1
+FILE=spca_0.6.0.tar.gz
+[[ Linux == \M\I\N\G\W ]]
+echo 'Testing with: R CMD check "spca_0.6.0.tar.gz" --no-manual --as-cran '
Testing with: R CMD check "spca_0.6.0.tar.gz" --no-manual --as-cran 
+_R_CHECK_CRAN_INCOMING_=FALSE
+[[ FALSE == \F\A\L\S\E ]]
+echo '(CRAN incoming checks are off)'
(CRAN incoming checks are off)
+_R_CHECK_CRAN_INCOMING_=FALSE
+R CMD check spca_0.6.0.tar.gz --no-manual --as-cran
* using log directory ‘/home/travis/build/merolagio/spca/spca.Rcheck’
* using R version 3.1.2 (2014-10-31)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* checking for file ‘spca/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘spca’ version ‘0.6.0’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘spca’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ...
   ‘Advanced_Example.Rmd’ ... OK
   ‘BE_Algorithm.Rmd’ using ‘UTF-8’ ... OK
   ‘Introduction.Rmd’ ... OK
   ‘manual.Rmd’ ... OK
 OK
* checking re-building of vignette outputs ... OK
* DONE
+[[ -n '' ]]
+[[ -n '' ]]
The command "./travis-tool.sh run_tests" exited with 0.
Done. Your build exited with 0.