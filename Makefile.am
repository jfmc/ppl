# Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AUTOMAKE_OPTIONS = foreign

srcdir = @srcdir@
VPATH = @srcdir@

EXTRA_DIST = configure ppl.lsm.in ppl.spec.in acinclude.m4

@SET_MAKE@

SUBDIRS = @extra_subdirs@ src tests m4 doc


#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

#  build and install the .info pages
#info_TEXINFOS = PPL.texinfo
#ppl_TEXINFOS = gpl.texinfo

#  install the man pages
#man_MANS=ppl.1
