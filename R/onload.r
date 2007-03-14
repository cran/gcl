################################################################################
#
# File:         onload.r
# RCS:          $Header: $
# Description:  Package .onLoad function
# Author:       Staal Vinterbo
# Created:      Wed Mar 14 14:51:09 2007
# Modified:     Wed Mar 14 17:31:01 2007 (Staal Vinterbo) staal@peep
# Language:     ESS[S]
# Package:      glc
# Status:       Experimental
#
# onload.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# onload.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with onload.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2007, Staal Vinterbo, all rights reserved.
#
################################################################################

.onLoad <- function(libname, pkgname){
  options(gcl.decorate=2)
}

  
