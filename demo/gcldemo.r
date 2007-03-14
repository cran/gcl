################################################################################
#
# File:         glcdemo.r
# RCS:          $Header: $
# Description:  Example file for gcl package
# Author:       Staal Vinterbo
# Created:      Thu Nov 10 23:01:35 2005
# Modified:     Tue Jun 19 09:55:24 2007 (Staal Vinterbo) staal@peep
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# gcldemo.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# gcldemo.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with example.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2005, Staal Vinterbo, all rights reserved.
#
################################################################################


library(gcl)
#gcldemo <- function() {
  df <-
    structure(list(V1 = c(0.628, 0.317, 0.275, 0.458, 0.926, 0.456, 
       0.894, 0.415, 0.694, 0.193), V2 = c(0.525, 0.746, 0.669, 0.179, 
       0.162, 0.082, 0.951, 0.731, 0.656, 0.478), V3 = c(0.634, 0.994, 
       0.709, 0.198, 0.857, 0.109, 0.896, 0.626, 0.335, 0.615), V4 = c(0.714, 
       0.661, 0.208, 0.996, 0.309, 0.306, 0.076, 0.463, 0.187, 0.59), 
       V5 = as.integer(c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))),
              .Names = c("V1", "V2", "V3", "V4", "V5"),
              class = "data.frame",
              row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

  cat("Data table:\n")
  print(df)

  cat("computing fuzzy rule classifier function cf...\n")
  cf <- gcl(df,filter=df)
  cat("done.\n")
  cat(cf())
  cat("Classifying data:\n")
  print(cf(df))
#  cat("Dumping source of classifying function cf to /tmp/example-output.r\n")
#  dump("cf", "/tmp/example-output.r")
#  cat("Removing old definition of classifying function cf...")
#  rm("cf")
#  cat("done.\n")
#  cat("Sourcing /tmp/example-output.r containing definition of function cf...")
#  source('/tmp/example-output.r')
#  cat("done.\n")
#  cat("Classifying data again:\n")
#  print(cf(df))
  cat("Making class calls according to max membership:\n")
  calls <- apply(cf(df), 1, which.max) - 1
  m <- cbind(calls, df[,5])
  colnames(m) <- c("GCL Call made", "Actual Class")
  errors <- sum(m[,1] != m[,2])
  cat("Calls:\n")
  print(m)
  cat("Made", errors, "errors.\n")
  cat("That is all folks.\n")
#}

#gcldemo()
