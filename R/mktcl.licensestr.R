`mktcl.licensestr` <-
function () 
paste(" tcl generated software is free software; you can redistribute it and/or modify", 
    " it under the terms of the GNU General Public License as published by", 
    " the Free Software Foundation; either version 2 of the License, or", 
    " (at your option) any later version.", "", " tcl is distributed in the hope that it will be useful,", 
    " but WITHOUT ANY WARRANTY; without even the implied warranty of", 
    " MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the", 
    " GNU General Public License for more details.", "", " You should have received a copy of the GNU General Public License", 
    " along with tcl; if not, write to the Free Software", " Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA", 
    "", paste(" (c) Copyright 2005-", format(Sys.Date(), "%Y"), 
        ", Staal Vinterbo, all rights reserved.            ", 
        sep = ""), sep = "\n")
