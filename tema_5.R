#R version 4.4.0
install.packages("asteRisk")
library(asteRisk)

first_data <- readGPSNavigationRINEX(readline(prompt="Enter first RINEX file name: "))
second_data <- readGPSNavigationRINEX(readline(prompt="Enter second RINEX file name: "))
out_file <- file(readline(prompt="Enter output file name: "), open = 'a')

if(identical(first_data, second_data)) {
  print("Datoteke su identične")
}

identical_props <- function(first_array, second_array) {
  if(is.null(first_array) || is.null(second_array)) {
    return(FALSE)
  }
  
  return(identical(sort(na.omit(names(first_data))), sort(na.omit(names(second_data)))))
}

if(identical_props(first_data[["header"]], second_data[["header"]])) {
  for(param in names(first_data[["header"]])) {
    if(second_data[["header"]][[param]] != first_data[["header"]][[param]]) {
      lines <- c(paste(paste("HEADER", param, first_data[["header"]][[param]], sep = " : "), paste("HEADER", param, second_data[["header"]][[param]], sep = " : "), sep = " <-> "))
      writeLines(lines, out_file)
    }
  }
} else {
  writeLines(c("Header properties are not equal!"), out_file)
}

if(length(first_data[["messages"]]) == length(second_data[["messages"]])) {
  for(i in seq_along(first_data[["messages"]])) {
    if(identical_props(first_data[["messages"]][[i]], second_data[["messages"]][[i]])) {
      for(param in names(first_data[["messages"]][[i]])) {
        first_value = first_data[["messages"]][[i]][[param]]
        second_value = second_data[["messages"]][[i]][[param]]
        
        if(!identical(first_value, second_value)) {
          lines <- c(paste(paste("MESSAGE",i, param, first_value, sep = " : "), paste("MESSAGE",i, param, second_value, sep = " : "), sep = " <-> "))
          writeLines(lines, out_file)
        }
      }
    } else {
      writeLines(c(paste("Messages at index ", i, " do not have same amount of properties")), out_file)
    }
  }
} else {
  writeLines(c("RINEX files do not contain same number of messages!"), out_file)
}
close(out_file)

