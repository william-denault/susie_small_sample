# Define the values for n and h
n_values <- c(10, 20, 30, 50, 70, 100)
h_values <- c(25, 30, 50, 75)

# Specify the output file where the generated script will be saved

path = "/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/cor_susie_unfilter/"

tt=1

# Loop over all combinations of n and h
for (n in n_values) {
  for (h in h_values) {

    output_file <- paste0(path, "cor_susie_small_n",tt,".R")

    # Open a connection to write to the file
    fileConn <- file(output_file, open = "w")

    # Write the header of the script
    writeLines("library(susieR)\n", fileConn)
    writeLines("rm(list=ls())\n", fileConn)
    writeLines('source("/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/cor_sample_sim_unfilter.R")\n', fileConn)

    # Write the inner part of the script for each combination of n and h
    writeLines(paste("for (o in 1:100){\n", sep=""), fileConn)
    writeLines(paste("  temp0 <- run_susie_sim(N=", n, ", h=", h/100, ", n_sim=100)\n", sep=""), fileConn)

    # Write file check and load/save logic
    writeLines(paste('  if(file.exists("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie', n, '_h', h, '_unfilter.RData")){\n', sep=""), fileConn)
    writeLines(paste('    load("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie', n, '_h', h, '_unfilter.RData")\n', sep=""), fileConn)
    writeLines("    if(!is.null(temp)){\n", fileConn)
    writeLines("      temp <- c(temp, temp0)\n", fileConn)
    writeLines("    } else {\n", fileConn)
    writeLines("      temp <- temp0\n", fileConn)
    writeLines("    }\n", fileConn)
    writeLines("  } else {\n", fileConn)
    writeLines("    temp <- temp0\n", fileConn)
    writeLines(paste('  }\n', sep=""), fileConn)
    writeLines(paste('  save(temp, file="/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie', n, '_h', h, '_unfilter.RData")\n', sep=""), fileConn)
    writeLines("}\n", fileConn)



    # Close the connection to the file
    close(fileConn)
    tt=tt+1
    # Notify the user
    cat("Script has been saved to", output_file, "\n")
  }
}

