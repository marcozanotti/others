# Function to write install or library 
write_packages <- function(package, type) {
	
	if (type == "inst") {
		f <- "install.packages"
		res <- paste0(f, "('", package, "')")
	} else {
		f <- "library"
		res <- paste0(f, "(", package, ")")
	}
	
	return(res)
	
}


# Function to write sections to R script
write_sections <- function(
	path, 
	sections = c("inst", "lib", "import", "clean", "feateng", "eda", "model"), 
	packages = c("tidyverse", "readxl", "lubridate", "skimr", "DataExplorer")
) {
	
	# Create R script sections in order
	# 0. Title
	txt <- c(
		"# Project:   ----",
		"", 
		"# Author: ",
		"# Date: ", 
		"",
		"",
		""
	)
	
	# 1. Installs
	if ("inst" %in% sections) {
		txt_add <- c(
			"# Installs ----",
			"",
			sapply(packages, write_packages, type = "inst", USE.NAMES = FALSE),
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 2. Libraries
	if ("lib" %in% sections) {
		txt_add <- c(
			"# Libraries ----",
			"",
			sapply(packages, write_packages, type = "lib", USE.NAMES = FALSE),
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 3. Importing data
	if ("import" %in% sections) {
		txt_add <- c(
			"# Importing data ----",
			"",
			"data <- read_excel('data/dataset.xlsx')",
			"str(data)",
			"skim(data)",
			"",
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 4. Cleaning data
	if ("clean" %in% sections) {
		txt_add <- c(
			"# Cleaning data ----",
			"",
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 5. Feature engineering
	if ("feateng" %in% sections) {
		txt_add <- c(
			"# Feature engineering ----",
			"",
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 6. EDA
	if ("eda" %in% sections) {
		txt_add <- c(
			"# Explorative analysis ----",
			"",
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	# 7. Modelling
	if ("model" %in% sections) {
		txt_add <- c(
			"# Modelling ----",
			"",
			"",
			"",
			""
		)
		txt <- c(txt, txt_add)
	}
	
	writeLines(txt, path)	
	
	return(invisible(NULL))
	
}


# Function to create the project template
project_template <- function(
	path, 
	proj_name, 
	directories = c("data", "R", "results"),
	sections = c("inst", "lib", "import", "clean", "feateng", "eda", "model"),
	packages = c("tidyverse", "readxl", "lubridate", "skimr", "DataExplorer")
){

	proj_path <- file.path(path, proj_name)

	# Create directories
	if (!dir.exists(proj_path))
		dir.create(proj_path)

	for (dir in directories) {
		if (!dir.exists(file.path(proj_path, dir)))
			dir.create(file.path(proj_path, dir))
	}

	# Create R file for the analysis
	if (!file.exists(file.path(proj_path, "R", "analysis.R")))
		file.create(file.path(proj_path, "R", "analysis.R"))
	
	# Write R file with sections (keep sections' order)
	file_path <- file.path(proj_path, "R", "analysis.R")
	write_sections(file_path, sections, packages)
	
	return(invisible(NULL))

}


project_template(
	"Bogo", 
	"project-regression", 
	packages = c(
		"tidyverse", "readxl", "lubridate", "skimr", "DataExplorer", 
		"broom", "lmtest", "sandwich"
	)
)
