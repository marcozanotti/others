# function to extract significance stars
extract_significance <- function(p_value) {
	
	res <- dplyr::case_when(
		p_value < 0.001 ~ "***", 
		p_value >= 0.001 & p_value < 0.01 ~ "**",
		p_value >= 0.01 & p_value < 0.05 ~ "*",
		p_value >= 0.05 & p_value < 0.1 ~ ".",
		TRUE ~ ""
	)
	return(res)
	
}

# function to format lm/glm results in a tidy tibble
format_lm_output <- function(lm_results, digits = 4) {
	
	lm_res_tidy <- lm_results  |> 
		broom::tidy() |> 
		dplyr::mutate(stars = extract_significance(p.value)) |> 
		dplyr::mutate(
			text = paste0(
				round(estimate, digits), " ", stars, " \n", "(", round(std.error, digits), ")"
			)
		) |> 
		dplyr::select(term, text)
	
	return(lm_res_tidy)
	
}

# function to format lm/glm statistics in a tidy tibble
format_lm_statistics <- function(adj_r2, fstat, nobs, digits = 4) {
	
	stat_tbl <- dplyr::tibble(
		"term" = c("Adj.R2", "F.Stat", "N.Obs"),
		"text" = c(adj_r2, fstat, nobs) |> round(digits) |> as.character()
	)
	
	return(stat_tbl)
	
}

# function to combine lm/glm and stat results
combine_results <- function(lm_res_tidy, lm_stat_tidy) {
	
	lm_res_bind <- purrr::map2(lm_res_tidy, lm_stat_tidy, dplyr::bind_rows)
	x_vars <- purrr::map(lm_res_tidy, "term") |> unlist() |> unique()
	statistics <- purrr::map(lm_stat_tidy, "term") |> unlist() |> unique()
	res <- dplyr::tibble("term" = c(x_vars, statistics)) 
	
	res <- purrr::map(lm_res_bind, ~ dplyr::left_join(res, ., by = "term")) |> 
		dplyr::bind_cols(.name_repair = "unique") |> 
		dplyr::select(1, dplyr::contains("text")) |> 
		purrr::set_names("Variables", names(lm_res_bind))
	
	return(res)
	
}

# function to estimate lm models and format results in a tidy way
lm_tidy <- function(
		formulas, data, 
		robust_vcov = NULL, 
		digits = 4
	) {
	
	# estimate lm model
	lm_res <- purrr::map(formulas, lm, data = data)
	
	# extract lm statistics
	summary_res <- purrr::map(lm_res, summary)
	adj_r2 <- purrr::map(summary_res, "adj.r.squared")
	adj_r2 <- ifelse(adj_r2 < 0, 0, adj_r2)
	fstat <- purrr::map(summary_res, "fstatistic") |> purrr::map("value")
	nobs <- purrr::map(summary_res, "df")
	nobs <- (unlist(purrr::map(nobs, 1)) + unlist(purrr::map(nobs, 2))) |> as.list()
	
	if (!is.null(robust_vcov)) {
		lm_res <- purrr::map(lm_res, lmtest::coeftest, vcov. = robust_vcov)
	}
	
	# format results and combine
	lm_res_tidy <- purrr::map(lm_res, format_lm_output, digits = digits) 
	lm_stat_tidy <- purrr::pmap(list(adj_r2, fstat, nobs), format_lm_statistics, digits = digits)
	res <- suppressMessages(combine_results(lm_res_tidy, lm_stat_tidy))
	
	return(res)
	
}

# function to estimate glm models and format results in a tidy way
glm_tidy <- function(
		formulas, data, 
		family = binomial(link = "logit"), 
		robust_vcov = NULL, 
		digits = 4
) {

	# estimate glm model
	glm_res <- purrr::map(formulas, glm, data = data, family = family)
	
	# extract glm statistics
	summary_res <- purrr::map(glm_res, summary)
	dev <- purrr::map(summary_res, "deviance")
	null_dev <- purrr::map(summary_res, "null.deviance")
	adj_r2 <- (1 - unlist(purrr::map(dev, 1)) / unlist(purrr::map(null_dev, 1))) |> as.list()
	adj_r2 <- ifelse(adj_r2 < 0, 0, adj_r2)
	fstat <- NA
	nobs <- purrr::map(summary_res, "df.null")
	nobs <- (unlist(purrr::map(nobs, 1)) + 1) |> as.list()
	
	if (!is.null(robust_vcov)) {
		glm_res <- purrr::map(glm_res, lmtest::coeftest, vcov. = robust_vcov)
	}
	
	# format results and combine
	glm_res_tidy <- purrr::map(glm_res, format_lm_output, digits = digits) 
	glm_stat_tidy <- purrr::pmap(list(adj_r2, fstat, nobs), format_lm_statistics, digits = digits)
	res <- suppressMessages(combine_results(glm_res_tidy, glm_stat_tidy))
	
	return(res)
	
}

# function to estimate plm models and format results in a tidy way
plm_tidy <- function(
		formulas, data, 
		robust_vcov = NULL, 
		digits = 4,
		...
) {
	
	# estimate plm model
	plm_res <- purrr::map(formulas, plm::plm, data = data, ...)
	
	# extract plm statistics
	summary_res <- purrr::map(plm_res, summary)
	adj_r2 <- purrr::map(summary_res, "r.squared") |> purrr::map(2)
	adj_r2 <- ifelse(adj_r2 < 0, 0, adj_r2)
	fstat <- purrr::map(summary_res, "fstatistic") |> purrr::map("statistic")
	nobs <- purrr::map(summary_res, "df")
	nobs <- (unlist(purrr::map(nobs, 1)) + unlist(purrr::map(nobs, 2))) |> as.list() # approximation
	
	if (!is.null(robust_vcov)) {
		plm_res <- purrr::map(plm_res, lmtest::coeftest, vcov. = robust_vcov)
	}
	
	# format results and combine
	plm_res_tidy <- purrr::map(plm_res, format_lm_output, digits = digits) 
	plm_stat_tidy <- purrr::pmap(list(adj_r2, fstat, nobs), format_lm_statistics, digits = digits)
	res <- suppressMessages(combine_results(plm_res_tidy, plm_stat_tidy))
	
	return(res)
	
}

# function to format correlation output (by Hmisc::rcorr)
format_cor_output <- function(cor_mat, p_mat, mat_type = "full", diag = TRUE, digits = 4) {
	
	dims <- dim(cor_mat)
	nms <- rownames(cor_mat)
	
	res <- tibble::tibble(
		"r" = cor_mat |> as.numeric(),
		"p.value" = p_mat |> as.numeric()
	) |> 
		dplyr::mutate(stars = extract_significance(p.value)) |> 
		dplyr::mutate(text = paste0(round(r, digits), " ", stars)) |> 
		dplyr::pull(text) |> 
		matrix(nrow = dims[1], ncol = dims[2], dimnames = list(nms, nms))
	
	if (mat_type == "upper") {
		res[lower.tri(res)] <- ""  
	}
	if (mat_type == "lower") {
		res[upper.tri(res)] <- ""  
	}
	if (diag == FALSE) {
		diag(res) <- ""
	}
	
	return(res)
	
}

# function to format correlation matrix
# mat_type = c("full", "lower", "upper")
cor_tidy <- function(
		data,
		cor_type = "pearson",
		mat_type = "full",
		diag = TRUE,
		digits = 4
) {
	
	data_tmp <- data |> tidyr::drop_na() |> as.matrix() 
	cor_res <- Hmisc::rcorr(data_tmp, type = cor_type)
	res <- format_cor_output(cor_res$r, cor_res$P, mat_type, diag, digits)
	return(res)
	
}

# function to flatten correlation matrix
# flatten_cor_mat <- function(cormat, pmat) {
# 	ut <- upper.tri(cormat)
# 	df <- data.frame(
# 		row = rownames(cormat)[row(cormat)[ut]],
# 		column = rownames(cormat)[col(cormat)[ut]],
# 		cor = (cormat)[ut],
# 		p = pmat[ut]
# 	)
# 	return(df)
# }

# function to draw html table of lm results
dt_table <- function(data, title = "", caption = "", rownames = FALSE) {
	
	p_len <- nrow(data)
	res <- data |>
		DT::datatable(
			extensions = "Buttons",
			options = list(
				pageLength = p_len,
				paging = FALSE,
				searching = FALSE,
				ordering = FALSE,
				lenghtChange = FALSE,
				autoWidth = FALSE,
				dom = "Bfrtip",
				buttons = c("copy", "print", "csv", "excel", "pdf"),
				drawCallback = DT::JS(
					c(
						"function(settings){",
						"  var datatable = settings.oInstance.api();",
						"  var table = datatable.table().node();",
						paste0("  var caption = '", caption, "'"),
						"  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
						"}"
					)
				)
			),
			rownames = rownames,
			caption = title
		)
	return(res)
	
}
