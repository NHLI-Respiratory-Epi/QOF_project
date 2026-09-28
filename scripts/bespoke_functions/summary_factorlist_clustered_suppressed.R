# Clustered and disclosure-controlled version of finalfit::summary_factorlist.
# Based on https://github.com/ewenharrison/finalfit/blob/master/R/summaryfactorlist.R
# Run library(finalfit); source("summary_factorlist_clustered_suppressed.R")
# p-values use original unsuppressed observations. Suppression changes display only.
# Example: mydata %>% summary_factorlist_clustered("outcome", c("age", "sex"),
#                                                  p = TRUE, cluster = "pracid",
#                                                  suppress = TRUE)

.ff_cluster_p <- function(dat, dependent, explanatory, is_numeric, nonpara, cluster, digits) {
  dat <- as.data.frame(dat)
  keep <- stats::complete.cases(dat[, c(dependent, explanatory, cluster), drop = FALSE])
  dat <- dat[keep, , drop = FALSE]
  if (nrow(dat) < 2L || length(unique(dat[[cluster]])) < 2L)
    return(NA_character_)
  if (is.factor(dat[[dependent]])) dat[[dependent]] <- droplevels(dat[[dependent]])
  if (is.factor(dat[[explanatory]])) dat[[explanatory]] <- droplevels(dat[[explanatory]])
  if (length(unique(dat[[dependent]])) < 2L ||
      length(unique(dat[[explanatory]])) < 2L) return(NA_character_)
  des <- survey::svydesign(ids = stats::reformulate(cluster),
                           data = dat, weights = ~1)
  numeric_dependent <- is.numeric(dat[[dependent]])
  if (!numeric_dependent && !is_numeric) {
    pv <- survey::svychisq(stats::reformulate(c(explanatory, dependent)),
                           des, statistic = "F")$p.value
  } else if (nonpara && (numeric_dependent != is_numeric)) {
    # The outcome and predictor have different types: use the numeric variable
    # as the rank-test response and the categorical variable as its groups.
    y <- if (numeric_dependent) dependent else explanatory
    g <- if (numeric_dependent) explanatory else dependent
    pv <- survey::svyranktest(stats::reformulate(g, response = y), des,
                              test = "KruskalWallis")$p.value
  } else {
    y <- if (numeric_dependent) dependent else explanatory
    x <- if (numeric_dependent) explanatory else dependent
    if (numeric_dependent && is_numeric && nonpara) {
      des$variables[[y]] <- rank(des$variables[[y]], ties.method = "average")
      des$variables[[x]] <- rank(des$variables[[x]], ties.method = "average")
    }
    model <- survey::svyglm(stats::reformulate(x, response = y), des)
    pv <- survey::regTermTest(model, x)$p
  }
  finalfit:::p_tidy(pv, digits, "")
}

.ff_suppress_counts <- function(tab, dependent, explanatory) {
  # Called once for the complete explanatory-variable contingency table.
  # Percentages are computed downstream from n, so replace n before computing them.
  tab$.ff_hidden <- tab$n > 0 & tab$n < 10
  tab$.ff_hidden[is.na(tab$.ff_hidden)] <- FALSE
  tab$.ff_hidden_total <- FALSE
  if (any(tab$.ff_hidden)) {
    raw_totals <- tapply(tab$n, as.character(tab[[explanatory]]), sum)
    tab$.ff_hidden_total <- raw_totals[as.character(tab[[explanatory]])] > 0 &
                            raw_totals[as.character(tab[[explanatory]])] < 10
    # Half-up nearest-ten rounding of each individual cell, including hidden cells.
    # Hidden values remain hidden in the output; rounded counts set denominators.
    tab$n <- floor(tab$n / 10 + 0.5) * 10
  }
  tab
}

.ff_round_affected_row_totals <- function(tab, affected, total_name, missing_name,
                                          show_missing, var_labels, percent_digits = 1) {
  target <- dplyr::recode(affected, !!! var_labels)
  chosen <- tab$label %in% target & !is.na(tab[[total_name]]) &
            nzchar(tab[[total_name]])
  if (any(chosen)) {
    original_n <- as.numeric(sub(" .*", "", tab[[total_name]][chosen]))
    original_missing <- as.numeric(tab[[missing_name]][chosen])
    rounded_n <- floor(original_n / 10 + 0.5) * 10
    rounded_missing <- floor(original_missing / 10 + 0.5) * 10
    denom <- rounded_n + rounded_missing
    shown_n <- as.character(rounded_n)
    small_n <- original_n > 0 & original_n < 10
    shown_n[small_n] <- "<10"
    eligible <- !small_n & denom > 0
    shown_n[eligible] <- paste0(rounded_n[eligible], " (",
                                round(100 * rounded_n[eligible] / denom[eligible],
                                      percent_digits), ")")
    tab[[total_name]][chosen] <- shown_n
    shown_missing <- as.character(rounded_missing)
    shown_missing[original_missing > 0 & original_missing < 10] <- "<10"
    tab[[missing_name]][chosen] <- shown_missing
  }
  if (!show_missing) tab[[missing_name]] <- NULL
  tab
}

#' Summarise a set of factors (or continuous variables) by a dependent variable
#'
#' A function that takes a single dependent variable with a vector of
#' explanatory variable names (continuous or categorical variables) to produce a
#' summary table.
#'
#' This function aims to produce publication-ready summary tables for
#' categorical or continuous dependent variables. It usually takes a categorical
#' dependent variable to produce a cross table of counts and proportions
#' expressed as percentages or summarised continuous explanatory variables.
#' However, it will take a continuous dependent variable to produce mean
#' (standard deviation) or median (interquartile range) for use with linear
#' regression models.
#'
#' @param cluster Character column name for practice clustering; default `pracid`.
#'   Set to NULL for the original independent-observation p-values.
#' @param suppress Logical. For categorical outcomes, replace counts 1 to 9
#'   with `<10`; if a variable has any such count, round all its other
#'   contingency cells to tens and recalculate percentages from rounded cells.
#'   Statistical tests always use the original data.
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of dependent variable (2
#'   to 5 factor levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param formula an object of class "formula" (or one that can be coerced to 
#'   that class). Optional instead of standard dependent/explanatory format. 
#'   Do not include if using dependent/explanatory. 
#' @param cont Summary for continuous explanatory variables: "mean" (standard
#'   deviation) or "median" (interquartile range). If "median" then
#'   non-parametric hypothesis test performed (see below).
#' @param cont_nonpara Numeric vector of form e.g. \code{c(1,2)}. Specify which
#'   variables to perform non-parametric hypothesis tests on and summarise with
#'   "median".
#' @param cont_cut  Numeric: number of unique values in continuous variable at
#'   which to consider it a factor.
#' @param cont_range Logical. Median is show with 1st and 3rd quartiles.
#' @param p Logical: Include null hypothesis statistical test.
#' @param p_cont_para Character. Continuous variable parametric test. One of
#'   either "aov" (analysis of variance) or "t.test" for Welch two sample
#'   t-test. Note continuous non-parametric test is always Kruskal Wallis
#'   (kruskal.test) which in two-group setting is equivalent to Mann-Whitney U
#'   /Wilcoxon rank sum test.
#'
#'   For continous dependent and continuous explanatory, the parametric test
#'   p-value returned is for the Pearson correlation coefficient. The
#'   non-parametric equivalent is for the p-value for the Spearman correlation
#'   coefficient.
#' @param p_cat Character. Categorical variable test. One of either "chisq" or
#'   "fisher".
#' @param column Logical: Compute margins by column rather than row.
#' @param total_col Logical: include a total column summing across factor
#'   levels.
#' @param orderbytotal Logical: order final table by total column high to low.
#' @param digits Number of digits to round to (1) mean/median, (2) standard
#'   deviation / interquartile range, (3) p-value, (4) count percentage, 
#'   (5) weighted count.
#' @param na_include Logical: make explanatory variables missing data explicit
#'   (\code{NA}).
#' @param na_include_dependent Logical: make dependent variable missing data
#'   explicit.
#' @param na_complete_cases Logical: include only rows with complete data.
#' @param na_to_p Logical: include missing as group in statistical test.
#' @param na_to_prop Logical: include missing in calculation of column proportions.
#' @param fit_id Logical: allows merging via \code{\link{finalfit_merge}}.
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table.
#' @param dependent_label_prefix Add text before dependent label.
#' @param dependent_label_suffix Add text after dependent label.
#' @param add_col_totals Logical. Include column total n, unchanged by suppression.
#' @param include_col_totals_percent Include column percentage of total.
#' @param col_totals_rowname Logical. Row name for column totals.
#' @param col_totals_prefix Character. Prefix to column totals, e.g. "N=".
#' @param add_row_totals Logical. Include row totals. For variables with a
#'   suppressed cell, Total N and Missing N are rounded to tens and Total N's
#'   percentage is recalculated from rounded counts. This differs from
#'   \code{total_col} above particularly for continuous explanatory variables.
#' @param include_row_totals_percent Include row percentage of total.
#' @param include_row_missing_col Logical. Include missing data total for each
#'   row. Only used when \code{add_row_totals} is \code{TRUE}.
#' @param row_totals_colname Character. Column name for row totals.
#' @param row_missing_colname Character. Column name for missing data totals for
#'   each row.
#' @param weights Character vector of length 1: name of column to use for weights. 
#' Explanatory continuous variables are multiplied by weights. 
#' Explanatory categorical variables are counted with a frequency weight (sum(weights)).
#' @param catTest Deprecated. See \code{p_cat} above.
#' 
#' @return Returns a \code{factorlist} dataframe.
#'
#' @family finalfit wrappers
#' @seealso \code{\link{fit2df}} \code{\link{ff_column_totals}}
#'   \code{\link{ff_row_totals}} \code{\link{ff_label}} \code{\link{ff_glimpse}}
#'   \code{\link{ff_percent_only}}. For lots of examples, see \url{https://finalfit.org/}
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' # Load example dataset, modified version of survival::colon
#' data(colon_s)
#'
#' # Table 1 - Patient demographics ----
#' explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
#' dependent = "perfor.factor"
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory, p=TRUE)
#'
#' # summary.factorlist() is also commonly used to summarise any number of
#' # variables by an outcome variable (say dead yes/no).
#'
#' # Table 2 - 5 yr mortality ----
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory)
summary_factorlist_clustered <- function(.data, 
															 dependent = NULL, explanatory = NULL, 
															 formula = NULL, 
															 cont = "mean", cont_nonpara = NULL, cont_cut = 5, cont_range = TRUE, 
															 p = FALSE, p_cont_para = "aov", p_cat = "chisq",
															 column = TRUE, total_col = FALSE, orderbytotal = FALSE,
															 digits = c(1, 1, 3, 1, 0), 
															 na_include = FALSE, na_include_dependent = FALSE, 
															 na_complete_cases = FALSE, na_to_p = FALSE, na_to_prop = TRUE,
															 fit_id = FALSE,
															 add_dependent_label = FALSE,  
															 dependent_label_prefix = "Dependent: ", dependent_label_suffix = "",
															 add_col_totals = FALSE, include_col_totals_percent = TRUE,
															 col_totals_rowname = NULL, col_totals_prefix = "",
															 add_row_totals = FALSE, include_row_totals_percent = TRUE,
															 include_row_missing_col = TRUE,
															 row_totals_colname = "Total N", row_missing_colname = "Missing N",
															 catTest = NULL,
															 weights = NULL, cluster = "pracid", suppress = TRUE){
	
	# Formula interface -----------------
	## Added at request
	if(!is.null(formula) & (!is.null(dependent) | !is.null(explanatory))) stop("Formula OR dependent/explanatory terms, not both")
	if(!is.null(formula)){
		.terms = ff_parse_formula(formula)
		dependent = .terms$dependent
		explanatory = .terms$explanatory
	}
	
	# Warnings/Checks --------------
	if(!is.data.frame(.data)) stop(".data is not dataframe")
	if (isTRUE(suppress) && !is.null(weights)) stop("Suppression requires unweighted counts (weights = NULL).")
	if (!is.null(cluster) && (length(cluster) != 1L || !is.character(cluster) ||
		is.na(cluster) || !cluster %in% names(.data))) stop("cluster must name one column in .data, or be NULL.")

	if(any(class(.data) %in% c("tbl_df", "tbl", "data.table"))) .data = data.frame(.data)
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(any(explanatory == ".")){
		explanatory = .data %>% 
			dplyr::select(-dependent) %>% 
			names()
	}
	if(is.null(dependent)){
		message("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		.data$all = factor(1, labels="all")
	}
	if(na_to_p & !na_include) warning("If wish to pass missing to hypothesis test (na_to_p), must have na_include = TRUE")
	if(!is.null(weights) & p) {
		warning("Hypothesis tests (probably) not valid for weighted data. Setting p = FALSE")
		p = FALSE}
	
	# Deprecated catTest from Hmisc for reverse dependencies
	if(!is.null(catTest)){
		message("catTest is deprecated. Using p_cat = 'fisher'")
		p_cat = "fisher"}
	
	# Extract explanatory terms (to support using * and :)
	explanatory_terms = paste("~", paste(explanatory, collapse = "+")) %>% 
		formula() %>% 
		terms() %>% 
		attr("term.labels")
	
	if(dependent %in% explanatory) stop("Cannot have dependent variable in explanatory list.")
	
	if(!is.null(cont_nonpara) && max(cont_nonpara) > length(explanatory)) {
		stop("cont_nonpara cannot include values greater than the number of explanatory variables")
	}
	
	# Definitions ------------------------------------------------------------
	## Dependent as survival object handling
	d_is.surv = grepl("Surv[(].*[)]", dependent)
	
	if(d_is.surv){
		message("Dependent variable is a survival object")
		.data$all = factor(1, labels="all")
		dependent = "all"
		
		# Remove strata and cluster terms - keep in table for now
		drop = grepl("cluster[(].*[)]", explanatory) |
			grepl("strata[(].*[)]", explanatory) |
			grepl("frailty[(].*[)]", explanatory)
		explanatory = explanatory[!drop]
	}    
	
	# Remove interactions and indicator variables
	## Intentionally done separately to above line. 
	explanatory = paste("~", paste(explanatory, collapse = "+")) %>% 
		formula() %>% 
		all.vars()
	
	## Dependent is numeric
	d_is.numeric = .data %>% 
		dplyr::pull(dependent) %>% 
		is.numeric()
	
	if (isTRUE(suppress) && d_is.numeric) stop("Suppression is supported for categorical dependent variables only.")
	if(d_is.numeric & add_col_totals){
		add_col_totals = FALSE
		message("Cannot have add_col_totals with numeric dependent.")
	}
	
	## Continuous data to categorical if unique values below threshold
	cont_distinct = .data %>%
		dplyr::select(explanatory) %>% 
		dplyr::summarise_if(is.numeric, dplyr::n_distinct) %>% 
		purrr::keep(~ .x < cont_cut) %>% 
		names()
	.data = .data %>% 
		dplyr::mutate_at(cont_distinct, as.factor) %>% 
		ff_relabel_df(.data)
	
	## Explanatory variable type
	explanatory_type = .data %>% 
		dplyr::select(explanatory) %>% 
		purrr::map(is.numeric)
	
	# Non-parametric variables
	explanatory_nonpara = vector(length = length(explanatory))
	explanatory_nonpara[cont_nonpara] = TRUE
	if(cont == "median") explanatory_nonpara = TRUE
	
	## Labels
	var_labels = .data %>% 
		dplyr::select(explanatory) %>% 
		extract_variable_label()
	
	## Weights
	is_weighted = ifelse(is.null(weights), FALSE, TRUE)
	
	# Missing data handling ------------------------------------------------------------
	df.in = .data
	
	# Explanatory variables, make NA explicit for factors
	if(na_complete_cases){
		df.in = df.in %>% 
			tidyr::drop_na()
	}
	
	if(na_include){
		df.in = df.in %>% 
			dplyr::mutate_if(names(.) %in% unlist(explanatory) & 
											 	sapply(., is.factor),
											 	forcats::fct_na_value_to_level, level = "(Missing)"
			)}
	
	if(na_include_dependent & !d_is.numeric){
		df.in = df.in %>% 
			dplyr::mutate(
				!! sym(dependent) := forcats::fct_na_value_to_level(!! sym(dependent), level = "(Missing)")
			)
	} else if(!na_include_dependent & !d_is.numeric){
		df.in = df.in %>% 
			tidyr::drop_na(dependent)
	} else if(na_include_dependent & d_is.numeric){
		warnings("Dependent is numeric and missing values cannot be made explicit. 
							 Make dependent a factor or use na_include_dependent = FALSE.")
	}
	affected <- character(0)
	if (isTRUE(suppress) && !d_is.numeric) {
		for (v in explanatory[!unlist(explanatory_type)]) {
			counts <- table(df.in[[dependent]], df.in[[v]])
			if (any(counts > 0 & counts < 10)) affected <- c(affected, v)
		}
	}
	
	## Missing data to p-tests or not 
	if(na_to_p){
		df.p = df.in
	} else {
		df.p = .data
	}
	if (p && !is.null(cluster) && !requireNamespace("survey", quietly = TRUE))
		stop("Install the survey package to calculate clustered p-values.")
	if (p && !is.null(cluster) && p_cat == "fisher")
		warning("Clustered Fisher tests are unavailable; using Rao-Scott tests instead.")
	if(p && na_to_p){
		message("Explanatory variable(s) missing data included in hypothesis test (p-value).")
	}
	
	if(!na_include_dependent &
		 .data %>% 
		 dplyr::pull(dependent) %>% 
		 is.na() %>% 
		 any()) {message("Note: dependent includes missing data. These are dropped.")}
	
	
	# Continuous dependent --------------------------------------------------------------------
	if(d_is.numeric){
		
		## Hypothesis tests ---------
		if(p){
			if (!is.null(cluster)) {
				p_tests = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara),
					~ .ff_cluster_p(df.p, dependent, ..1, ..2, ..3, cluster, digits[3]))
			} else {
			p_tests =  purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
														 # Categorical / parametric
														 ~ if(!..2 && !..3){
														 	if(p_cont_para == "aov"){
														 		summary(aov(as.formula(paste(dependent, "~", ..1)), df.p))[[1]][["Pr(>F)"]][[1]] %>% 
														 			p_tidy(digits[3], "")
														 	} else if (p_cont_para == "t.test"){
														 		t.test(as.formula(paste(dependent, "~", ..1)), df.p)$p.value %>% 
														 			p_tidy(digits[3], "")
														 	}
														 	# Categorical / non-parametric
														 } else if (!..2 & ..3){
														 	kruskal.test(as.formula(paste(dependent, "~", ..1)), df.p)$p.value %>% 
														 		p_tidy(digits[3], "") 
														 	# Continous / parametric
														 } else if (..2 & !..3){
														 	cor.test(as.formula(paste("~", dependent, "+", ..1)), df.p, method="pearson")$p.value %>% 
														 		p_tidy(digits[3], "") 
														 	# Continous / non-parametric
														 } else if (..2 & ..3){
														 	cor.test(as.formula(paste("~", dependent, "+", ..1)), df.p, method="spearman")$p.value %>% 
														 		p_tidy(digits[3], "") 
														 }
			)
			}
		}  
		
		summary_cont_name = rep("Mean (sd)", length(explanatory_nonpara))
		summary_cont_name[explanatory_nonpara] = "Median (IQR)"
		
		## Output table  --------------  
		df.out = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara, summary_cont_name), 
												 ~ if(!..2){
												 	df.in %>% 
												 		dplyr::group_by(!! sym(..1)) %>%  
												 		tidyr::drop_na(!! sym(dependent)) %>% 
												 		dplyr::summarise(value_mean = mean(!! sym(dependent), na.rm = TRUE),
												 										 value_sd = sd(!! sym(dependent), na.rm = TRUE),
												 										 value_median = median(!! sym(dependent), na.rm = TRUE),
												 										 value_q1 =quantile(!! sym(dependent), 0.25, na.rm = TRUE),
												 										 value_q3 = quantile(!! sym(dependent), 0.75, na.rm = TRUE),
												 										 n = dplyr::n()) %>% 
												 		tidyr::drop_na() %>% 
												 		dplyr::ungroup() %>% 
												 		dplyr::mutate(
												 			col_total = sum(n),
												 			col_total_prop = 100 * n/col_total,
												 			Total = format_n_percent(n, col_total_prop, digits[4], digits[5]),
												 			label = ..1,
												 			unit = ..4,
												 		)  %>% 
												 		dplyr::rename(levels = 1) %>% 
												 		{ if(! ..3){
												 			dplyr::mutate(., 
												 										value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																	 value_sd %>% round_tidy(digits[1]), ")")
												 			)
												 		} else {
												 			{ if(cont_range){
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_q1 %>% round_tidy(digits[1]), " to ",
												 																		 value_q3 %>% round_tidy(digits[1]), ")")
												 				) 
												 			} else {
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 {value_q3 - value_q1} %>% round_tidy(digits[1]), ")")
												 				)
												 			}}
												 			
												 		}} %>% 
												 		{if(total_col){
												 			dplyr::select(., label, levels, unit, value, Total)
												 		} else {
												 			dplyr::select(., label, levels, unit, value)
												 		}}  %>% 
												 		dplyr::mutate_all(as.character)
												 } else if(..2){
												 	df.in %>% 
												 		tidyr::drop_na(!! sym(dependent)) %>% 
												 		dplyr::summarise(value_mean = mean(!! sym(dependent), na.rm = TRUE),
												 										 value_sd = sd(!! sym(dependent), na.rm = TRUE),
												 										 value_median = median(!! sym(dependent), na.rm = TRUE),
												 										 value_q1 =quantile(!! sym(dependent), 0.25, na.rm = TRUE),
												 										 value_q3 = quantile(!! sym(dependent), 0.75, na.rm = TRUE),
												 										 value_min = min(!! sym(..1), na.rm = TRUE),
												 										 value_max = max(!! sym(..1), na.rm = TRUE),
												 										 n = (!is.na(!! sym(..1))) %>% sum(),
												 										 Total = format_n_percent(n, 100, digits[4], digits[5])) %>%
												 		dplyr::mutate(
												 			label = ..1,
												 			levels = paste0("[", value_min %>% round_tidy(digits[1]), ",", 
												 											value_max %>% round_tidy(digits[1]), "]"),
												 			unit = ..4
												 		) %>% 
												 		{ if(! ..3){
												 			dplyr::mutate(., 
												 										value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																	 value_sd %>% round_tidy(digits[1]), ")")
												 			)
												 		} else {
												 			{ if(cont_range){
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_q1 %>% round_tidy(digits[1]), " to ",
												 																		 value_q3 %>% round_tidy(digits[1]), ")")
												 				) 
												 			} else {
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 {value_q3 - value_q1} %>% round_tidy(digits[1]), ")")
												 				)
												 			}}
												 			
												 		}} %>% 
												 		{if(total_col){
												 			dplyr::select(., label, levels, unit, value, Total)
												 		} else{
												 			dplyr::select(., label, levels, unit, value)
												 		}}  %>% 
												 		dplyr::mutate_all(as.character)
												 }
		)
	} else {
		
		# Categorical dependent -----------------------------------------------------------------------------
		
		## Hypothesis tests ---------
		if(p){
			if (!is.null(cluster)) {
				p_tests = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara),
					~ .ff_cluster_p(df.p, dependent, ..1, ..2, ..3, cluster, digits[3]))
			} else {
			p_tests =  purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
														 ~ if(!..2){
														 	df.p %>%
														 		{ if(p_cat == "chisq"){
														 			dplyr::summarise(., chisq.test(!! sym(..1), !! sym(dependent))$p.value) %>% 
														 				p_tidy(digits[3], "")
														 		} else if (p_cat == "fisher"){
														 			dplyr::summarise(., fisher.test(!! sym(..1), !! sym(dependent))$p.value) %>% 
														 				p_tidy(digits[3], "")
														 		}}
														 } else if (..2 & !..3){
														 	{if (p_cont_para == "aov"){
														 		summary(aov(as.formula(paste(..1, "~", dependent)), df.p))[[1]][["Pr(>F)"]][[1]] %>% 
														 			p_tidy(digits[3], "")
														 	} else if (p_cont_para == "t.test"){
														 		t.test(as.formula(paste(..1, "~", dependent)), df.p)$p.value %>% 
														 			p_tidy(digits[3], "")
														 	}}
														 } else if (..2 & ..3){
														 	kruskal.test(as.formula(paste(..1, "~", dependent)), df.p)$p.value %>% 
														 		p_tidy(digits[3], "") 
														 }
			)
			}
		}
		
		
		## Output table  --------------  
		df.out = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
												 ~ if(!..2){
												 	df.in %>% 
												 		dplyr::group_by(!! sym(dependent)) %>% 
												 		{ if(is.null(weights)){
												 			dplyr::count(., !! sym(..1), .drop = FALSE)
												 		} else {
												 			dplyr::count(., !! sym(..1), .drop = FALSE, wt = !! sym(weights))
												 		}
												 		} %>%  
												 		dplyr::ungroup() %>% 
												 		tidyr::drop_na() %>% 
												 		{ if (isTRUE(suppress)) .ff_suppress_counts(., dependent, ..1) else . } %>%
												 		{ if(na_to_prop) {
												 			dplyr::mutate(., grand_total = sum(n))
												 		} else {
												 			dplyr::mutate(., grand_total = sum(n[.[[2]] != "(Missing)"], na.rm = TRUE))
												 		}
												 		} %>% 
												 		dplyr::group_by_at(2) %>% 
												 		dplyr::mutate(row_total = sum(n),
												 									col_total_prop = 100 * row_total / grand_total) %>% 
												 		{ if(column) {
												 			dplyr::group_by(., !! sym(dependent)) %>% 
												 				# Choose to include missing in column proportions 
												 				{ if(na_to_prop) {
												 					dplyr::mutate(., 
												 												col_total = sum(n),
												 												prop = 100 * n / col_total,
												 												Total = format_n_percent(row_total, col_total_prop, digits[4], digits[5])
												 					)
												 				} else {
												 					dplyr::mutate(., 
												 												col_total = sum(n[.[[2]] != "(Missing)"], na.rm = TRUE),
												 												prop = 100 * n / col_total,
												 												prop = if_else(!! sym(names(.)[2]) == "(Missing)", NA_real_, prop),
												 												col_total_prop = if_else(!! sym(names(.)[2]) == "(Missing)", 
												 																								 NA_real_, col_total_prop),
												 												Total = format_n_percent(row_total, col_total_prop, digits[4], digits[5], 
												 																								 na_include = FALSE)
												 					)}
												 				} %>% 
												 				dplyr::select(-col_total)
												 		} else { 
												 			dplyr::group_by_at(., 2) %>% 
												 				dplyr::mutate(
												 					prop = 100 * n / row_total,
												 					Total = paste0(row_total, " (100)")
												 				)
												 		}
												 		} %>% 
												 		dplyr::ungroup() %>% 
												 		dplyr::mutate(
												 			value = format_n_percent(n, prop, digits[4], digits[5], na_include = FALSE),
												 		value = if (isTRUE(suppress)) ifelse(.ff_hidden, "<10", value) else value,
												 		Total = if (isTRUE(suppress)) ifelse(.ff_hidden_total, "<10", Total) else Total
												 		) %>%
												 		dplyr::select(-prop, -n, -grand_total, -col_total_prop, -dplyr::any_of(c(".ff_hidden", ".ff_hidden_total"))) %>% 
												 		tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 		dplyr::mutate(
												 			label = names(.)[1]
												 		) %>%
												 		dplyr::rename(levels = 1) %>% 
												 		{if(orderbytotal){
												 			dplyr::arrange(., -row_total) 
												 		} else {
												 			.
												 		}} %>% 
												 		dplyr::select(-row_total) %>%     
												 		dplyr::select(label, levels, dplyr::everything()) %>% 
												 		dplyr::select(-Total, dplyr::everything()) %>% 
												 		dplyr::mutate_all(as.character) %>% 
												 		# Total column
												 		{ if(total_col){
												 			.
												 		} else {
												 			dplyr::select(., -Total)
												 		}
												 		}
												 } else {
												 	df.in %>% 
												 		{ if(!is_weighted){
												 			dplyr::mutate(., 
												 										value_mean_total = mean(!! sym(..1), na.rm = TRUE),
												 										value_sd_total = sd(!! sym(..1), na.rm = TRUE),
												 										value_median_total = median(!! sym(..1), na.rm = TRUE),
												 										value_q1_total = quantile(!! sym(..1), 0.25, na.rm = TRUE),
												 										value_q3_total = quantile(!! sym(..1), 0.75, na.rm = TRUE)
												 			) %>%
												 				dplyr::group_by(!! sym(dependent)) %>% 
												 				dplyr::summarise(
												 					value_mean = mean(!! sym(..1), na.rm = TRUE),
												 					value_sd = sd(!! sym(..1), na.rm = TRUE),
												 					value_median = median(!! sym(..1), na.rm = TRUE),
												 					value_q1 = quantile(!! sym(..1), 0.25, na.rm = TRUE),
												 					value_q3 = quantile(!! sym(..1), 0.75, na.rm = TRUE),
												 					value_iqr = value_q3 - value_q1,
												 					value_mean_total = unique(value_mean_total),
												 					value_sd_total = unique(value_sd_total),
												 					value_median_total = unique(value_median_total),
												 					value_q1_total = unique(value_q1_total),
												 					value_q3_total = unique(value_q3_total),
												 					value_iqr_total = value_q3_total - value_q1_total
												 				)
												 		} else {
												 			dplyr::mutate(., 
												 										value_mean_total = Hmisc::wtd.mean(!! sym(..1), weights = !! sym(weights), na.rm = TRUE),
												 										value_sd_total = sqrt(Hmisc::wtd.var(!! sym(..1),  weights = !! sym(weights), na.rm = TRUE)),
												 										value_median_total = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.5, na.rm = TRUE),
												 										value_q1_total = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.25, na.rm = TRUE),
												 										value_q3_total = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.75, na.rm = TRUE),
												 			) %>%
												 				dplyr::group_by(!! sym(dependent)) %>% 
												 				dplyr::summarise(
												 					value_mean = Hmisc::wtd.mean(!! sym(..1), weights = !! sym(weights), na.rm = TRUE),
												 					value_sd = sqrt(Hmisc::wtd.var(!! sym(..1),  weights = !! sym(weights), na.rm = TRUE)),
												 					value_median = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.5, na.rm = TRUE),
												 					value_q1 = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.25, na.rm = TRUE),
												 					value_q3 = Hmisc::wtd.quantile(!! sym(..1), weights = !! sym(weights), probs = 0.75, na.rm = TRUE),
												 					value_iqr = value_q3 - value_q1,
												 					value_mean_total = unique(value_mean_total),
												 					value_sd_total = unique(value_sd_total),
												 					value_median_total = unique(value_median_total),
												 					value_q1_total = unique(value_q1_total),
												 					value_q3_total = unique(value_q3_total),
												 					value_iqr_total = value_q3_total - value_q1_total
												 				)
												 			
												 			
												 		}} %>% 
												 				{ if(! ..3) {
												 					dplyr::mutate(., 
												 												value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																			 value_sd %>%  round_tidy(digits[2]), ")") ,
												 												Total = paste0(value_mean_total %>% round_tidy(digits[1]), " (", 
												 																			 value_sd_total %>%  round_tidy(digits[2]), ")") 
												 					) %>% 
												 						dplyr::select(dependent, value, Total) %>% 
												 						tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 						dplyr::mutate(
												 							label = .x,
												 							levels = "Mean (SD)"
												 						) 
												 				} else if (..3){
												 					{if(cont_range){
												 						dplyr::mutate(., 
												 													value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																				 value_q1 %>% round_tidy(digits[2]), " to ",
												 																				 value_q3 %>% round_tidy(digits[2]), ")"), 
												 													Total = paste0(value_median_total %>% round_tidy(digits[1]), " (", 
												 																				 value_q1_total %>% round_tidy(digits[2]), " to ",
												 																				 value_q3_total %>% round_tidy(digits[2]), ")") 
												 						)
												 					} else {
												 						dplyr::mutate(., 
												 													value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																				 value_iqr %>% round_tidy(digits[2]), ")"), 
												 													Total = paste0(value_median_total %>% round_tidy(digits[1]), " (", 
												 																				 value_iqr_total %>% round_tidy(digits[2]), ")") 
												 						)
												 					}} %>% 
												 						dplyr::select(dependent, value, Total) %>% 
												 						tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 						dplyr::mutate(
												 							label = .x,
												 							levels = "Median (IQR)"
												 						) 
												 				}
												 				} %>% 
												 				dplyr::select(label, levels, dplyr::everything()) %>% 
												 				dplyr::select(-Total, dplyr::everything()) %>% 
												 				# Total column
												 				{ if(total_col){
												 					.
												 				} else {
												 					dplyr::select(., -Total)
												 				}
												 				}
												 		}
		) 
												 }
		df.out = df.out %>% 
			# Add hypothesis test
			{ if(p){
				purrr::map2_df(., p_tests,
											 ~ dplyr::mutate(.x,
											 								p = .y)
				)} else {
					dplyr::bind_rows(.)
				}} %>%
			dplyr::select(label, levels, dplyr::everything()) %>% 
			as.data.frame() %>%
			{ if(fit_id){
				levels_id = .$levels
				# Catagorical outcome, continous explanatory
				drop = levels_id %in% c("Mean (SD)", "Median (IQR)")
				levels_id[drop] = ""
				# Continuous outcome, continuous explanatory
				regex_sqbracket = "^(\\[).*(\\])$"
				drop = grepl(regex_sqbracket, levels_id)
				levels_id[drop] = ""
				# Where extra terms included, add these in, e.g. I(var) (not interactions)
				extra_terms = explanatory_terms[-which(explanatory_terms %in% explanatory)]
				drop = grepl(":", extra_terms)
				extra_terms = extra_terms[!drop]
				{ if(!identical(extra_terms, character(0))){
					levels_id = c(levels_id, rep("", length(extra_terms)))
					dplyr::add_row(., label = extra_terms)
				} else {
					.
				}} %>% 
					dplyr::mutate(., fit_id = paste0(label, levels_id),
												index = 1:dim(.)[1])
			} else {
				.
			}} %>% 
			
			# Recode variable names to labels where available 
			dplyr::mutate(
				label = dplyr::recode(label,  !!! var_labels)
			) %>% 
			
			# Remove duplicate variables/p-values
			rm_duplicate_labels() %>% 
			
			# Add column totals
			{ if(add_col_totals){
				ff_column_totals(., df.in, dependent, 
												 percent = include_col_totals_percent, 
												 na_include_dependent = na_include_dependent,
												 digits = digits[c(4, 5)], label = col_totals_rowname, 
												 prefix = col_totals_prefix, weights = weights)
			} else {
				.
			}} %>% 
			
			# Add row totals
			{ if(add_row_totals){
				ff_row_totals(., .data, dependent, explanatory,
											missing_column = include_row_missing_col ||
												(isTRUE(suppress) && length(affected) > 0),
											na_include_dependent = FALSE, na_complete_cases = na_complete_cases, 
											total_name = row_totals_colname, na_name = row_missing_colname) %>%
					{ if (isTRUE(suppress) && length(affected) > 0)
						.ff_round_affected_row_totals(., affected, row_totals_colname,
							row_missing_colname, include_row_missing_col, var_labels)
				  else . }
			} else {
				.
			}} %>% 
			
			
			# Add dependent label
			{ if(add_dependent_label){
				dependent_label(., .data, dependent, 
												prefix=dependent_label_prefix, suffix = dependent_label_suffix)
			} else {
				.
			}} %>% 
			
			# Replace any missing values with "", e.g. in (Missing) column
			dplyr::mutate_all(.,
												~ ifelse(is.na(.), "", .)
			)
		class(df.out) = c("data.frame.ff", class(df.out))
		return(df.out)
	}
	
	
	

# Resolve finalfit's internal formatting helpers without requiring exported bindings.
environment(summary_factorlist_clustered) <- new.env(parent = asNamespace("finalfit"))
environment(summary_factorlist_clustered)$.ff_cluster_p <- .ff_cluster_p
environment(summary_factorlist_clustered)$.ff_suppress_counts <- .ff_suppress_counts
environment(summary_factorlist_clustered)$.ff_round_affected_row_totals <- .ff_round_affected_row_totals
