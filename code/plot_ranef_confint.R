ranef_data <- ranef(m$aslt$nbhd_sch_ml, condVar = TRUE) %>% 
	as_tibble() %>% 
	filter(term == "schoolTRUE") %>% 
	rename(estimate = condval, label = grp) %>% 
	mutate(
		conf.low = estimate - (1.96 * condsd), 
		conf.high = estimate + (1.96 * condsd)
	) %>% 
	mutate_at(vars(one_of("estimate", "conf.low", "conf.high")), exp) %>% 
	mutate(
		# calculate end of leader lines
		position = ifelse(estimate < 1, conf.high, conf.low),
		# determine alignment of term labels
		align = ifelse(estimate < 1, "left", "right"),
		# determine significance of each term
		significance = ifelse(((conf.low < 1 & conf.high < 1) | (conf.low > 1 & conf.high > 1)), "p < 0.05", "n.s."),
		x_position = ifelse(significance == "n.s.", ifelse(estimate < 1, conf.high, conf.low), 1),
	) %>% 
	mutate(
		# add a space at the end of the label closest to the centre line
		# this is necessary because the geom_text() nudge parameter only takes a
		# single value for the whole chart
		label = ifelse(estimate < 1, paste0("  ", label), paste0(label, "  "))
	)

bigger_lim <- max(max(ranef_data$conf.high), max(1 / ranef_data$conf.low))
x_lim <- c(1 / bigger_lim, bigger_lim)

sig_col <- c("#999999", "#000000")
names(sig_col) <- c("n.s.", "p < 0.05")

# plot confidence intervals
ggplot(
	data = ranef_data,
	mapping = aes(x = estimate, y = label, colour = significance)
) +
	geom_segment(aes(x = position, xend = 1, y = label, yend = label), 
							 colour = "#999999", linetype = 2) +
	geom_vline(xintercept = 1) +
	geom_point(aes(x = estimate), size = 3, colour = "#FFFFFF") +
	geom_segment(aes(x = conf.low, xend = conf.high, y = label, yend = label)) +
	geom_point(aes(x = estimate), size = 2) +
	geom_text(aes(x = x_position, label = label, hjust = align), 
						colour = "#666666", 
						# for the origin of this magic number (err, ratio), see
						# https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size
						size = 12 / (1/0.352777778)) +
	scale_x_log10(
		name = "odds ratio (log scale)",
		limits = x_lim,
		breaks = function (x) pretty(x, n = 5)
	) +
	scale_y_discrete(name = NULL, limits = rev(ranef_data$label)) +
	scale_colour_manual(values = sig_col, drop = FALSE) +
	theme_my +
	theme(
		legend.position = c(0, 1),
		legend.justification = c(0, 1)
	)


