# Function to plot raw eEPSC amplitudes vs. time
# dataframe should be the raw dataframe
# category is:
# 1: HFS with no modifications
# 2: eEPSCs after adding insulin
# 3: HFS in the presence of insulin

make_raw_plots <-
  function(dataframe,
           treatment,
           category,
           color_choice,
           ...) {
    # Define list to store plots
    x <- list()
    
    # Filter df to only include cells belonging to a specific category
    df <-
      dataframe %>% filter(Category == category) %>% filter(Treatment == treatment)
    
    # Get a list of all cells in this category
    letters <- as.character(unique(unlist(df$Letter)))
    
    # Make plots for each letter
    for (i in letters) {
      plot_df <- df %>% filter(Letter == i)
      x[[i]] <- ggplot(plot_df, aes(x = Time, y = P1)) +
        ggtitle(paste("Recording", i),
                subtitle = paste(
                  "Treatment:",
                  unique(plot_df$Treatment),
                  " Sex:",
                  unique(plot_df$Sex)
                )) +
        labs(y = "eEPSC Amplitude (pA)", x = "Time (min)")
      
      # Add points which are coloured by sex:
      if (unique(plot_df$Sex) == "Male") {
        x[[i]] <- x[[i]] +
          geom_point(
            shape = 16,
            colour = my_colours[color_choice],
            size = 3.5,
            alpha = 0.7
          )
      } else {
        x[[i]] <- x[[i]] +
          geom_point(
            shape = 15,
            colour = my_colours[color_choice],
            size = 3.5,
            alpha = 0.7
          )
      }
      
      # If category = 2 (an experiment where insulin was added), do the following:
      if (category == 2) {
        # Get limits of x- and y-axes
        ymax <- layer_scales(x[[i]])$y$get_limits()[2]
        xmax <- layer_scales(x[[i]])$x$get_limits()[2]
        ymax2 <- layer_scales(x[[i]])$y$get_limits()[2]
        
        # Add line showing times when insulin was applied
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "segment",
            x = 5,
            xend = xmax,
            y = ymax + 0.1 * ymax,
            yend = ymax + 0.1 * ymax,
            colour = line_col,
            linewidth = 0.6
          )
        
        # Add "Insulin" label
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "text",
            x = 5,
            y = ymax2 + 0.16 * ymax2,
            label = "Insulin",
            size = 4,
            hjust = 0,
            family = plot_font_family
          )
      }
      
      # If category = 1 or 3 (experiments involving HFS) do the following:
      if (category == 1 |
          category == 3) {
        # Get limits of x- and y-axes
        ymax <- layer_scales(x[[i]])$y$get_limits()[2]
        xmax <- layer_scales(x[[i]])$x$get_limits()[2]
        ymax2 <- layer_scales(x[[i]])$y$get_limits()[2]
        
        # Add an arrow showing when HFS was applied (x = 5 min)
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "segment",
            x = 5,
            y = ymax + 0.22 * ymax,
            xend = 5,
            yend = ymax + 0.10 * ymax,
            arrow = arrow(type = "closed", length = unit(0.02, "npc"))
          )
        
        # Add "HFS" text label
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "text",
            x = 5,
            y = ymax + 0.27 * ymax,
            label = "HFS",
            size = 3.5,
            hjust = 0.5,
            family = plot_font_family
          )
      }
      
      # Save plots to a subfolder called 'Output-individual-plots'
      if (save_choice == "yes") {
        ggsave(
          x[[i]],
          path = here("Figures", "Output-individual-plots"),
          file = paste0(i, ".png"),
          width = 7,
          height = 5,
          units = "in",
          dpi = 300
        )
      }
    }
    
    # Assign plot list to global variable to access later
    assign(paste0("Individual_raw_plots_", treatment, "_category_", category),
           x,
           envir = .GlobalEnv)
  }


# Function to make individual plots of eEPSCs vs. Time
# These plots are "pruned" to display one point per minute
# It uses the mean eEPSC amplitude per minute
# dataframe should contained pruned data for individual cells
# category is:
# 1: HFS with no modifications
# 2: eEPSCs after adding insulin
# 3: HFS in the presence of insulin
make_individual_pruned_plots <-
  function(data,
           treatment,
           category,
           color_choice,
           ...) {
    # Define list to store plots
    x <- list()
    
    # Filter df to only include cells belonging to a specific category
    df <-
      data %>% filter(Category == category) %>% filter(Treatment == treatment)
    
    # Get a list of all cells in this category
    letters <- as.character(unique(unlist(df$Letter)))
    
    # Make plots for each letter
    for (i in letters) {
      plot_df <- df %>% filter(Letter == i)
      x[[i]] <- ggplot(plot_df, aes(x = Time,
                                    y = mean_P1)) +
        ggtitle(paste("Recording", i),
                subtitle = paste(
                  "Treatment:",
                  unique(plot_df$Treatment),
                  " Sex:",
                  unique(plot_df$Sex)
                )) +
        labs(y = "eEPSC Amplitude (pA)", x = "Time (min)")
      
      # Add points which are coloured by sex:
      if (unique(plot_df$Sex) == "Male") {
        x[[i]] <- x[[i]] +
          geom_point(
            shape = 16,
            colour = my_colours[color_choice],
            size = 3.5,
            alpha = 1
          )
      }
      else {
        x[[i]] <- x[[i]] +
          geom_point(
            shape = 15,
            colour = my_colours[color_choice],
            size = 3.5,
            alpha = 1
          )
      }
      
      # If category = 2 (an experiment where insulin was added), do the following:
      if (category == 2) {
        # Get limits of x- and y-axes
        ymax <- layer_scales(x[[i]])$y$get_limits()[2]
        xmax <- layer_scales(x[[i]])$x$get_limits()[2]
        ymax2 <- layer_scales(x[[i]])$y$get_limits()[2]
        
        # Add line showing times when insulin was applied
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "segment",
            x = 5,
            xend = xmax,
            y = ymax + 0.1 * ymax,
            yend = ymax + 0.1 * ymax,
            colour = line_col,
            linewidth = 0.6
          )
        
        # Add "Insulin" label
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "text",
            x = 5,
            y = ymax2 + 0.16 * ymax2,
            label = "Insulin",
            size = 4,
            hjust = 0,
            family = plot_font_family
          )
      }
      
      # If category = 1 or 3 (experiments involving HFS) do the following:
      if (category == 1 |
          category == 3) {
        # Get limits of x- and y-axes
        ymax <- layer_scales(x[[i]])$y$get_limits()[2]
        xmax <- layer_scales(x[[i]])$x$get_limits()[2]
        ymax2 <- layer_scales(x[[i]])$y$get_limits()[2]
        
        # Add an arrow showing when HFS was applied (x = 5 min)
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "segment",
            x = 5,
            y = ymax + 0.22 * ymax,
            xend = 5,
            yend = ymax + 0.10 * ymax,
            arrow = arrow(type = "closed", length = unit(0.02, "npc"))
          )
        
        # Add "HFS" text label
        x[[i]] <-
          x[[i]] +
          annotate(
            geom = "text",
            x = 5,
            y = ymax + 0.27 * ymax,
            label = "HFS",
            size = 3.5,
            hjust = 0.5,
            family = plot_font_family
          )
      }
      
      # Save plots to a subfolder called 'Output-individual-plots'
      if (save_choice == "yes") {
        ggsave(
          x[[i]],
          path = here("Figures", "Output-individual-plots"),
          file = paste0(i, "_pruned.png"),
          width = 7,
          height = 5,
          units = "in",
          dpi = 300
        )
      }
    }
    
    # Assign plot list to global variable to access later
    assign(paste0(
      "Individual_pruned_plots_",
      treatment,
      "_category_",
      category
    ),
    x,
    envir = .GlobalEnv)
  }

# Function to make summary plots of all cells within a particular experimental group.
# category is:
# 1: HFS with no modifications
# 2: eEPSCs after adding insulin
# 3: HFS in the presence of insulin
# pruned_df should be the pruned dataframe for all cells, not the pruned data for individual cells
# treatment is:
# "None", "HNMPA", etc.
# color_choice
# See colour palette options in the chunk 'set-ggplot2-theme'

make_pruned_summary_plots <- function(category,
                                      treatment,
                                      data,
                                      color_choice,
                                      representative_trace,
                                      ...) {
  df <-
    data %>% filter(Category == category) %>% filter(Treatment == treatment)
  
  treatment_plot <- df %>%
    ggplot(aes(
      x = Time,
      y = Mean_all,
      ymin = Mean_all - se,
      ymax = Mean_all + se
    )) +
    geom_pointrange(
      aes(color = Sex, shape = Sex),
      size = 0.9,
      alpha = 1,
      position = position_dodge(width = 0.3)
    ) +
    scale_shape_manual(values = c(17, 16),
                       labels = c((paste0(
                         "Females, n = ", df$n[df$Sex == "Female"][1]
                       )),
                       (paste0(
                         "Males, n = ", df$n[df$Sex == "Male"][1]
                       )))) +
    scale_color_manual(values = c(my_colours_very_pale[color_choice], my_colours[color_choice]),
                       labels = c((paste0(
                         "Females, n = ", df$n[df$Sex == "Female"][1]
                       )),
                       (paste0(
                         "Males, n = ", df$n[df$Sex == "Male"][1]
                       )))) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    coord_cartesian(ylim = c(0, y_axis_limit)) +
    labs(
      x = "Time (min)",
      y = "eEPSC \n(% baseline)",
      shape = "Sex",
      color = "Sex"
    ) +
    theme(axis.title.y = element_text(angle = 90))
  
  # Get limits of x-axis
  # The Y-axis is fixed using the 'y_axis_limit' variable in the set-up chunk,
  # so I specified 'ymax' relative to 'y_axis_limit'
  ymax <- y_axis_limit - 25
  xmax <-
    layer_scales(treatment_plot)$x$get_limits()[2]
  
  
  # If category = 2 (an experiment where insulin was added)
  # add a line showing the times when insulin was applied:
  if (category == 2) {
    treatment_plot <-
      treatment_plot +
      annotate(
        geom = "segment",
        x = 5,
        xend = xmax,
        y = ymax,
        yend = ymax,
        colour = line_col,
        linewidth = 0.5
      ) +
      # Add "Insulin" label
      annotate(
        geom = "text",
        x = 5,
        y = ymax + 0.06 * ymax,
        label = "Insulin",
        size = 4,
        hjust = 0,
        family = plot_font_family
      )
  }
  
  # If category = 1 or 3 (experiments involving HFS) do the following:
  if (category == 1 |
      category == 3) {
    # Get limits of x- and y-axes
    ymax <- layer_scales(treatment_plot)$y$get_limits()[2]
    xmax <- layer_scales(treatment_plot)$x$get_limits()[2]
    ymax2 <- layer_scales(treatment_plot)$y$get_limits()[2]
    
    # Add an arrow showing when HFS was applied (x = 5 min)
    treatment_plot <-
      treatment_plot +
      annotate(
        geom = "segment",
        x = 5,
        y = ymax + 0.22 * ymax,
        xend = 5,
        yend = ymax + 0.10 * ymax,
        arrow = arrow(type = "closed", length = unit(0.02, "npc"))
      )
    
    # Add "HFS" text label
    treatment_plot <-
      treatment_plot +
      annotate(
        geom = "text",
        x = 5,
        y = ymax + 0.27 * ymax,
        label = "HFS",
        size = 3.5,
        hjust = 0.5,
        family = plot_font_family
      )
  }
  
  treatment_plot <- treatment_plot +
    geom_text(
      data = t_test_eEPSCs %>% filter(Treatment == treatment),
      aes(x = asterisk_time,
          y = y_axis_limit - 50,
          label = significance_stars),
      inherit.aes = F,
      size = 8,
      family = plot_font_family
    ) +
    annotation_custom(representative_trace, xmin = 1, xmax = 8, ymin = 0, ymax = 40)
  
  
  # Save plots to a subfolder called 'Output-summary-plots' within Figures/
  if (save_choice == "yes") {
    ggsave(
      treatment_plot,
      path = here("Figures", "Output-summary-plots"),
      file = paste0("Summary-plot-", treatment, "-category-", category, ".png"),
      width = 10,
      height = 7,
      units = "in",
      dpi = 300,
      scaling = 1.25
    )
  }
  
  treatment_plot
}




make_presentation_plots <- function(category,
                                    treatment,
                                    data,
                                    color_choice,
                                    ...) {
  df <-
    data %>% filter(Category == category) %>% filter(Treatment == treatment)
  
  presentation_plot <- df %>%
    ggplot(aes(
      x = Time,
      y = Mean_all,
      ymin = Mean_all - se,
      ymax = Mean_all + se
    )) +
    geom_rect(aes(xmin = 5,
                  xmax = 10,
                  ymin = -5,
                  ymax = y_axis_limit),
              fill = rectangle_shading_colour) +
    geom_rect(aes(xmin = 15,
                  xmax = 20,
                  ymin = -5,
                  ymax = y_axis_limit),
              fill = rectangle_shading_colour) +
    geom_pointrange(
      aes(color = Sex, shape = Sex),
      size = 0.9,
      alpha = 1,
      position = position_dodge(width = 0.3)
    ) +
    scale_shape_manual(values = c(17, 16),
                       labels = c((paste0(
                         "Females, n = ", df$n[df$Sex == "Female"][1]
                       )),
                       (paste0(
                         "Males, n = ", df$n[df$Sex == "Male"][1]
                       )))) +
    scale_color_manual(
      values = c(my_colours_very_pale[color_choice], my_colours_very_dark[color_choice]),
      labels = c((paste0(
        "Females, n = ", df$n[df$Sex == "Female"][1]
      )),
      (paste0(
        "Males, n = ", df$n[df$Sex == "Male"][1]
      )))
    ) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    labs(
      x = "Time (min)",
      y = "eEPSC\nAmplitude\n(% baseline)",
      color = NULL,
      shape = NULL
    ) +
    theme(axis.title.y = element_text(angle = 90))
  
  # Get limits of x-axis
  # The Y-axis is fixed using the 'y_axis_limit' variable in the set-up chunk,
  # so I specified 'ymax' relative to 'y_axis_limit'
  ymax <- y_axis_limit - 25
  xmax <-
    layer_scales(presentation_plot)$x$get_limits()[2]
  
  
  # If category = 2 (an experiment where insulin was added)
  # add a line showing the times when insulin was applied:
  if (category == 2) {
    presentation_plot <-
      presentation_plot +
      annotate(
        geom = "segment",
        x = 5,
        xend = xmax,
        y = ymax,
        yend = ymax,
        colour = line_col,
        linewidth = 0.5
      ) +
      # Add "Insulin" label
      annotate(
        geom = "text",
        x = 5,
        y = ymax + 0.06 * ymax,
        label = "Insulin",
        size = 4,
        hjust = 0,
        family = plot_font_family
      )
  }
  
  # If category = 1 or 3 (experiments involving HFS) do the following:
  if (category == 1 |
      category == 3) {
    # Get limits of x- and y-axes
    ymax <- layer_scales(presentation_plot)$y$get_limits()[2]
    xmax <- layer_scales(presentation_plot)$x$get_limits()[2]
    ymax2 <- layer_scales(presentation_plot)$y$get_limits()[2]
    
    # Add an arrow showing when HFS was applied (x = 5 min)
    presentation_plot <-
      presentation_plot +
      annotate(
        geom = "segment",
        x = 5,
        y = ymax + 0.22 * ymax,
        xend = 5,
        yend = ymax + 0.10 * ymax,
        arrow = arrow(type = "closed", length = unit(0.02, "npc"))
      )
    
    # Add "HFS" text label
    presentation_plot <-
      presentation_plot +
      annotate(
        geom = "text",
        x = 5,
        y = ymax + 0.27 * ymax,
        label = "HFS",
        size = 3.5,
        hjust = 0.5,
        family = plot_font_family
      )
  }
  
  
  presentation_plot <- presentation_plot +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.17, 0.13),
      legend.background = element_rect(fill = NA),
      plot.title = element_markdown(
        hjust = 0.5,
        lineheight = 1.5,
        family = plot_font_family
      ),
      plot.title.position = "plot",
      axis.title.x = element_text(margin = margin(b = 2)),
      axis.title.y = element_text(
        angle = 0,
        hjust = 0.5,
        margin = margin(r = 14)
      )
    ) +
    coord_fixed(ratio = 0.1, ylim = c(-5, y_axis_limit), xlim = c(-0.2, 25.5), expand=F) +
    geom_text(
      data = t_test_eEPSCs %>% filter(Treatment == treatment),
      aes(x = asterisk_time,
          y = y_axis_limit - 50,
          label = significance_stars),
      inherit.aes = F,
      size = 8,
      family = plot_font_family
    )
  
  presentation_plot
}



make_presentation_plots_one_sex <-
  function(sex,
           point_shape,
           point_color,
           category,
           treatment,
           data,
           color_choice,
           ...) {
    df <-
      data %>% filter(Category == category) %>% filter(Treatment == treatment) %>% filter(Sex == sex)
    
    presentation_plot <- df %>%
      ggplot(aes(
        x = Time,
        y = Mean_all,
        ymin = Mean_all - se,
        ymax = Mean_all + se,
        color = Sex,
        shape = Sex
      )) +
      geom_pointrange(size = 0.9,
                      alpha = 1,
                      #color = point_color,
                      #shape = point_shape,
                      position = position_dodge(width = 0.3)) +
      scale_shape_manual(values = c(point_shape),
                         labels = c(paste0(sex, "s, n = ", df$n[df$Sex == sex][1]))) +
      scale_color_manual(values = c(point_color),
                         labels = c(paste0(sex, "s, n = ", df$n[df$Sex == sex][1]))) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      labs(
        x = "Time (min)",
        y = "eEPSC\nAmplitude\n(% baseline)",
        color = NULL,
        shape = NULL
      ) +
      theme(axis.title.y = element_text(angle = 0))
    
    # Get limits of x-axis
    # The Y-axis is fixed using the 'y_axis_limit' variable in the set-up chunk,
    # so I specified 'ymax' relative to 'y_axis_limit'
    ymax <- y_axis_limit - 25
    xmax <-
      layer_scales(presentation_plot)$x$get_limits()[2]
    
    
    # If category = 2 (an experiment where insulin was added)
    # add a line showing the times when insulin was applied:
    if (category == 2) {
      presentation_plot <-
        presentation_plot +
        annotate(
          geom = "segment",
          x = 5,
          xend = xmax,
          y = ymax,
          yend = ymax,
          colour = line_col,
          linewidth = 0.5
        ) +
        # Add "Insulin" label
        annotate(
          geom = "text",
          x = 5,
          y = ymax + 0.06 * ymax,
          label = "Insulin",
          size = 4,
          hjust = 0,
          family = plot_font_family
        )
    }
    
    # If category = 1 or 3 (experiments involving HFS) do the following:
    if (category == 1 |
        category == 3) {
      # Get limits of x- and y-axes
      ymax <- layer_scales(presentation_plot)$y$get_limits()[2]
      xmax <- layer_scales(presentation_plot)$x$get_limits()[2]
      ymax2 <- layer_scales(presentation_plot)$y$get_limits()[2]
      
      # Add an arrow showing when HFS was applied (x = 5 min)
      presentation_plot <-
        presentation_plot +
        annotate(
          geom = "segment",
          x = 5,
          y = ymax + 0.22 * ymax,
          xend = 5,
          yend = ymax + 0.10 * ymax,
          arrow = arrow(type = "closed", length = unit(0.02, "npc"))
        )
      
      # Add "HFS" text label
      presentation_plot <-
        presentation_plot +
        annotate(
          geom = "text",
          x = 5,
          y = ymax + 0.27 * ymax,
          label = "HFS",
          size = 3.5,
          hjust = 0.5,
          family = plot_font_family
        )
    }
    
    
    presentation_plot <- presentation_plot +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.17, 0.13),
        plot.title = element_markdown(
          hjust = 0.5,
          lineheight = 1.5,
          family = plot_font_family
        ),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(b = 2)),
        axis.title.y = element_text(
          angle = 0,
          hjust = 0.5,
          margin = margin(r = 14)
        )
      ) +
      coord_fixed(ratio = 0.1, ylim = c(0, y_axis_limit))
    
    presentation_plot
  }





make_facet_plot <-
  function(data,
           treatment,
           sex,
           category,
           y_var = P1,
           color_choice) {
    plot_data <- data %>%
      filter(Treatment == treatment &
               Sex == sex & Category == category)
    
    plot_data %>%
      ggplot(aes(x = Time, {
        {
          y_var
        }
      })) +
      geom_point(color = my_colours[color_choice]) +
      modified_facet_theme +
      #geom_hline(aes(yintercept = baseline_mean), linetype = "dashed") +
      facet_wrap(. ~ Letter, ncol = 3, scales = "free_y") +
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf,
        color = "gray"
      ) +
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        color = "gray"
      ) +
      labs(
        title = paste0(
          unique(plot_data$Sex),
          "s, Treatment: ",
          unique(plot_data$Treatment)
        ),
        subtitle = paste0("Category: ", unique(plot_data$Category))
      )
  }


make_pruned_facet_plot <-
  function(data,
           treatment,
           sex,
           category,
           color_choice) {
    plot_data <- data %>%
      filter(Treatment == treatment &
               Sex == sex & Category == category)
    
    plot_data %>%
      ggplot(aes(x = Time,
                 y = mean_P1)) +
      geom_point(colour = my_colours[color_choice],
                 size = 2.5) +
      modified_facet_theme +
      geom_hline(aes(yintercept = baseline_mean), linetype = "dashed") +
      facet_wrap(. ~ Letter, ncol = 3, scales = "free_y") +
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf,
        color = "gray"
      ) +
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        color = "gray"
      ) +
      labs(
        title = paste0(
          unique(plot_data$Sex),
          "s, Treatment: ",
          unique(plot_data$Treatment)
        ),
        subtitle = paste0("Category: ", unique(plot_data$Category))
      )
  }

make_AP_plot <-
  function(data, y, y_axis_title) {
    data %>%
      ggplot(aes(
        x = State,
        y = {
          {
            y
          }
        },
        color = State,
        shape = State
      )) +
      geom_line(aes(group = Letter),
                linewidth = connecting_line_width,
                color = connecting_line_colour_aps) +
      geom_point(alpha = 0.8,
                 size = geom_sina_size,
                 position = position_jitter(0.02)) +
      #geom_sina(bw = 7, alpha = 0.8, maxwidth = 0.25, size = geom_sina_size) +
      scale_color_manual(values = c(baseline_group_colour, insulin_group_colour)) +
      stat_summary(
        fun.data = mean_se,
        geom = "pointrange",
        color = mean_point_colour,
        size = mean_point_size
      ) +
      theme(
        legend.position = "none",
        axis.line = element_line(linewidth = 0.4),
        axis.title = element_text(face = "plain")
      ) +
      labs(x = NULL,
           y = y_axis_title) +
      geom_signif(
        comparisons = list(c("Baseline", "Insulin")),
        test = "wilcox.test",
        test.args = list(paired = T),
        map_signif_level = list_of_significance_stars,
        vjust = -0.3,
        family = significance_stars_font,
        textsize = geom_signif_text_size,
        size = 0.4
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.2, .2)))
  }


save_AP_plot <- function(ap_plot, plot_name) {
  ggsave(
    plot = ap_plot,
    path = here("Figures", "Output-summary-plots"),
    file = paste0(plot_name, "-summary-plot.png"),
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )
}


make_qq_plot <- function(data, parameter_title, parameter) {
  ggplot(data, aes(sample = {
    {
      parameter
    }
  })) +
    stat_qq() +
    stat_qq_line() +
    labs(title = glue("QQ-Plot for {parameter_title}"))
}


# coord_cartesian() is required to lock' the coordinates of all AP traces to the same scales
make_AP_trace_plot <-
  function(file_ID, sweep1, sweep2, trace_color) {
    abf2_load(paste0("../Data/ABF-Files/", file_ID, ".abf")) %>%
      MeltAbf() %>%
      rename("Voltage" = chan1,
             "Current" = chan2) %>%
      filter(Episode %in% c(sweep1, sweep2)) %>%
      ggplot(aes(x = Time, y = Voltage, group = Episode)) +
      geom_line(color = trace_color, linewidth = AP_trace_size) +
      labs(x = NULL,
           y = NULL) +
      coord_cartesian(
        xlim  = c(ap_traces_x_min, ap_traces_x_max),
        ylim = c(ap_traces_y_min, ap_traces_y_max)
      ) +
      theme_void()
  }



make_PPR_plot <- function(ppr_min, ppr_max) {
  ppr_plot <- raw_df %>%
    filter(Category == 2) %>%
    filter(Treatment %in% c("None", "HNMPA", "Fasting", "PPP")) %>%
    filter(PPR < ppr_max & PPR > ppr_min) %>%
    filter(Interval == "t0to5" | Interval == "t20to25") %>%
    mutate(
      Treatment = factor(Treatment, levels = c("None", "Fasting", "HNMPA", "PPP")),
      State = case_when(
        Interval == "t0to5" ~ "Baseline",
        Interval == "t20to25" ~ "Insulin",
        T ~ Interval
      )
    ) %>%
    group_by(Treatment, Letter, State) %>%
    summarise(mean_PPR_cell = mean(PPR)) %>%
    ungroup() %>%
    ggplot(aes(x = State, y = mean_PPR_cell, color = Treatment)) +
    geom_sina(size = 2, position = position_dodge(width = 1)) +
    stat_summary(
      aes(x = State, y = mean_PPR_cell, group = Treatment),
      inherit.aes = F,
      fun.data = "mean_se",
      geom = "pointrange",
      position = position_dodge(width = 1),
      color = mean_point_colour,
      size = mean_point_size + 0.2
    ) +
    scale_color_manual(values = c(my_colours[1], my_colours[3], my_colours[5], my_colours[2])) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    theme(legend.position = "right")
  
  if (save_choice == "yes") {
    ggsave(
      plot = ppr_plot,
      path = here("Figures", "Output-summary-plots"),
      file = paste0("PPR_plot_", ppr_min, "_to_", ppr_max, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
    
  }
  
  ppr_plot
}

# Manually add significance bracket to PPR Plot
# since geom_signif does not work with nested data

add_bracket <- function(treatment, y_position) {
  geom_bracket(
    xmin = paste0("Baseline!", treatment),
    xmax = paste0("Insulin!", treatment),
    y.position = y_position,
    label = t_test_PPR %>%
      filter(Treatment == treatment) %>%
      pull(significance_stars),
    inherit.aes = F,
    family = plot_font_family,
    vjust = -0.5,
    tip.length = 0.03,
    bracket.nudge.y = 0.02,
    color = "black"
  )
}