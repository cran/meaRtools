plot_plate_summary_for_bursts <- function(s, outputdir, parameters) {
  for (i in (1:length(s))) {
    basename <- get_file_basename(s[[i]]$file)
    burst_plot_path <- paste(outputdir, "/", basename, "_burst_plot.pdf",
                            sep = "")
    pdf(file = burst_plot_path)
    # layout
    p <- .plot_mealayout(s[[i]]$layout, use_names = T, cex = 0.48)
    title(main = paste(paste("Electrode Layout"),
      paste("file= ", strsplit(basename(s[[i]]$file), ".RData")[[1]][1],
            sep = ""), sep = "\n"))

    # Add distribution plots

    # Perform IBI distribution analysis
    if (parameters$burst_distribution_ibi$perform) {

      feature <- "ibi"; print("Running IBI distribution analysis.")
      params <- parameters$burst_distribution_ibi
      p <- calc_burst_distributions(s[[i]], min_vals = params$min_cases,
                xlimit = params$x_axis_lim, bins_in_sec = params$bins_in_sec,
                feature = feature, filter_values_by_min = params$filter_by_min,
                min_values = params$min_values, per_well = params$per_well,
                outputdir = outputdir, min_electrodes =
                  parameters$well_min_rate, parameters$time_stamp)
    }

    # Perform ISI distribution analysis
    if (parameters$burst_distribution_isi$perform) {

      feature <- "isi"; print("Running ISI distribution analysis.")
      params <- parameters$burst_distribution_isi
      p <- calc_burst_distributions(s[[i]], min_vals = params$min_cases,
              xlimit = params$x_axis_lim, bins_in_sec = params$bins_in_sec,
              feature = feature, filter_values_by_min = params$filter_by_min,
              min_values = params$min_values, per_well = params$per_well,
              outputdir = outputdir, min_electrodes = parameters$well_min_rate,
              parameters$time_stamp)
    }

    # Perform nSpikes distribution analysis
    if (parameters$burst_distribution_nspikes$perform) {

      feature <- "nspikes_in_burst"
      print("Running nSpikes in bursts distribution analysis.")
      params <- parameters$burst_distribution_nspikes
      p <- calc_burst_distributions(s[[i]], min_vals = params$min_cases,
              xlimit = params$x_axis_lim, bins_in_sec = params$bins_in_sec,
              feature = feature, filter_values_by_min = params$filter_by_min,
              min_values = params$min_values, per_well = params$per_well,
              outputdir = outputdir, min_electrodes = parameters$well_min_rate,
              parameters$time_stamp)
    }

    # Perform duration of bursts distribution analysis
    if (parameters$burst_distribution_durn$perform) {

      feature <- "duration"
      print("Running duration of bursts distribution analysis.")
      params <- parameters$burst_distribution_durn
      p <- calc_burst_distributions(s[[i]], min_vals = params$min_cases,
              xlimit = params$x_axis_lim, bins_in_sec = params$bins_in_sec,
              feature = feature, filter_values_by_min = params$filter_by_min,
              min_values = params$min_values, per_well = params$per_well,
              outputdir = outputdir, min_electrodes = parameters$well_min_rate,
              parameters$time_stamp)
    }

    # Perform duration of bursts distribution analysis
    if (parameters$burst_distribution_spike_freq$perform) {

      feature <- "spikes_density_in_burst"
      print("Running spike density in bursts distribution analysis.")
      params <- parameters$burst_distribution_spike_freq
      p <- calc_burst_distributions(s[[i]], min_vals = params$min_cases,
              xlimit = params$x_axis_lim, bins_in_sec = params$bins_in_sec,
              feature = feature, filter_values_by_min = params$filter_by_min,
              min_values = params$min_values, per_well = params$per_well,
              outputdir = outputdir, min_electrodes = parameters$well_min_rate,
              parameters$time_stamp)
    }

    # MFR
    p <- .plot_meanfiringrate(s[[i]], main = "Mean Firing Rate by Plate (Hz)")

    p <- .channel_plot_by_well(s[[i]], resp = "meanfiringrate",
                               resp_label = "Mean Firing Rate (Hz)")
    # Mean Duration
    p <- .channel_plot_by_well(s[[i]], resp = "bs$mean_dur",
                               resp_label = "Mean Duration of Burst (s)")
    # plot of Number of bursts by channel and well
    p <- .channel_plot_by_well(s[[i]], resp = "bs$nbursts",
                               resp_label = "Number of Bursts")
    # mean Inter Burst Interval
    p <- .channel_plot_by_well(s[[i]], resp = "bs$mean_ibis",
                               resp_label = "Mean IBIs (ms)")
    # mean ISI within bursts
    p <- .channel_plot_by_well(s[[i]], resp = "bs$mean_isis",
                               resp_label = "Mean ISI w/i Bursts (s)")
    # mean burst per minute
    p <- .channel_plot_by_well(s[[i]], resp = "bs$bursts_per_min",
                               resp_label = "Mean Burst per Minute")
    # mean spikes in a burst
    p <- .channel_plot_by_well(s[[i]], resp = "bs$mean_spikes",
                               resp_label = "Mean # Spikes/Burst")
    # % spikes in a burst
    p <- .channel_plot_by_well(s[[i]], resp = "bs$per_spikes_in_burst",
                               resp_label = "% Spikes/Burst")
    dev.off()
  }
}

write_plate_summary_for_bursts <- function(s, outputdir) {
  master_sum <- .get_mean_burst_info_per_well(s)
  csvwell <- paste(outputdir, "/", get_project_plate_name(s[[1]]$file),
                   "_well_bursts.csv", sep = "")

  for (i in 1:length(s)) {
    div <- .get_div(s[[i]])
    basename <- get_file_basename(s[[i]]$file)
    csvfile <- paste(outputdir, "/", basename, "_bursts.csv", sep = "")

    ########## data frame summarized over well
    # get number of object in master_sum[[1]] list
    tempdf <- c(); tempcolnames <- c()
    for (j in 2:length(master_sum[[i]])) {
      tempc <- unlist(master_sum[[i]][j])
      tempdf <- cbind(tempdf, tempc)
      tempcolnames <- c(tempcolnames, names(master_sum[[i]][j]))
    } # end of loop through master_sum list objects

    # need to switch around columns so first columns come first
    if (dim(tempdf)[2] > 20) {
      if (dim(tempdf)[1] == 1) {
        df <- cbind(t(tempdf[, 21:25]), t(tempdf[, 1:20]))
      } else {
        df <- cbind(tempdf[, 21:25], tempdf[, 1:20])
      }
      colnames <- c(tempcolnames[21:25], tempcolnames[1:20])
      colnames(df) <- colnames
    }

    ################## channel by channel burst summary

    # meta data and misc
    # get vector of which wells are active
    wellindex <- which(is.element(names(s[[i]]$treatment),
                                  unique(s[[i]]$cw)))
    well <- c(); file <- c();

    file <- rep(strsplit(basename(s[[i]]$file), ".RData")[[1]][1],
                length(s[[i]]$cw))
    well <- s[[i]]$cw

    # channel data frame
    df2 <- cbind(file, well, as.data.frame(s[[i]]$bs[1:length(s[[i]]$bs)]))


    # write a title
    write.table("Burst Analysis Averaged Over Each Well",
      csvfile, sep = ",", append = FALSE, row.names = FALSE, col.names = FALSE)


    write.table(paste("file= ", strsplit(basename(s[[i]]$file),
                                         ".RData")[[1]][1], sep = ""),
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    # recording time
    write.table(paste("recording time (s): [", paste(s[[i]]$rec_time[1],
                round(s[[i]]$rec_time[2]), sep = " ,"),
      "]", sep = ""), csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)

    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)

    # summary write data, add back genotype (column 1)
    if (dim(df)[1] == 1) {
      suppressWarnings(write.table(t(df[, - c(2:3)]),
        csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = TRUE))
      suppressWarnings(write.table(cbind(div, t(df[, - c(2:3)])),
        csvwell, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE))
    } else {
      suppressWarnings(write.table(df[, - c(2:3)],
        csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = TRUE))
      suppressWarnings(write.table(cbind(div, df[, - c(2:3)]),
        csvwell, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE))
    }

    # new lines
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)

    # title
    write.table("Channel Burst Summary", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
    write.table(paste("file= ", strsplit(basename(s[[i]]$file),
                ".RData")[[1]][1], sep = ""),
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)

    # channel data
    suppressWarnings(write.table(df2,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
    # new lines
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE,
                col.names = FALSE)
  } # end of loop through writting tables
}
