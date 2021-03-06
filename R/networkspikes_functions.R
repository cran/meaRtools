has_network_spikes <- function(nspikes) {
  return(!is.null(.as_data_frame_network_spikes(nspikes)))
}

summarize_network_spikes <- function(e, nspikes, ns_e, sur) {
  wells <- nspikes$wells
  names(wells) <- wells # keep the names valid.
  for (i in 1:length(wells)) {
    well <- wells[[i]]
    data <- nspikes$ns_all[[i]]$measures
    if (!is.null(data)) {
      indexes <- .names_to_indexes(names(e$spikes), well, allow_na = TRUE)
      electrodes <- names(e$spikes)[indexes]

      en_map <- matrix(0, length(electrodes), dim(data)[1])
      rownames(en_map) <- electrodes
      colnames(en_map) <- as.character(data[, 1])
      for (ns in 1:dim(data)[1]) {
        current_ns <- data[ns, 1]
        en_map[, ns] <- unlist(lapply(e$spikes[indexes],
        function(x) {
          length(x[x > current_ns & x <=
                     current_ns + (nspikes$ns_all)[[i]]$ns_t])
        }))
      }
      en_map[en_map < ns_e] <- 0
      filtered_indexes <-
        which(colSums(en_map >= ns_e) >= (nspikes$ns_all)[[i]]$ns_n)

      en_map <- en_map[, filtered_indexes]
      if (length(filtered_indexes) == 1) {
        # deal with R vector matrix problem
        dim(en_map) <- c(length(electrodes), length(filtered_indexes))
        colnames(en_map) <- names(filtered_indexes)
        rownames(en_map) <- electrodes
      }

      if (dim(en_map)[2] > 0) {
        p <- data[filtered_indexes, 1:2]
        dim(p) <- c(length(filtered_indexes), 2)
        colnames(p) <- c("time", "index")
        if (is.na(p[1, 1])) {
          p <- NULL
        }
      } else {
        p <- NULL
      }
    } else {
      p <- NULL
    }

    ns <- list(counts = nspikes$ns_all[[i]]$counts,
               ns_n = (nspikes$ns_all)[[i]]$ns_n,
               ns_t = (nspikes$ns_all)[[i]]$ns_t)
    class(ns) <- "ns"
    m <- .mean_ns(ns, p, plot = FALSE, nrow = 4,
                  ncol = 4, ask = FALSE, sur = sur)
    if (is.null(m)) {
      ns$brief <- c(n = 0, peak_m = NA, peak_sd = NA, durn_m = NA, durn_sd = NA,
        percent_of_spikes_in_ns = NA,
        mean_spikes_in_ns = NA,
        mean_insis = NA)
      ns$en_map <- NULL
      ns$brief_electrode <- NULL
    } else {
      ns$mean <- m$ns_mean
      ns$measures <- m$measures
      peak_val <- ns$measures[, "peak_val"]
      durn <- ns$measures[, "durn"]
      insis <- diff(ns$measures[, "time"])
      if (length(insis) > 0) {
        mean_insis <- mean(insis)
      } else {
        mean_insis <- NA
      }
      ns$brief <- c(n = nrow(ns$measures), peak_m = mean(peak_val),
        peak_sd = sd(peak_val), durn_m = mean(durn, na.rm = TRUE),
        durn_sd = sd(durn, na.rm = TRUE),
        percent_of_spikes_in_ns = 100 * sum(en_map) / sum(e$nspikes[indexes]),
        mean_spikes_in_ns = sum(en_map) / nrow(ns$measures),
        mean_insis = mean_insis
      )
      if (dim(en_map)[2] != dim(ns$measures)[1]) {
        en_map <- en_map[, as.character(ns$measures[, 1])]
      }
      ns$en_map <- en_map

      # now briefs at electrode level
      features <- c("spikes", "ns", "spikes_in_ns", "percent_of_spikes_in_ns",
        "mean_spikes_per_ns", "sd_spikes_per_ns", "mean_insis")
      en_brief <- matrix(0, dim(en_map)[1], length(features))
      rownames(en_brief) <- rownames(en_map)
      colnames(en_brief) <- features
      en_brief[, "spikes"] <- e$nspikes[indexes]
      en_brief[, "ns"] <- rowSums(en_map > 0)
      en_brief[, "spikes_in_ns"] <- rowSums(en_map)
      en_brief[, "percent_of_spikes_in_ns"] <-
        100 * en_brief[, "spikes_in_ns"] / en_brief[, "spikes"]

      en_brief[, "mean_spikes_per_ns"] <-
        en_brief[, "spikes_in_ns"] / en_brief[, "ns"]
      en_brief[, "sd_spikes_per_ns"] <-
        unlist(lapply(rownames(en_brief), function(e) {
        temp <- sd(en_map[e, which(en_map[e, ] > 0)])
        temp[is.na(temp)] <- NaN
        temp
      }))

      en_brief[, "mean_insis"] <- unlist(lapply(rownames(en_brief),
        function(e) {
        insis <- diff(as.numeric(names(which(en_map[e, ] > 0))))
        m <- mean(insis)
        m
      }))

      en_brief[is.nan(en_brief)] <- NA

      ns$en_brief <- en_brief
    }

    nspikes$ns_all[[i]] <- ns
  }
  nspikes
}

calculate_network_spikes <- function(e, sur=100, ns_n, ns_t) {
  # get well information
  plateinfo <- get_plateinfo(e$layout$array)
  wells <- plateinfo$wells
  names(wells) <- wells # keep the names valid.
  wells_layout <- plateinfo$layout
  ## lets just use lapply
  ns_all <- lapply(wells, function(well) {
    .compute_ns(e, ns_t = ns_t, ns_n = ns_n, sur = sur, whichcells = well)})
  return(list(wells = wells, ns_all = ns_all, wells_layout = wells_layout))
}

.as_data_frame_network_spikes <- function(nspikes) {
  # useful function
  mean_ns_to_xy <- function(ns, well) {
    ## Convert the mean network spike into a dataframe suitable for lattice
    ## graphics.
    if (!is.null(ns$mean)) {
      m <- ns$mean
      t <- as.vector(time(m))
      d <- data.frame(t = t, y = as.vector(m), well = well)
      d
    }
  }

  # get data in right format for lattice
  xys <- mapply(SIMPLIFY = FALSE, function(ns, well) {
    mean_ns_to_xy(ns, well)
  }, nspikes$ns_all, names(nspikes$ns_all))

  d <- do.call("rbind", xys) # merge all the data frames.
  d
}
xyplot_network_spikes <- function(nspikes) {
  p1 <- NULL
  ## Produce the title for each well; note difference between '0' for zero
  ## network spikes found in a well vs 'NA' (no spikes on that well).
  strip_names <- sapply(nspikes$wells, function(well) {
    n <- nspikes$ns_all[[well]]
    n_ns <- n$brief[[1]]
    paste(well, n_ns)
  })
  df <- .as_data_frame_network_spikes(nspikes)
  if (!is.null(df)) {
    p1 <- xyplot(y ~ 10 * t | factor(well,
        levels = nspikes$wells), data = df,
        strip = strip.custom(factor.levels = strip_names),
        main = "Mean NS", xlab = "Time (ms)",
        ylab = "# electrodes", type = "l", drop.unused.levels = FALSE,
        layout = nspikes$wells_layout)
    print(p1)
    p1
  }
}
.active_wells_network_spikes <- function(nspikes) {
  active_wells <- nspikes
  active_wells$ns_all <-
    nspikes$ns_all[sort(nspikes$wells[sapply(nspikes$wells,
      function(well) {
          !is.na(nspikes$ns_all[[well]]$brief[[1]]) &
               nspikes$ns_all[[well]]$brief[[1]] > 0
      })
    ])]
  active_wells
}

plot_active_wells_network_spikes <- function(nspikes) {
  active_wells <- .active_wells_network_spikes(nspikes)$ns_all
  if (length(active_wells) > 0) {
    for (j in 1:length(active_wells)) {
      plot_network_spikes(active_wells[[j]],
          main = names(active_wells)[j], ylab = "Count", xlab = "Time (s)")
      y <- as.vector(active_wells[[j]]$mean)
      plot(ts(y, start = c(- (length(y) - 1) / 2, 1)),
        xlab = "Time (ms)", ylab = "Count", main = paste("Mean NS for",
        names(active_wells)[j], sep = " "))
    }
  }
}

write_network_spikes_to_csv <- function(s, nspikes, outputdir) {
  csvwell <- paste(outputdir, "/",
                   get_project_plate_name(s$file), "_ns.csv", sep = "")
  div <- .get_div(s)
  active_wells <- .active_wells_network_spikes(nspikes)$ns_all
  if (length(active_wells) > 0) {
    # sahar 10292014 - add genotype column and change newcol from 2 to 3
    newcol <- 3
    # 2 to peak_min and peak_max
    p <- length(active_wells[[1]]$brief) +
      length(active_wells[[1]]$mean) + newcol
    nsdata <- matrix(0, length(s$well), p)
    temp <- c()
    # Diana Hall 10-31-2014 change
    length_temp_mean <- length(active_wells[[1]]$mean)
    for (j in 1:length(s$well)) {
      cur_well <- s$well[j]
      if (is.element(cur_well, names(active_wells))){
        temp <- active_wells[[cur_well]]
        nsdata[j, 1:length(temp$brief)] <- temp$brief
        nsdata[j, length(temp$brief) + 1] <- min(temp$measures[, "peak_val"])
        nsdata[j, length(temp$brief) + 2] <- max(temp$measures[, "peak_val"])
        nsdata[j, length(temp$brief) + 3] <- s$treatment[cur_well]
        nsdata[j, (length(temp$brief) + newcol + 1):p] <- as.double(temp$mean)

      } else {
        temp$brief <- c(0, rep(NA, 4), 0, NA, NA)
        nsdata[j, 1:length(temp$brief)] <- temp$brief
        nsdata[j, length(temp$brief) + 1] <- NA
        nsdata[j, length(temp$brief) + 2] <- NA
        nsdata[j, length(temp$brief) + 3] <- s$treatment[cur_well]
        nsdata[j, (length(temp$brief) + newcol + 1):p] <-
          rep(0, length_temp_mean)
      }
    }

    nsdata <- data.frame(nsdata)
    names(nsdata)[1:length(temp$brief)] <- names(active_wells[[1]]$brief)
    names(nsdata)[(length(temp$brief) + 1):(length(temp$brief) + newcol)] <-
      c("peak_min", "peak_max", "treatment")

    for (j in 1:(p - length(temp$brief) - newcol)) {
      names(nsdata)[j + newcol + length(temp$brief)] <- paste("t", j, sep = "")
    }
    nsdata <- cbind(s$well, nsdata)
    names(nsdata)[1] <- "well"

    basename <- get_file_basename(s$file)
    csvfile <- paste(outputdir, "/", basename, "_ns.csv", sep = "")

    write.table(paste("file= ",
        strsplit(basename(s$file), ".RData")[[1]][1], sep = ""),
      csvfile, sep = ",", append = FALSE,
      row.names = FALSE, col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
    # recording time
    write.table(paste("recording time (s): [", paste(s$rec_time[1],
      round(s$rec_time[2]), sep = " ,"),
      "]", sep = ""), csvfile, sep = ",",
      append = TRUE, row.names = FALSE, col.names = FALSE)


    write.table(" ", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)

    write.table("Network Spike analysis at well level",
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    suppressWarnings(write.table(nsdata,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
    suppressWarnings(write.table(cbind(div, nsdata),
      csvwell, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE))


    write.table(" ", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
    write.table("Network Spike analysis at electrode level",
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)


    en_df <- do.call("rbind", lapply(nspikes$ns_all, function(well) {
      temp <- well$en_brief
      temp
    }))
    en_df <- en_df[order(rownames(en_df)), ]
    en_df <- cbind(rownames(en_df), en_df)
    colnames(en_df)[1] <- "electrode"
    suppressWarnings(write.table(en_df,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))

  }
}
