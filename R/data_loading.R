# Data loading and track management

# ---- BigWig data cache ------------------------------------------------------
#
# A package-level cache for bigwig signal data fetched by
# ImportBigWigRegion(). Keyed by (file/URL, chrom, start, end) --  values are
# the numeric vector returned by bwimport::bw_import().
#
# Without caching, every plot click re-fetches every bigwig from upstream.
# For an IGV session with 48 tracks served over FTP that's tens of seconds
# of network IO per plot, including when the user only tweaked a colour or
# font size at the same region. With the cache, re-plotting the same region
# is essentially instant.
#
# Scope: package-level environment (one per R session). Survives plot clicks
# within a session; doesn't persist across process restart.
# Eviction: none --  uncapped LRU may be added later. Use clear_bigwig_cache()
# if memory becomes tight.
# Correctness: failed fetches (bad URL, missing file) are NOT cached so a
# transient outage doesn't poison the cache.

.bw_cache <- new.env(parent = emptyenv())

#' @keywords internal
.bw_cache_key <- function(bw_file, chrom, start, end) {
  paste(bw_file, chrom, start, end, sep = "")
}


#' Clear the BigWig data cache
#'
#' @description Empties the in-memory cache that stores BigWig signal data
#' fetched by \code{ImportBigWigRegion()}. Useful when memory is getting
#' tight, or to force a re-fetch (e.g., the upstream file has been updated).
#'
#' @return Invisibly returns the number of entries removed.
#'
#' @export
#'
#' @examples
#' clear_bigwig_cache()
clear_bigwig_cache <- function() {
  n <- length(ls(.bw_cache, all.names = TRUE))
  rm(list = ls(.bw_cache, all.names = TRUE), envir = .bw_cache)
  invisible(n)
}


#' Number of entries currently in the BigWig data cache
#'
#' @description Returns the number of \code{(file, chrom, start, end)}
#' tuples currently held in the cache that backs \code{ImportBigWigRegion()}.
#'
#' @return Integer; entry count.
#'
#' @export
#'
#' @examples
#' bigwig_cache_size()
bigwig_cache_size <- function() {
  length(ls(.bw_cache, all.names = TRUE))
}




#' Sort Unlisted Sample Names
#'
#' @description Internal function: 
#' Sort Unlisted Sample Names
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param unlisted_which_sample 
#' @param unlisted_sample_names 
#' @param incl_rep 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
SortUnlistedSampleNames = function(unlisted_which_sample, unlisted_sample_names, incl_rep=F){
  .subsamples = strsplit(unlisted_which_sample, split='.', fixed=T)[[1]]
  if (incl_rep){
    .sample.rep.split = strsplit(.subsamples[length(.subsamples)], split='.rep', fixed=T)[[1]]
    .subsamples[length(.subsamples)] = .sample.rep.split[1]
    .n.rep = .sample.rep.split[2]
  }
  unlisted.sample.name = unlisted_sample_names
  for (i in 1:length(.subsamples)){
    unlisted.sample.name = grep(.subsamples[i], unlisted.sample.name, fixed=TRUE, value=T)
  }
  if (length(unlisted.sample.name)>1){
    .sample.name.matrix = do.call('rbind', lapply(unlisted.sample.name, function(x) strsplit(x, split='.', fixed=T)[[1]]))
    unlisted.sample.name.index = 1:nrow(.sample.name.matrix)
    for (j in 1:ncol(.sample.name.matrix)){
      for (i in 1:length(.subsamples)){
        if (any(.sample.name.matrix[,j]==.subsamples[i])){
          unlisted.sample.name.index = intersect(unlisted.sample.name.index, which(.sample.name.matrix[,j]==.subsamples[i]))
        }
      }
    }
    unlisted.sample.name = unlisted.sample.name[unlisted.sample.name.index]
  }
  if (incl_rep){
    unlisted.sample.name = paste0(unlisted.sample.name, '.rep', .n.rep)
  }
  return(unlisted.sample.name)
}


#' Unpack Samples
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param seqtype 
#' @param samples 
#' @param which_samples 
#' @param which_reps 
#' @param bigwig_list 
#' @param bigwig_dirs 
#' @param verbosity 
#'
#' @return placeholder
#' 
#' @examples
#' NULL
#' 
UnpackSamples = function(seqtype, samples, which_samples, which_reps, bigwig_list, bigwig_dirs, verbosity){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .all.sample.names = samples[[seqtype]]
  .n.levels = ListDepth(.all.sample.names)
  if (.n.levels > 0){
    .unlisted.sample.names = ListFlatten(.all.sample.names)
    .unlisted.sample.names = paste(rep(names(.unlisted.sample.names), lengths(.unlisted.sample.names)), as.character(unlist(.unlisted.sample.names)), sep='.')
  }else{
    .unlisted.sample.names = .all.sample.names
  }
  .incl.rep = FALSE
  if (!is.null(which_samples)){
    .n.levels.which.samples = ListDepth(which_samples)
    if (.n.levels.which.samples > 0){
      .unlisted.which.samples = ListFlatten(which_samples)
      .unlisted.which.samples = paste(rep(names(.unlisted.which.samples), lengths(.unlisted.which.samples)), as.character(unlist(.unlisted.which.samples)), sep='.')
    }else{
      if (!is.numeric(which_samples)){
        .unlisted.which.samples = which_samples
      }else{
        .unlisted.which.samples = .unlisted.sample.names[which_samples]
      }
    }
    if (any(grepl('.rep\\d$', .unlisted.which.samples))){
      if (all(grepl('.rep\\d$', .unlisted.which.samples))){
        .unlisted.sample.names = sapply(.unlisted.which.samples, SortUnlistedSampleNames, .unlisted.sample.names, incl_rep=T)
        .incl.rep=T
        which_reps = NULL
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('some, but not all sample names are specified by replicate number (i.e. ends with ".rep" followed by integer)',
                                                                          '\n', paste(paste('\t', '.)', .unlisted.which.samples), collapse='\n'))
      }
    }else{
      .unlisted.sample.names = sapply(.unlisted.which.samples, SortUnlistedSampleNames, .unlisted.sample.names)
    }
  }
  if (length(.messages[['errors']])==0){
    .unlisted.which.reps = NULL
    if (!is.null(which_reps)){
      .n.levels.which.reps = ListDepth(which_reps)
      if (.n.levels.which.reps > 0){
        .unlisted.which.reps = ListFlatten(which_reps)
        .names.unlisted.which.reps = sapply(names(.unlisted.which.reps), SortUnlistedSampleNames, as.character(.unlisted.sample.names), incl_rep=F)
        names(.unlisted.which.reps) = as.character(.names.unlisted.which.reps)
      }else{
        if (is.null(names(which_reps))){
          .unlisted.which.reps = lapply(as.character(.unlisted.sample.names), function(x) which_reps);  names(.unlisted.which.reps) = as.character(.unlisted.sample.names)
        }else{
          .unlisted.which.reps = as.list(which_reps)
        }
      }
    }
    bw.files = list()
    for (.sample.name in as.character(.unlisted.sample.names)){
      .subsample.names = strsplit(.sample.name, split='.', fixed=T)[[1]]
      if (.incl.rep){
        .sample.rep.split = strsplit(.subsample.names[length(.subsample.names)], split='.rep', fixed=T)[[1]]
        .subsample.names[length(.subsample.names)] = .sample.rep.split[1]
        .n.rep = as.integer(.sample.rep.split[2])
      }
      .reps = bigwig_list[[seqtype]]
      .dirs = bigwig_dirs[[seqtype]]
      for (.subsample.name in .subsample.names){
        .reps = .reps[[.subsample.name]]
        .dirs = .dirs[[.subsample.name]]
      }
      if (!is.null(.unlisted.which.reps)){
        .reps = .reps[.unlisted.which.reps[[.sample.name]]]
        .dirs = .dirs[.unlisted.which.reps[[.sample.name]]]
      }
      if (length(.reps) > 0){
        if (.incl.rep){
          if (.n.rep <= length(.reps)){
            .reps = .reps[.n.rep]
            .dirs = .dirs[.n.rep]
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('the supplied replicate number does not exist',
                                                                              '\n', '\t', '.) ', .sample.name,
                                                                              '\n', paste(paste('\t', '\t', .reps), collapse='\n'))
          }
        }
        if (!is.null(which_reps)){
          .reps = .reps[which_reps[[.sample.name]]]
          .dirs = .dirs[which_reps[[.sample.name]]]
        }
        .n.reps = length(.reps)
        for (.n.rep in 1:.n.reps){
          .sample.rep.name = paste0(.sample.name, '.rep', .n.rep)
          .bw.file = paste0(.dirs[.n.rep], .reps[.n.rep])
          bw.files[[.sample.rep.name]] = .bw.file
        }
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('the supplied sample name does not exist',
                                                                          '\n', '\t', '.) ', .sample.name,
                                                                          '\n', paste(paste('\t', '\t', as.character(.unlisted.sample.names)), collapse='\n'))
      }
    }
  }
  PrintOutput(.messages, verbosity)
  if (length(.messages[['errors']])==0){
    return(bw.files)
  }else{
    return(NULL)
  }
}


#' Import BigWig region
#'
#' @param bw_file Character scalar: path to a local BigWig or a URL (http/https/ftp)
#' @param chrom   Character scalar: chromosome name (e.g., "chr1")
#' @param start   Integer(1): 1-based start (inclusive)
#' @param end     Integer(1): 1-based end (inclusive)
#' @return Numeric vector of length end - start + 1
#' @export
#' @examples
#' bw_URL <- "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_3pseq/siGFP_noPAP_in_batch1_plus.bw"
#' chrom <- "chr12"
#' chrom_start <- 6531808
#' chrom_end <- 6541078
#' vals <- bw_import(bw_URL, chrom, chrom_start, chrom_end)
#' max(vals) # [1] 9503.29
#'
bw_import_obs <- function(bw_file, chrom, start, end) {
  stopifnot(
    is.character(bw_file), length(bw_file) == 1L,
    is.character(chrom),   length(chrom)   == 1L
  )
  start <- as.integer(start)
  end   <- as.integer(end)
  if (!is.finite(start) || !is.finite(end) || start < 1L || end < start) {
    stop("Invalid coordinates: start must be >= 1 and end >= start.", call. = FALSE)
  }
  
  is_url <- grepl("^(https?|ftp)://", bw_file, ignore.case = TRUE)
  
  # On Windows we build libBigWig without libcurl (-DNOCURL),
  # so remote files must be downloaded to a temp file first.
  if (.Platform$OS.type == "windows" && is_url) {
    # FIX: use a temp directory without spaces in the path if possible,
    # since libBigWig's fopen may struggle with spaces on some Windows setups.
    td <- tempdir()
    # Try to use the 8.3 short path form to avoid spaces
    short_td <- tryCatch(
      utils::shortPathName(td),
      error = function(e) td
    )
    tf <- tempfile(tmpdir = short_td, fileext = ".bw")
    tryCatch(
      curl::curl_download(url = bw_file, destfile = tf, mode = "wb"),
      error = function(e) {
        stop(sprintf(
          "Failed to download BigWig file from URL:\n  %s\nError: %s\n\nTip: check your internet connection and try Sys.setenv(BWIMPORT_IPRESOLVE = \"4\") to force IPv4.",
          bw_file, conditionMessage(e)
        ), call. = FALSE)
      }
    )
    on.exit(try(unlink(tf), silent = TRUE), add = TRUE)
    bw_file <- tf
  } else if (.Platform$OS.type == "windows" && !is_url) {
    # FIX: for local files on Windows, normalise to forward slashes
    # and expand ~ so libBigWig's fopen sees a clean path.
    bw_file <- normalizePath(bw_file, winslash = "/", mustWork = TRUE)
  }
  
  bw_import_impl(bw_file, chrom, start, end)
}


#' Import BigWig Signal With Chromosome Name Fallback
#'
#' @description Internal function:
#' Import signal from a single BigWig file for a genomic region.
#' If the primary chromosome name fails (e.g. "chr1"), falls back
#' to the name without "chr" prefix (e.g. "1"). Returns zeros
#' for missing files or complete failures.
#'
#' @keywords internal
#'
#' @param bw_file path or URL to a BigWig file
#' @param chrom chromosome name
#' @param chrom_start start coordinate
#' @param chrom_end end coordinate
#' @param verbosity verbosity level
#'
#' @return numeric vector of signal values
#'
#' Raw bigwig fetch (internal): no caching, no fallback, no chrom-prefix retry.
#' Returns the numeric vector on success, NULL on failure. The split lets the
#' parallel batch fetcher distinguish "fetch failed" from "fetch returned zero"
#' so we don't cache transient failures.
#' @keywords internal
.fetch_bw_raw <- function(bw_file, chrom, chrom_start, chrom_end) {
  .data <- tryCatch(
    bwimport::bw_import(bw_file, chrom, chrom_start, chrom_end),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(.data)) {
    # Try alternate chromosome naming (strip "chr" prefix)
    .alt.chrom <- sub("^chr", "", chrom)
    .data <- tryCatch(
      bwimport::bw_import(bw_file, .alt.chrom, chrom_start, chrom_end),
      error   = function(e) NULL,
      warning = function(w) NULL
    )
  }
  .data
}


ImportBigWigRegion = function(bw_file, chrom, chrom_start, chrom_end, verbosity) {
  region_length <- chrom_end - chrom_start + 1
  if (!file.exists(bw_file) & !grepl("^(https?|ftp)://", bw_file)) {
    if (verbosity > 1) {
      cat('WARNING(s):', '\n')
      cat(' - non-existing file', '\n')
      cat(paste('\t', '.)', bw_file), '\n')
    }
    return(rep(0, region_length))
  }

  # Cache lookup. Hit -> return immediately (no network round-trip).
  cache_key <- .bw_cache_key(bw_file, chrom, chrom_start, chrom_end)
  if (exists(cache_key, envir = .bw_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .bw_cache, inherits = FALSE))
  }

  # Cache miss -> fetch from upstream.
  .data <- .fetch_bw_raw(bw_file, chrom, chrom_start, chrom_end)
  if (is.null(.data)) {
    # Failed fetch -> return all-zero fallback but DON'T cache it,
    # so a later retry can still succeed if the upstream recovers.
    return(rep(0, region_length))
  }

  # Cache successful fetch and return.
  assign(cache_key, .data, envir = .bw_cache)
  return(.data)
}


#' Pick number of parallel workers for batched bigwig fetch (internal).
#'
#' Respects user opt-out and physical core count. Cap at 8 to avoid hammering
#' remote bigwig servers / saturating the network stack. Cross-platform: this
#' worker count drives `future`'s `multisession` plan, which uses background R
#' processes (PSOCK) on all OSes including Windows.
#' @keywords internal
.pick_n_bw_workers <- function(n_tasks) {
  if (isTRUE(getOption("seqNdisplayR.disable_parallel_bw", FALSE))) return(1L)
  ncores <- tryCatch(parallel::detectCores(logical = FALSE),
                     error = function(e) NA_integer_)
  if (is.na(ncores) || ncores < 2L) return(1L)
  max_workers <- max(1L, min(as.integer(ncores) - 1L, 8L))
  as.integer(min(max_workers, n_tasks))
}


#' Ensure a future plan is set, lazily on first batched fetch (internal).
#'
#' If the user has already configured `future::plan(...)`, respect it. If the
#' current plan is `sequential` (future's default), set up a `multisession`
#' plan with workers picked by `.pick_n_bw_workers()`. Workers persist across
#' plot calls, so the worker-start cost is paid once per R session, not once
#' per plot.
#' @keywords internal
.ensure_future_plan <- function(n_tasks) {
  if (!requireNamespace("future", quietly = TRUE)) return(invisible(FALSE))
  cur <- future::plan()
  # If user already set a non-trivial plan, honour it
  if (!inherits(cur, "sequential")) return(invisible(TRUE))
  n <- .pick_n_bw_workers(n_tasks)
  if (n <= 1L) return(invisible(FALSE))
  future::plan(future::multisession, workers = n)
  invisible(TRUE)
}


#' Batched bigwig fetch over a region (internal).
#'
#' Cache-aware, parallel where supported. Returns a named list of numeric
#' vectors aligned with the input bw_files. Failed fetches return an
#' all-zero vector and are NOT cached, so a transient outage can be retried.
#'
#' @param bw_files Named list or named character vector of bigwig paths/URLs.
#' @param chrom,chrom_start,chrom_end Region.
#' @param verbosity Integer.
#' @keywords internal
.fetch_bw_batch <- function(bw_files, chrom, chrom_start, chrom_end, verbosity = 0L) {
  region_length <- chrom_end - chrom_start + 1L
  N <- length(bw_files)
  if (N == 0L) return(list())
  nms <- names(bw_files)
  results <- vector("list", N)
  names(results) <- nms

  # Step 1: cache lookup + early validity check.
  miss_idx <- integer(0)
  for (i in seq_len(N)) {
    bw <- as.character(bw_files[[i]])
    if (!file.exists(bw) & !grepl("^(https?|ftp)://", bw)) {
      if (verbosity > 1) {
        cat('WARNING(s):', '\n', ' - non-existing file', '\n',
            paste('\t', '.)', bw), '\n')
      }
      results[[i]] <- rep(0, region_length)
      next
    }
    key <- .bw_cache_key(bw, chrom, chrom_start, chrom_end)
    if (exists(key, envir = .bw_cache, inherits = FALSE)) {
      results[[i]] <- get(key, envir = .bw_cache, inherits = FALSE)
    } else {
      miss_idx <- c(miss_idx, i)
    }
  }

  if (length(miss_idx) == 0L) return(results)

  miss_files <- vapply(miss_idx, function(i) as.character(bw_files[[i]]),
                       character(1L))

  # Step 2: fetch the misses. Try `future`/`future.apply` first (cross-
  # platform persistent worker pool); fall back to sequential when the
  # packages aren't installed, the user has disabled parallelism, or there's
  # only one miss to dispatch.
  use_future <- length(miss_idx) > 1L &&
                !isTRUE(getOption("seqNdisplayR.disable_parallel_bw", FALSE)) &&
                requireNamespace("future",       quietly = TRUE) &&
                requireNamespace("future.apply", quietly = TRUE) &&
                isTRUE(.ensure_future_plan(length(miss_idx)))

  if (use_future) {
    # Run the same low-level fetch in each worker. Globals (chrom, start,
    # end) are auto-detected by future; bwimport must be loaded inside
    # each worker, hence `future.packages`.
    .ch <- chrom; .cs <- chrom_start; .ce <- chrom_end
    fetched <- future.apply::future_lapply(
      miss_files,
      function(f) {
        .data <- tryCatch(
          bwimport::bw_import(f, .ch, .cs, .ce),
          error   = function(e) NULL,
          warning = function(w) NULL
        )
        if (is.null(.data)) {
          .data <- tryCatch(
            bwimport::bw_import(f, sub("^chr", "", .ch), .cs, .ce),
            error   = function(e) NULL,
            warning = function(w) NULL
          )
        }
        .data
      },
      future.globals  = list(.ch = .ch, .cs = .cs, .ce = .ce),
      future.packages = "bwimport",
      future.seed     = NULL
    )
  } else {
    fetched <- lapply(miss_files, function(f)
      .fetch_bw_raw(f, chrom, chrom_start, chrom_end))
  }

  # Step 3: populate parent cache + result list.
  for (j in seq_along(miss_idx)) {
    i <- miss_idx[j]
    val <- fetched[[j]]
    if (!is.null(val) && !inherits(val, "try-error") &&
        is.numeric(val) && length(val) == region_length) {
      key <- .bw_cache_key(as.character(bw_files[[i]]), chrom, chrom_start, chrom_end)
      assign(key, val, envir = .bw_cache)
      results[[i]] <- val
    } else {
      results[[i]] <- rep(0, region_length)
    }
  }

  results
}


#' Log2-Transform a Signal Matrix
#'
#' @description Internal function:
#' Sign-aware log2 transformation with automatic pseudocount adjustment.
#' Used for both explicit log2 transform and temporary transform during
#' batch correction.
#'
#' @keywords internal
#'
#' @param bw_matrix numeric matrix (rows = positions, cols = samples)
#' @param pseudo_count pseudocount to add before log2
#' @param seqtype dataset name (for warning messages)
#' @param reason character string describing why transform is applied (for warning messages)
#' @param verbosity verbosity level
#'
#' @return list with elements: matrix (transformed), signs (sign matrix), pseudo_count (possibly adjusted)
#'
Log2TransformMatrix = function(bw_matrix, pseudo_count, seqtype, reason = "log2-transformation", verbosity) {
  .signs <- sign(bw_matrix)
  if (any(.signs <= 0) | pseudo_count < 1) {
    .abs <- abs(bw_matrix) + pseudo_count
    if (any(.abs < 1)) {
      .adj <- 1 - min(abs(bw_matrix))
      if (verbosity > 1) {
        cat('WARNING(s):', '\n')
        cat(paste0('\t', '.) ', 'automatically adjusting pseudocount from ', pseudo_count, ' to ', .adj,
                   ' for ', seqtype, ' to allow ', reason,
                   ', because the data contain values equal to or less than zero'), '\n')
      }
      pseudo_count <- .adj
    }
  }
  .transformed <- .signs * log2(abs(bw_matrix) + pseudo_count)
  list(matrix = .transformed, signs = .signs, pseudo_count = pseudo_count)
}


#' Batch-Correct a Signal Matrix
#'
#' @description Internal function:
#' Apply limma::removeBatchEffect to a signal matrix. If the data
#' is not already log2-transformed, temporarily transforms it for
#' correction and reverses afterwards.
#'
#' @keywords internal
#'
#' @param bw_matrix numeric matrix (rows = positions, cols = samples)
#' @param batch batch assignments (vector, list, or NULL for auto-detect from rep numbers)
#' @param log2_transform logical --  is the matrix already log2-transformed?
#' @param pseudo_count pseudocount for temporary log2 transform
#' @param seqtype dataset name (for warning messages)
#' @param verbosity verbosity level
#'
#' @return corrected matrix
#'
#' @importFrom limma removeBatchEffect
#'
BatchCorrectMatrix = function(bw_matrix, batch, log2_transform, pseudo_count, seqtype, verbosity) {
  if (is.null(batch)) {
    batch <- as.integer(sapply(colnames(bw_matrix), function(rep) rev(strsplit(rep, split='.rep', fixed=TRUE)[[1]])[1]))
  } else if (is.list(batch)) {
    batch <- ListFlatten(batch)
    batch <- structure(as.vector(unlist(batch)),
                       names = paste0(rep(names(batch), lengths(batch)), '.rep',
                                      as.character(unlist(sapply(lengths(batch), function(x) 1:x)))))
  }
  if (is.null(names(batch))) names(batch) <- colnames(bw_matrix)
  batch <- batch[colnames(bw_matrix)]
  .batches <- unique(batch)
  if (length(.batches) <= 1) {
    if (verbosity > 1) {
      cat('WARNING(s):', '\n')
      cat(paste0('\t', '.) ', 'batch correction is requested for ', seqtype, ' but no batch information is provided'), '\n')
    }
    return(bw_matrix)
  }
  # Temporarily log2-transform if not already
  .signs <- NULL
  if (!log2_transform) {
    .log2 <- Log2TransformMatrix(bw_matrix, pseudo_count, seqtype, reason = "batch correction", verbosity)
    bw_matrix <- .log2$matrix
    .signs <- .log2$signs
    pseudo_count <- .log2$pseudo_count
  }
  .group <- factor(sub("\\.rep\\d+$", "", names(batch)))
  bw_matrix <- limma::removeBatchEffect(bw_matrix, batch = batch, group = .group)
  # Reverse log2 if it was temporary
  if (!log2_transform) {
    bw_matrix <- .signs * (2^abs(bw_matrix) - pseudo_count)
  }
  return(bw_matrix)
}


#' Build Named Track List From Matrix
#'
#' @description Internal function:
#' Convert a signal matrix into a named list of tracks, optionally
#' averaging across replicates and zeroing negative values.
#'
#' @keywords internal
#'
#' @param bw_matrix numeric matrix (rows = positions, cols = sample.repN)
#' @param calc_mean logical --  average replicates into per-sample tracks?
#' @param neg_vals_set_0 logical --  set negative values to zero?
#'
#' @return named list of numeric vectors
#'
BuildTrackList = function(bw_matrix, calc_mean, neg_vals_set_0) {
  track.list <- list()
  if (calc_mean) {
    .sample.names <- unique(sapply(colnames(bw_matrix), function(x) strsplit(x, split='.rep', fixed=TRUE)[[1]][1]))
    for (.sample.name in .sample.names) {
      .all.rep.names <- sapply(colnames(bw_matrix), function(x) strsplit(x, split='.rep', fixed=TRUE)[[1]][1])
      .rep.cols <- names(.all.rep.names[.all.rep.names == .sample.name])
      .mean.track <- rowMeans(bw_matrix[, .rep.cols, drop = FALSE])
      if (neg_vals_set_0) .mean.track[.mean.track < 0] <- 0
      track.list[[.sample.name]] <- .mean.track
    }
  } else {
    if (neg_vals_set_0) bw_matrix[bw_matrix < 0] <- 0
    for (.col in colnames(bw_matrix)) {
      track.list[[.col]] <- bw_matrix[, .col]
    }
  }
  track.list
}


#' Load And Transform Data For Track (new as of 25-10-09)
#'
#' @description Internal function for extracting BigWig signal
#'   from a given genomic region for multiple samples, with options
#'   for log2-transformation, batch correction, negative-value handling,
#'   and mean computation across replicates.
#'
#' @param seqtype Character; type of sequence (e.g., "ChIP", "ATAC")
#' @param plotted_region GRanges object specifying the region to extract
#' @param samples List of sample metadata
#' @param bigwigs Nested list of BigWig file paths organized by strand
#' @param bigwig_dirs List of directories containing BigWig files
#' @param parameters Optional list of parameters per seqtype (log2 transform, pseudo count, batch info, etc.)
#' @param get_subsamples Logical; if TRUE, returns only the subset of sample names
#' @param print_order Logical; if TRUE, prints the order of samples
#' @param verbosity Integer; level of verbosity for warnings/messages (default: 0)
#'
#' @return A list of numeric vectors representing the signal for each sample,
#'   with optional mean computation across replicates
#'
#' @import IRanges
#' @import GenomicRanges
#' @importFrom limma removeBatchEffect
#' @importFrom BiocGenerics strand
#' @importFrom GenomeInfoDb seqnames
#'
#' @keywords internal
LoadAndTransformDataForTrack <- function(seqtype, plotted_region, samples, bigwigs, bigwig_dirs, parameters=NULL, get_subsamples=FALSE, print_order=FALSE, verbosity=0) {
  .chrom = as.character(GenomeInfoDb::seqnames(plotted_region))
  .strand = as.character(BiocGenerics::strand(plotted_region))
  .chrom.start = IRanges::start(plotted_region)
  .chrom.end = IRanges::end(plotted_region)
  .dataset.present = TRUE
  
  if (seqtype %in% names(bigwigs[[.strand]])){
    .bw.list = list()
    
    # Set default parameters if not provided
    if (is.null(parameters)){ 
      .which.samples = NULL
      .which.reps = NULL
      .log2.transform = FALSE
      .pseudo.count = 1
      .batch.correct = FALSE
      .batch = NULL
      .neg.valued.bw = FALSE
      .calc.mean = FALSE
      .neg.vals.set.0 = FALSE
    } else {
      .param = parameters[[seqtype]]
      .which.samples = .param[['whichSamples']]
      .which.reps = .param[['whichReps']]
      .log2.transform = .param[['log2transform']]
      .pseudo.count = .param[['pseudoCount']]
      .batch.correct = .param[['batchCorrect']]
      .batch = .param[['batch']]
      .neg.valued.bw = .param[['negative_valued_bw']]
      .calc.mean = .param[['calcMean']]
      .neg.vals.set.0 = .param[['negValsSet0']]
      
      if (!is.null(.which.samples)){
        if (is.list(.which.samples)){
          .unlisted.which.samples = ListFlatten(.which.samples)
          .unlisted.which.samples = paste(rep(names(.unlisted.which.samples), lengths(.unlisted.which.samples)), as.character(unlist(.unlisted.which.samples)), sep='.')
        } else if (any(is.na(.which.samples))){
          .unlisted.which.samples = .which.samples
          .dataset.present = FALSE
        } else {
          .unlisted.which.samples = .which.samples
        }
        if (any(grepl('.rep\\d$', .unlisted.which.samples))){
          .calc.mean = FALSE
        }
      }
    }
    
    if (.dataset.present){
      .bw.files = UnpackSamples(seqtype, samples, .which.samples, .which.reps, bigwigs[[.strand]], bigwig_dirs[[.strand]], verbosity)
      .sample.rep.names = names(.bw.files)
      
      if (any(is.na(.sample.rep.names))){
        if (verbosity > 0){
          cat('ERROR(s):', '\n')
          cat(' - discrepancy between requested and stored sample names (or numbers)', '\n')
          cat(paste('\t', '.)', 'stored sample names:', paste(.sample.rep.names, collapse=', '), length(.sample.rep.names), sep='\t'), '\n')
          cat(paste('\t', '\t', seqtype), '\n')
        }
        return(NULL)
      }
      
      if (get_subsamples | print_order){
        if (.calc.mean){
          sample.names = unique(sapply(.sample.rep.names, function(.sample.rep.name) strsplit(.sample.rep.name, split='.rep', fixed=TRUE)[[1]][1]))
        } else {
          sample.names = .sample.rep.names
        }
        if (print_order){
          cat(paste('printing order of samples for', seqtype), '\n')
          cat(paste(sample.names, collapse='\n'), '\n')
        }
        if (get_subsamples){
          return(sample.names)
        } else {
          cat('and exiting', '\n')
          return()
        }
      } else {
        # Batch-fetch BigWig data: cache-aware, parallel over uncached files
        # where the OS supports forking (Linux/macOS).  Falls back to
        # sequential on Windows or single-core boxes; controllable via
        # options(seqNdisplayR.disable_parallel_bw = TRUE).
        .bw.list = .fetch_bw_batch(
          bw_files    = .bw.files[.sample.rep.names],
          chrom       = .chrom,
          chrom_start = .chrom.start,
          chrom_end   = .chrom.end,
          verbosity   = verbosity
        )

        .bw.matrix = do.call(cbind, .bw.list)
        rownames(.bw.matrix) = .chrom.start:.chrom.end
        rm('.bw.list')

        # Flip sign for negative-valued bigwigs on minus strand
        if (.neg.valued.bw & .strand=='-'){
          .bw.matrix = -1*.bw.matrix
        }
        # Log2 transformation
        if (.log2.transform){
          .log2.result = Log2TransformMatrix(.bw.matrix, .pseudo.count, seqtype, "log2-transformation", verbosity)
          .bw.matrix = .log2.result$matrix
          .pseudo.count = .log2.result$pseudo_count
        }
        # Batch correction
        if (.batch.correct){
          .bw.matrix = BatchCorrectMatrix(.bw.matrix, .batch, .log2.transform, .pseudo.count, seqtype, verbosity)
        }
        # Build and return track list
        return(BuildTrackList(.bw.matrix, .calc.mean, .neg.vals.set.0))
      }
    } else {
      return()
    }
  } else {
    return()
  }
}


#' Load Tracks
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param plotted_region 
#' @param samples 
#' @param bigwigs 
#' @param bigwig_dirs 
#' @param parameters 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
LoadTracks = function(plotted_region, samples, bigwigs, bigwig_dirs, parameters, verbosity){
  .tracks = list()
  for (.seqtype in names(samples)){
    #cat(paste('loading', .seqtype, 'data'), '\n')
    .tracks[[.seqtype]] = LoadAndTransformDataForTrack(.seqtype, plotted_region, samples, bigwigs, bigwig_dirs, parameters, get_subsamples=FALSE, print_order=FALSE, verbosity)
  }
  return(.tracks)
}


