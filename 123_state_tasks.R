do_state_tasks <- function(oldest_active_sites, ...) {
  split_inventory(sites_info=oldest_active_sites)

  # Define task table rows
  state_codes = oldest_active_sites$state_cd

  # Define task table columns
  download_step <- create_task_step(
    step_name = 'download',
    target_name = function(task_name, step_name, ...){
        sprintf('%s_data', task_name)
    },
    command = function(task_name, ...){
        sprintf("get_site_data('1_fetch/tmp/inventory_%s.tsv', parameter)", task_name)
    }
  )

  plot_step <- create_task_step(
    step_name = 'plot',
    target_name = function(task_name, ...){
        sprintf('3_visualize/out/timeseries_%s.png', task_name)
    },
    command = function(steps, ...){
        sprintf("plot_site_data(out_file = target_name, site_data = %s, parameter)", steps[['download']]$target_name)
    }
  )

  tally_step <- create_task_step(
    step_name = 'tally',
    target_name = function(task_name, ...){
        sprintf('%s_tally', task_name)
    },
    command = function(steps, ...){
        sprintf("tally_site_obs(site_data = %s)", steps[['download']]$target_name)
    }
  )


  # create the task plan
  task_plan <- create_task_plan(
    task_names = state_codes,
    task_steps = list(download_step, tally_step, plot_step),
    final_step = c('tally', 'plot'),
    add_complete = FALSE)

  create_task_makefile(
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = c('remake.yml'),
    packages = c('tidyverse', 'dataRetrieval', 'lubridate'),
    sources = c(...),
    as_promises = TRUE,
    finalize_funs = c('combine_obs_tallies', 'summarize_timeseries_plots'),
    final_targets = c('obs_tallies', '3_visualize/out/timeseries_plots.yml'),
    tickquote_combinee_objects = TRUE)

  # Build the tasks
  loop_tasks(task_plan = task_plan, task_makefile = '123_state_tasks.yml')
  obs_tallies <- remake::fetch('obs_tallies_promise', remake_file = '123_state_tasks.yml')

  timeseries_plots_info <- yaml::yaml.load_file('3_visualize/out/timeseries_plots.yml') %>%
  tibble::enframe(name = 'filename', value = 'hash') %>%
  mutate(hash = purrr::map_chr(hash, `[[`, 1))

  # return the combined tallies
  return(list(obs_tallies=obs_tallies, timeseries_plots_info=timeseries_plots_info))

}

write_individual_tmp_files <- function(
    sites_info){
    tmpdir = '1_fetch/tmp'
    if(!dir.exists(tmpdir)) dir.create(tmpdir)

    state_files <- c()
    for(i in 1:nrow(sites_info)){
        row <- sites_info[i, ]
        filename <- sprintf("inventory_%s.tsv", row$state_cd)
        file_path = file.path(tmpdir, filename)
        readr::write_tsv(row, file_path)
        # collect the file names
        state_files <- c(state_files, file_path)
    }
    state_files <- sort(state_files)
    return(state_files)
}


split_inventory <- function(
    summary_file='1_fetch/tmp/state_splits.yml',
    sites_info=oldest_active_sites){

    state_files = write_individual_tmp_files(sites_info)
    scipiper::sc_indicate(ind_file = summary_file, data_file = state_files)
}

combine_obs_tallies <- function(...){
    # filter to just those arguments that are tibbles (because the only step
    # outputs that are tibbles are the tallies)
    dots <- list(...)
    tally_dots <- dots[purrr::map_lgl(dots, is_tibble)]
    combined = bind_rows(tally_dots)
    return(combined)
}

summarize_timeseries_plots <- function(ind_file, ...){
    # filter to just those arguments that are character strings (because the only
    # step outputs that are characters are the plot filenames)
    dots <- list(...)
    plot_dots <- dots[purrr::map_lgl(dots, is.character)]
    do.call(combine_to_ind, c(list(ind_file), plot_dots))
}
