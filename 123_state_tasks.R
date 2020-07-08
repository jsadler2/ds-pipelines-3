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
    #command = 'get_site_data(oldest_active_sites, target_name, parameter)'
    command = function(task_name, ...){
        sprintf("get_site_data(I('1_fetch/tmp/inventory_%s.tsv'), parameter)", task_name)
    }
  )

  # create the task plan
  task_plan <- create_task_plan(
    task_names = state_codes,
    task_steps = list(download_step),
    add_complete = FALSE)

  create_task_makefile(
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = c('remake.yml'),
    packages = c('tidyverse', 'dataRetrieval'),
    sources = c('1_fetch/src/get_site_data.R'),
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

    # Return nothing to the parent remake file
  scmake(remake_file='123_state_tasks.yml')
  return()

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
