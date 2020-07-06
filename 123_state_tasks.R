do_state_tasks <- function(oldest_active_sites, ...) {

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
        sprintf("get_site_data(oldest_active_sites, I('%s'), parameter)", task_name)
    }
  )

  # create the task plan
  task_plan <- create_task_plan(
    task_names = state_codes,
    task_steps = list(download_step),
    add_complete = FALSE)

  create_task_makefile(
    task_plan,
    '123_state_tasks.yml',
    include = c('remake.yml'),
    packages = c('tidyverse', 'dataRetrieval'),
    sources = c('1_fetch/src/get_site_data.R'),
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

    # Return nothing to the parent remake file
  scmake(remake_file='123_state_tasks.yml')
  return()

}
