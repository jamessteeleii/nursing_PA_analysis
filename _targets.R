# _targets.R file
library(targets)
library(tarchetypes)
# library(future)
# library(future.callr)
# plan(callr)
source("R/functions.R")
tar_option_set(
  packages = c(
    library(tidyverse),
    library(scales),
    library(glue),
    library(patchwork),
    library(tidytext),
    library(stringi),
    # library(forcats),
    # library(igraph),
    library(tidygraph),
    library(grid),
    library(ggraph),
    library(hunspell),
    library(SemNetCleaner)
  )
)

list(
  tar_target(data, read_data()),
  
  # Make all plots
  tar_target(PA_importance, plot_PA_importance(data)),
  tar_target(guideline_confidence, plot_guideline_confidence(data)),
  tar_target(knowledge_dose_adult, plot_knowledge_dose_adult(data)),
  tar_target(knowledge_other_adult, plot_knowledge_other_adult(data)),
  tar_target(knowledge_older_adult, plot_knowledge_older_adult(data)),
  tar_target(knowledge_dose_child, plot_knowledge_dose_child(data)),
  tar_target(knowledge_other_child, plot_knowledge_other_child(data)),
  tar_target(training, plot_training(data)),
  tar_target(training_settings, plot_training_settings(data)),
  tar_target(knowledge_sources, plot_knowledge_sources(data)),
  tar_target(PA_levels, plot_PA_levels(data)),
  tar_target(example_mod_activities, plot_example_mod_activities(data)),
  tar_target(example_vig_activities, plot_example_vig_activities(data)),
  
  # Export all plots as tiffs
  tar_target(PA_importance_tiff, make_plot_tiff(PA_importance,
                                                width = 7.5,
                                                height = 5,
                                                path = "plots/PA_importance.tiff")),
  tar_target(guideline_confidence_tiff, make_plot_tiff(guideline_confidence,
                                                       width = 10,
                                                       height = 7.5,
                                                       path = "plots/guideline_confidence.tiff")),
  tar_target(knowledge_dose_adult_tiff, make_plot_tiff(knowledge_dose_adult,
                                                       width = 10,
                                                       height = 7.5,
                                                       path = "plots/knowledge_dose_adult.tiff")),
  tar_target(knowledge_other_adult_tiff, make_plot_tiff(knowledge_other_adult,
                                                        width = 10,
                                                        height = 5,
                                                        path = "plots/knowledge_other_adult.tiff")),
  tar_target(knowledge_older_adult_tiff, make_plot_tiff(knowledge_older_adult,
                                                        width = 10,
                                                        height = 5,
                                                        path = "plots/knowledge_older_adult.tiff")),
  tar_target(knowledge_dose_child_tiff, make_plot_tiff(knowledge_dose_child,
                                                       width = 10,
                                                       height = 5,
                                                       path = "plots/knowledge_dose_child.tiff")),
  tar_target(knowledge_other_child_tiff, make_plot_tiff(knowledge_other_child,
                                                        width = 10,
                                                        height = 5,
                                                        path = "plots/knowledge_other_child.tiff")),
  tar_target(training_tiff, make_plot_tiff(training,
                                           width = 7.5,
                                           height = 7.5,
                                           path = "plots/training.tiff")),
  tar_target(training_settings_tiff, make_plot_tiff(training_settings,
                                                    width = 10,
                                                    height = 5,
                                                    path = "plots/training_settings.tiff")),
  tar_target(knowledge_sources_tiff, make_plot_tiff(knowledge_sources,
                                                    width = 10,
                                                    height = 5,
                                                    path = "plots/knowledge_sources.tiff")),
  tar_target(PA_levels_tiff, make_plot_tiff(PA_levels,
                                            width = 10,
                                            height = 5,
                                            path = "plots/PA_levels.tiff")),
  tar_target(example_mod_activities_tiff, make_plot_tiff(example_mod_activities,
                                                         width = 12.5,
                                                         height = 7.5,
                                                         path = "plots/example_mod_activities.tiff")),
  tar_target(example_vig_activities_tiff, make_plot_tiff(example_vig_activities,
                                                         width = 12.5,
                                                         height = 7.5,
                                                         path = "plots/example_vig_activities.tiff"))
)