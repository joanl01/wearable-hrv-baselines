get_corr <- function(data) {
  data |>
    select(where(is.numeric)) |>
    rstatix::cor_test()
    # mutate(
    #   day = unique(data$days),
    #   time = unique(data$timeline)
    # )
}

# 
# get_corr <- function(df) {
#   df |>
#     select(m1:m4) |>
#     rstatix::cor_test() |>
#     mutate(
#       day = unique(df$day),
#       time = unique(df$time)
#     )
# }