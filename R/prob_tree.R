#' Probability Tree
#' Adapted from: https://daranzolin.github.io/2018-01-07-probability-trees
#'
#' @param pa Probability that A occurs
#' @param pba Probability that B occurs, given A occurred
#' @param pab Probability that B desn't occur, given that A didn't occur
#' @export
#'
  prob_tree <- function(pa, pba, pab) {

    if (!all(c(pa, pba, pab) > 0) && !all(c(pa, pba, pab) < 1)) {
      stop("Les probabilités doivent être comprises entre 0 et 1.",
           call. = FALSE)
    }
    c_pa <- 1 - pa
    c_tp <- 1 - pba
    c_tn <- 1 - pab

    round4 <- purrr::partial(round, digits = 4)

    b1 <- round4(pa * pba)
    b2 <- round4(pa * c_tp)
    b3 <- round4(c_pa * c_tn)
    b4 <- round4(c_pa * pab)

    bp <-  round4(b1/(b1 + b3))

    labs <- c("X", pa, c_pa, pba, c_tp, pab, c_tn, b1, b2, b4, b3)

    tree <-
      create_graph() %>%
      add_n_nodes(
        n = 11,
        type = "path",
        label = labs,
        node_aes = node_aes(
          shape = "circle",
          height = 1,
          width = 1,
          x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
          y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>%
      add_edge(
        from = 1,
        to = 2,
        edge_aes = edge_aes(
          label = "A(1)"
        )
      ) %>%
      add_edge(
        from = 1,
        to = 3,
        edge_aes = edge_aes(
          label = "A(0)"
        )
      ) %>%
      add_edge(
        from = 2,
        to = 4,
        edge_aes = edge_aes(
          label = "B(1)"
        )
      ) %>%
      add_edge(
        from = 2,
        to = 5,
        edge_aes = edge_aes(
          label = "B(0)"
        )
      ) %>%
      add_edge(
        from = 3,
        to = 7,
        edge_aes = edge_aes(
          label = "B(1)"
        )
      ) %>%
      add_edge(
        from = 3,
        to = 6,
        edge_aes = edge_aes(
          label = "B(0)"
        )
      ) %>%
      add_edge(
        from = 4,
        to = 8,
        edge_aes = edge_aes(
          label = "="
        )
      ) %>%
      add_edge(
        from = 5,
        to = 9,
        edge_aes = edge_aes(
          label = "="
        )
      ) %>%
      add_edge(
        from = 7,
        to = 11,
        edge_aes = edge_aes(
          label = "="
        )
      ) %>%
      add_edge(
        from = 6,
        to = 10,
        edge_aes = edge_aes(
          label = "="
        )
      )
    print(render_graph(tree))
    invisible(tree)
  }
