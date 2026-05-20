spca_test_matrices = function() {
  X_tall = scale(matrix(c(
    2.0,  1.0,  0.5,  0.0,
    1.8,  0.9,  0.4,  0.1,
   -1.9, -1.0, -0.5,  0.0,
   -2.1, -1.1, -0.6, -0.1,
    0.2,  1.5, -1.0,  0.5,
   -0.1,  1.4, -0.9,  0.6,
    0.0, -1.3,  1.1, -0.4,
    0.1, -1.4,  1.0, -0.5
  ), nrow = 8, byrow = TRUE), center = TRUE, scale = FALSE)

  X_fat = scale(matrix(c(
    2.0,  1.0,  0.5,  0.0,  1.8,  0.2,
    1.7,  0.8,  0.4,  0.1,  1.5,  0.3,
   -1.9, -1.0, -0.5,  0.0, -1.7, -0.2,
   -2.1, -1.1, -0.6, -0.1, -1.8, -0.3
  ), nrow = 4, byrow = TRUE), center = TRUE, scale = FALSE)

  S_tall = crossprod(X_tall)

  S_diag = matrix(c(
    4.0, 0.2, 0.1, 0.0,
    0.2, 2.0, 0.1, 0.0,
    0.1, 0.1, 1.0, 0.1,
    0.0, 0.0, 0.1, 0.5
  ), nrow = 4, byrow = TRUE)
  
  
  

  colnames(X_tall) = paste0("x", seq_len(ncol(X_tall)))
  colnames(X_fat) = paste0("x", seq_len(ncol(X_fat)))
  dimnames(S_tall) = list(colnames(X_tall), colnames(X_tall))
  dimnames(S_diag) = list(paste0("x", seq_len(ncol(S_diag))),
                           paste0("x", seq_len(ncol(S_diag))))

  list(
    X_tall = X_tall,
    S_tall = S_tall,
    X_fat = X_fat,
    S_diag = S_diag,
    DF_tall = as.data.frame(X_tall)
  )
}

make_load_mat = function() {
  L = matrix(c(0.3276875, 0.0, 0.3496920, 0.6110217, 0.0, 0.5153157, 0.0,
           0.3625446, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5084692, -0.5887437,
           -0.6283629), ncol = 2)
  dimnames(L) = list(paste0("v", seq_len(nrow(L))),
                             paste0("sPC", seq_len(ncol(L))))
  L
}

