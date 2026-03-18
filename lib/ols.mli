(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ordinary least squares via QR decomposition. *)

val fit :
  predictors:float array array ->
  response:float array ->
  (float array, string) result
(** [fit ~predictors ~response] returns OLS coefficients. [predictors] is an n×p
    matrix (n samples, p predictors). Returns p coefficients. *)

val bootstrap_ci :
  trials:int ->
  predictors:float array array ->
  response:float array ->
  ((float * float) array, string) result
(** [bootstrap_ci ~trials ~predictors ~response] returns 95%% CI for each
    coefficient via bootstrap resampling. *)
